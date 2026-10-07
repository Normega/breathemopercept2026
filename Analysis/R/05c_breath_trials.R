# R/05c_breath_trials.R
# Per-trial breathing features for every paced BCAT trial (baseline and
# combined task), the choice of belt for each participant, and a
# per-participant summary used for belt QC.
#
# Writes to physioQCPath:
#   breath_trials.csv        one row per participant x task x trial, chosen belt
#   breath_participants.csv  one row per participant: belt choice, lag, polarity,
#                            entrainment, and respkit quality metrics
# and updates each <id>_physio.rds: `resp` becomes the chosen belt (NULL if
# none) and `breath_source` records which.
#
# Pacer onset is the trial's code-3 trigger (snapped from PsychoPy
# trial.started + the task clock offset); the expected cycle durations come
# from the pacer schedule in physio_functions.R; observed breaths are respkit's
# (trough -> peak -> trough), matched to pacer cycles by the participant's lag.
# Breathing change is measured on all four cycles up to the cycle-4 inhale
# peak, the last paced event (see breath_trial_features()).
#
# Belt choice. The belt-pacer synchrony (median over trials of the correlation
# between the belt and the circle waveform) tests whether a belt is on this
# participant's chest far more directly than amplitude or breath-count
# heuristics: a limp or unworn belt does not follow a 4-s pacer, and the
# partner's belt follows a different trial sequence. The other seat's belt
# replaces the own belt only if (a) no participant sat there, or that
# participant's own belt does not follow their pacer (sync < .2), and (b) it
# follows this participant's pacer (sync >= .4) and beats the own belt by >= .3.
# ---------------------------------------------------------------

message("05c | Breath trial features...")

load_respkit()
source(file.path(analysisPath, "R", "physio_functions.R"))

breathTrialsFile <- file.path(physioQCPath, "breath_trials.csv")
breathPartFile   <- file.path(physioQCPath, "breath_participants.csv")
SYNC_MIN   <- 0.4; SYNC_MARGIN <- 0.3; PARTNER_SYNC_MAX <- 0.2

# Features of every trial for one belt, at one polarity.
belt_trials <- function(rec, tr, polarity) {
  res <- suppressWarnings(suppressMessages(resp_analyse(rec, polarity = polarity)))
  bt  <- res$breaths
  dur <- attr(tr, "durations")
  cyc <- unlist(lapply(seq_len(nrow(tr)), function(k)
    tr$t0[k] + c(0, cumsum(dur[[k]]))[seq_len(PACER_NBREATH)]))
  lag <- pacer_lag(bt$t_start, cyc)
  if (!is.finite(lag)) lag <- 0.5
  amp_ref <- stats::median(bt$amplitude, na.rm = TRUE)
  feats <- do.call(rbind, lapply(seq_len(nrow(tr)), function(k)
    as.data.frame(breath_trial_features(bt, res$recording, tr$t0[k], dur[[k]], lag, amp_ref))))
  # Polarity only relabels peaks and troughs; the signal is not negated, so a
  # belt read as "down" correlates negatively with the circle when correct.
  if (polarity == "down") feats$pacer_r <- -feats$pacer_r
  feats$exp_d1 <- vapply(dur, `[`, numeric(1), 1)
  feats$exp_d4 <- vapply(dur, `[`, numeric(1), PACER_NBREATH)
  list(trials = cbind(tr, feats), lag = lag, quality = res$quality, polarity = polarity,
       sync = stats::median(feats$pacer_r, na.rm = TRUE))
}

# Analyse one belt, choosing polarity from the pacer: if the belt
# anti-correlates with the circle, it is inverted (respkit's own polarity
# diagnostic is uninformative under a symmetric pacer).
analyse_belt <- function(rec, tr) {
  if (is.null(rec)) return(NULL)
  up <- tryCatch(belt_trials(rec, tr, "up"), error = function(e) NULL)
  if (is.null(up)) return(NULL)
  raw_sync <- stats::median(up$trials$pacer_r, na.rm = TRUE)
  if (is.finite(raw_sync) && raw_sync < -0.2) {
    dn <- tryCatch(belt_trials(rec, tr, "down"), error = function(e) NULL)
    if (!is.null(dn)) return(dn)
  }
  up
}

files <- sort(list.files(physioRdsPath, "_physio[.]rds$", full.names = TRUE))
only  <- strsplit(Sys.getenv("PHYSIO_IDS", ""), ",")[[1]]   # optional subset, for testing
if (length(only)) {
  files <- files[sub("_physio[.]rds$", "", basename(files)) %in% only]
  breathTrialsFile <- sub("[.]csv$", "_subset.csv", breathTrialsFile)
  breathPartFile   <- sub("[.]csv$", "_subset.csv", breathPartFile)
}
message(sprintf("  %d participant physio files", length(files)))

# ── Pass A: own and alternative belt for everyone ─────────────
own <- list(); alt <- list(); meta <- list()
for (f in files) {
  px  <- readRDS(f)
  pid <- px$id
  tr  <- tryCatch(trial_schedule(px), error = function(e) {
    message("  [ERROR] ", pid, " schedule: ", conditionMessage(e)); NULL })
  meta[[pid]] <- list(file = f, occupied_by = px$alt_occupied_by,
                      has_alt = !is.null(px$resp_alt), has_trials = !is.null(tr))
  if (is.null(tr)) next
  # `resp_own` preserves the own-seat belt once 05c has written `resp`, so a
  # rerun compares the same two candidates.
  own_rec <- if (!is.null(px$resp_own)) px$resp_own else px$resp
  own[[pid]] <- analyse_belt(own_rec, tr)
  if (!is.null(px$resp_alt)) alt[[pid]] <- analyse_belt(px$resp_alt, tr)
}
sync_of <- function(x) if (is.null(x)) NA_real_ else x$sync

# ── Pass B: choose ─────────────────────────────────────────────
choice <- setNames(rep("own_seat", length(meta)), names(meta))
for (pid in names(meta)) {
  s_own <- sync_of(own[[pid]]); s_alt <- sync_of(alt[[pid]])
  partner <- meta[[pid]]$occupied_by
  partner_ok <- is.null(partner) || is.na(partner) ||
    !is.finite(sync_of(own[[partner]])) || sync_of(own[[partner]]) < PARTNER_SYNC_MAX
  if (partner_ok && is.finite(s_alt) && s_alt >= SYNC_MIN &&
      (!is.finite(s_own) || s_alt - s_own >= SYNC_MARGIN))
    choice[pid] <- "other_seat"
}
# A belt cannot be worn twice.
for (pid in names(choice)[choice == "other_seat"]) {
  partner <- meta[[pid]]$occupied_by
  if (!is.null(partner) && !is.na(partner) && partner %in% names(choice))
    choice[partner] <- "claimed_by_partner"
}

# ── Write ─────────────────────────────────────────────────────
all_trials <- list(); all_parts <- list()
for (pid in names(meta)) {
  ch  <- choice[[pid]]
  sel <- switch(ch, own_seat = own[[pid]], other_seat = alt[[pid]], claimed_by_partner = NULL)
  px  <- readRDS(meta[[pid]]$file)
  if (is.null(px$resp_own)) px$resp_own <- px$resp
  rec <- switch(ch, own_seat = px$resp_own, other_seat = px$resp_alt, claimed_by_partner = NULL)
  if (!is.null(rec) && ch == "other_seat")
    rec <- add_provenance(rec, "chosen over the own-seat belt: pacer sync %.2f vs %.2f",
                          sync_of(alt[[pid]]), sync_of(own[[pid]]))
  px$resp <- rec
  px$breath_source <- ch
  px$breath_polarity <- if (is.null(sel)) NA_character_ else sel$polarity
  px$breath_lag_s    <- if (is.null(sel)) NA_real_ else sel$lag
  saveRDS(px, meta[[pid]]$file)

  row <- data.frame(id = pid, breath_source = ch,
                    sync_own = sync_of(own[[pid]]), sync_alt = sync_of(alt[[pid]]),
                    alt_occupied_by = if (is.null(meta[[pid]]$occupied_by)) NA else meta[[pid]]$occupied_by,
                    stringsAsFactors = FALSE)
  if (!is.null(sel)) {
    tt <- sel$trials; tt$id <- pid; tt$breath_source <- ch
    all_trials[[pid]] <- tt
    chg <- tt$direction != 0 & is.finite(tt$obs_ratio) & is.finite(tt$exp_ratio)
    gain <- if (sum(chg) >= 6)
      unname(stats::coef(stats::lm(log(obs_ratio) ~ log(exp_ratio), data = tt[chg, ]))[2]) else NA_real_
    # Quality with the amplitude (IQR) tests off: the frequency, band-ratio,
    # flatline and saturation tests are scale-free; filtered_iqr is kept for
    # calibration against synchrony.
    qa <- tryCatch(resp_quality(rec, thresholds = list(iqr_unusable = NA, iqr_degraded = NA)),
                   error = function(e) NULL)
    qcol <- function(nm) if (!is.null(qa) && nm %in% names(qa)) qa[[nm]][1] else NA
    row <- cbind(row, data.frame(
      polarity = sel$polarity, lag_s = sel$lag, sync = sel$sync,
      n_trials = nrow(tt), n_trigger_onsets = sum(tt$onset_source == "trigger"),
      median_matched = stats::median(tt$n_matched),
      direction_compliance = mean(sign(log(tt$obs_ratio[chg])) == sign(log(tt$exp_ratio[chg]))),
      entrainment_gain = gain,
      median_dur_mae = stats::median(tt$dur_mae, na.rm = TRUE),
      quality_default = as.character(sel$quality$quality[1]),
      quality_no_iqr  = as.character(qcol("quality")),
      filtered_iqr = qcol("filtered_iqr"), band_ratio = qcol("band_ratio"),
      stringsAsFactors = FALSE))
  }
  all_parts[[pid]] <- row
}

trials <- dplyr::bind_rows(all_trials)
parts  <- dplyr::bind_rows(all_parts)
trials <- trials[, c("id", setdiff(names(trials), "id"))]
write.csv(trials, breathTrialsFile, row.names = FALSE)
write.csv(parts,  breathPartFile,   row.names = FALSE)
message(sprintf("  -> %s (%d trials), %s (%d participants)",
                basename(breathTrialsFile), nrow(trials), basename(breathPartFile), nrow(parts)))
message("  Belt choice:"); print(table(parts$breath_source, useNA = "ifany"))
message("05c | Breath trial features complete.")
