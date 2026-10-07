# R/physio_functions.R
# Helpers for the dual-seat BIOPAC recordings. Sourced by 05a/05b.
#
# ── Lab setup (verified 2026-10-05; see Methods/physio_seat_mapping.md) ───────
# Two participants run at once on the Left (L) and Right (R) computers, sharing
# one MP160. Filenames are L<leftID>.R<rightID>.acq (separator "." or "_"; the
# letters may be lower case; an all-zero ID means the seat was empty).
#
#   Seat   PsychoPy port      Trigger bits         Breath     Heart
#   L      0xCFF8             high nibble (4-7)    Breath 2   Heart 2
#   R      0xD030             low nibble (0-3)     Breath 1   Heart 1
#
# The pre-2026-10-05 version of 05_prep_physio.R had this mapping reversed for
# all three signal types, so every <pid>_*.rds it wrote held the seat partner's
# data. The mapping above is the hardware default only: 05b re-checks every
# session by matching trigger timing against each participant's PsychoPy log.
#
# Trigger codes (after nibble normalisation, identical for both computers):
#   1 task start  2 task complete  3 BCAT trial on  4 BCAT trial off
#   7 heartbeat interval start  8 heartbeat interval stop
#   9 GERT task start  10 GERT task end  11 GERT clip on  12 GERT clip off
#
# On many sessions the trigger line idles at 240 (high nibble stuck at 15),
# which erases every Left-seat code while leaving the Right seat intact.
# ───────────────────────────────────────────────────────────────────────────────

SEAT_MAP <- list(
  L = list(trigger_mask = 0xF0, breath = "Breath 2", heart = "Heart 2"),
  R = list(trigger_mask = 0x0F, breath = "Breath 1", heart = "Heart 1")
)
TRIGGER_CHANNEL     <- "Experiment Triggers"
TRIGGER_MIN_PULSE_S <- 0.02   # port settle artefacts last a few ms; real pulses ~1 s

load_respkit <- function(path = RESPKIT_PATH) {
  if (!dir.exists(path)) stop("respkit not found at ", path, call. = FALSE)
  for (f in list.files(file.path(path, "R"), "[.]R$", full.names = TRUE))
    source(f, local = globalenv())
  # >= 0.2.0 has multi-channel reads, trigger masks and resp_acq_start_time().
  ver <- read.dcf(file.path(path, "DESCRIPTION"), fields = "Version")[1, 1]
  if (utils::compareVersion(ver, "0.2.0") < 0)
    stop("respkit ", ver, " found; this pipeline needs >= 0.2.0", call. = FALSE)
  assign("RESPKIT_VERSION", ver, envir = globalenv())
  invisible(TRUE)
}

# Parse "L11111.R22222.acq", "l33333.R44444.acq", "L55555..R66666.acq", or the
# R-first order. Returns character IDs, NA for an empty (all-zero) seat.
parse_acq_filename <- function(fname) {
  stem <- sub("\\.[^.]+$", "", basename(gsub("\\\\", "/", fname)))
  pat_lr <- "^[Ll][._]*(\\d+)[._]+[Rr][._]*(\\d+)"
  pat_rl <- "^[Rr][._]*(\\d+)[._]+[Ll][._]*(\\d+)"
  if (grepl(pat_lr, stem, perl = TRUE)) {
    L_raw <- sub(paste0(pat_lr, ".*$"), "\\1", stem, perl = TRUE)
    R_raw <- sub(paste0(pat_lr, ".*$"), "\\2", stem, perl = TRUE)
  } else if (grepl(pat_rl, stem, perl = TRUE)) {
    R_raw <- sub(paste0(pat_rl, ".*$"), "\\1", stem, perl = TRUE)
    L_raw <- sub(paste0(pat_rl, ".*$"), "\\2", stem, perl = TRUE)
  } else {
    return(list(L = NA_character_, R = NA_character_, parsed = FALSE))
  }
  to_id <- function(x) if (as.numeric(x) == 0) NA_character_ else x
  list(L = to_id(L_raw), R = to_id(R_raw), parsed = TRUE)
}

# Events for one seat's field of the shared trigger line, plus what 05b needs to
# know about the field: its idle level and whether it was dead (held at 15,
# i.e. every bit high, so no code could be sent). respkit does the masking,
# idle subtraction and pulse-length floor; see its README, "Several channels,
# shared trigger lines, wall-clock start".
seat_trigger_events <- function(trig, fs, mask, min_pulse_s = TRIGGER_MIN_PULSE_S) {
  field <- mask_trigger_codes(trig, mask)
  tab   <- tabulate(as.integer(field[is.finite(field)]) + 1L, nbins = 16L)
  idle  <- which.max(tab) - 1L
  ev <- suppressMessages(trigger_events(trig, fs, trigger_mask = mask,
                                        trigger_idle = "auto",
                                        min_pulse_s = min_pulse_s))
  list(events = if (is.null(ev)) data.frame(time = numeric(0), code = integer(0)) else
                  data.frame(time = ev$time, code = as.integer(ev$value)),
       idle = idle, idle_share = max(tab) / sum(tab),
       stuck = idle == 15L)
}

# Read one .acq once and write a seat-level extract for both seats, whatever the
# filename says about occupancy. Identity is assigned later (05b).
extract_acq <- function(path, out_dir, resp_fs = PHYSIO_RESP_HZ,
                        card_fs = PHYSIO_CARD_HZ, overwrite = FALSE) {
  fname <- basename(path)
  stem  <- sub("\\.acq$", "", fname, ignore.case = TRUE)
  outs  <- file.path(out_dir, paste0(stem, "__", c("L", "R"), ".rds"))
  if (!overwrite && all(file.exists(outs))) return(NULL)
  ids <- parse_acq_filename(fname)

  # Empty template files (~159 KB) define channels but hold no samples; the
  # header says so without reading any signal.
  hdr <- resp_acq_channels(path)
  n_samp <- hdr$n[match(TRIGGER_CHANNEL, hdr$name)]
  if (is.na(n_samp) || n_samp < 10 * max(hdr$fs, na.rm = TRUE))
    return(data.frame(source_file = fname, seat = c("L", "R"),
                      filename_id = vapply(c("L", "R"), function(s)
                        ifelse(is.na(ids[[s]]), "", ids[[s]]), ""),
                      error = sprintf("no recorded data (%s samples)", format(n_samp)),
                      stringsAsFactors = FALSE))

  # One pass, five channels (of 17).
  chans <- c(SEAT_MAP$L$breath, SEAT_MAP$R$breath,
             SEAT_MAP$L$heart,  SEAT_MAP$R$heart, TRIGGER_CHANNEL)
  recs <- resp_read_acq(path, channel = chans)
  fs   <- recs[[1]]$fs
  trig <- recs[[TRIGGER_CHANNEL]]$signal
  start_time <- recs[[1]]$meta$start_time
  raw_int  <- as.integer(round(trig[is.finite(trig)]))
  raw_tab  <- tabulate(pmin(pmax(raw_int, 0L), 255L) + 1L, nbins = 256L)
  raw_idle <- list(value = which.max(raw_tab) - 1L, share = max(raw_tab) / length(raw_int))
  rm(raw_int)

  rows <- list()
  for (seat in c("L", "R")) {
    map <- SEAT_MAP[[seat]]
    trg <- seat_trigger_events(trig, fs, map$trigger_mask)

    resp <- recs[[map$breath]]
    resp$id <- ids[[seat]]
    resp$events <- if (nrow(trg$events))
      data.frame(time = trg$events$time, label = as.character(trg$events$code),
                 value = trg$events$code, stringsAsFactors = FALSE) else NULL
    resp$meta$seat <- seat
    resp <- add_provenance(resp, "seat %s: trigger bits 0x%X, idle %d, %d event(s)%s",
                           seat, map$trigger_mask, trg$idle, nrow(trg$events),
                           if (trg$stuck) " (field held high: no codes)" else "")
    resp <- resp_downsample(resp, resp_fs)
    hr   <- recs[[map$heart]]$signal
    card <- resp_decimate(hr, q = round(fs / card_fs), fs = fs)
    sd_native <- c(breath = sd(recs[[map$breath]]$signal), heart = sd(hr))
    recs[[map$breath]] <- NULL; recs[[map$heart]] <- NULL   # free ~120 MB per seat

    ext <- list(
      schema          = "aya_physio_extract_v2",
      respkit_version = RESPKIT_VERSION,
      source_file     = fname,
      seat            = seat,
      filename_id     = ids[[seat]],
      acq_start_utc   = start_time,
      fs_native       = fs,
      duration_s      = length(hr) / fs,
      trigger         = trg,
      raw_idle        = raw_idle,
      resp            = resp,
      card            = list(signal = card$signal, fs = card$fs, units = "mV",
                             channel = map$heart),
      channels        = map,
      sd_native       = sd_native
    )
    saveRDS(ext, outs[seat == c("L", "R")])

    ev <- trg$events
    rows[[seat]] <- data.frame(
      source_file  = fname, seat = seat,
      filename_id  = ifelse(is.na(ids[[seat]]), "", ids[[seat]]),
      filename_parsed = ids$parsed,
      acq_start_utc = format(start_time, "%Y-%m-%d %H:%M:%OS3", tz = "UTC"),
      duration_min = round(length(hr) / fs / 60, 2),
      raw_idle     = raw_idle$value, raw_idle_share = round(raw_idle$share, 3),
      trigger_mask = sprintf("0x%02X", map$trigger_mask),
      field_idle   = trg$idle, field_stuck = trg$stuck,
      n_events     = nrow(ev),
      n_bcat_on    = sum(ev$code == 3L), n_gert_on = sum(ev$code == 11L),
      n_hbd_on     = sum(ev$code == 7L),
      codes        = paste(sort(unique(ev$code)), collapse = ","),
      breath_channel = map$breath, heart_channel = map$heart,
      breath_sd    = signif(ext$sd_native[["breath"]], 4),
      heart_sd     = signif(ext$sd_native[["heart"]], 4),
      resp_fs      = resp$fs, card_fs = card$fs,
      stringsAsFactors = FALSE)
  }
  do.call(rbind, rows)
}

# ── Identity and clock alignment (used by 05b) ────────────────────────────────

# PsychoPy `expStart` ("2025-11-05 11h23.33.367262 -0500") -> POSIXct UTC.
# It is stamped when the experiment clock starts, so it puts a script's
# clock on wall time to within ~0.1 s.
parse_exp_start <- function(x) {
  x <- sub("^(\\d{4}-\\d{2}-\\d{2}) (\\d{2})h(\\d{2})\\.(\\d{2}\\.\\d+) ", "\\1 \\2:\\3:\\4 ", x)
  as.POSIXct(x, format = "%Y-%m-%d %H:%M:%OS %z", tz = "UTC")
}

# BCAT trial onsets from every PsychoPy file that has them. One row per trial,
# with the file's own clock (seconds since that script started).
read_task_bcat_onsets <- function(task_dir) {
  files <- list.files(task_dir, pattern = "_(CombinedTask|Intero2025)_.*[.]csv$")
  # A file named with an e-mail address must not reach any output (its ID is
  # corrected in id_corrections.csv for the behavioural data).
  files <- files[!grepl("@", files, fixed = TRUE)]
  out <- lapply(files, function(f) {
    d <- tryCatch(read.csv(file.path(task_dir, f), stringsAsFactors = FALSE),
                  error = function(e) NULL)
    if (is.null(d) || !"trial.started" %in% names(d)) return(NULL)
    task <- if (grepl("_CombinedTask_", f)) "combined" else "bcat_baseline"
    keep <- if (task == "combined") !is.na(d$level) else
      if ("trials.thisN" %in% names(d)) !is.na(d$trials.thisN) else FALSE
    keep <- keep & !is.na(suppressWarnings(as.numeric(d$trial.started)))
    if (!any(keep)) return(NULL)
    d <- d[keep, ]
    data.frame(task_file = f, task = task,
               id   = sub("^([^_]+)_.*$", "\\1", f),
               date = as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})_.*", "\\1", f)),
               trial = seq_len(nrow(d)),
               onset = as.numeric(d$trial.started),
               offset = if ("trial.stopped" %in% names(d))
                 suppressWarnings(as.numeric(d$trial.stopped)) else NA_real_,
               direction = if ("Direction" %in% names(d)) d$Direction else NA,
               exp_start = if ("expStart" %in% names(d))
                 parse_exp_start(d$expStart[1]) else as.POSIXct(NA, tz = "UTC"),
               stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(out)
}

# Best constant clock offset mapping task onsets onto event times
# (acq_time = task_time + offset). Candidate offsets come from pairing every
# event with each of the first few onsets; each is scored by how many onsets
# land within `tol` seconds of an event.
match_clock <- function(ev, onsets, tol = 0.1, n_anchor = 3L) {
  ev <- sort(ev); onsets <- sort(onsets)
  none <- list(offset = NA_real_, n_matched = 0L, frac = 0, med_err = NA_real_)
  if (length(ev) < 2L || length(onsets) < 2L) return(none)
  cand <- as.vector(outer(ev, onsets[seq_len(min(n_anchor, length(onsets)))], "-"))
  score <- vapply(cand, function(o) {
    p <- onsets + o
    j <- findInterval(p, ev, all.inside = TRUE)
    d <- pmin(abs(p - ev[j]), abs(p - ev[pmin(j + 1L, length(ev))]))
    sum(d <= tol)
  }, numeric(1))
  o <- cand[which.max(score)]
  p <- onsets + o
  j <- findInterval(p, ev, all.inside = TRUE)
  d <- pmin(abs(p - ev[j]), abs(p - ev[pmin(j + 1L, length(ev))]))
  ok <- d <= tol
  # refine: offset that minimises error over matched onsets
  if (any(ok)) {
    nearest <- ifelse(abs(p - ev[j]) <= abs(p - ev[pmin(j + 1L, length(ev))]),
                      ev[j], ev[pmin(j + 1L, length(ev))])
    o <- o + stats::median(nearest[ok] - p[ok])
    d <- abs(onsets + o - nearest)
  }
  list(offset = o, n_matched = sum(ok), frac = mean(ok),
       med_err = if (any(ok)) stats::median(d[ok]) else NA_real_)
}


# ── Channel resolution (used by 05b) ──────────────────────────────────────────

# Quick ECG usability score for choosing between channels, not for analysis.
# Band-pass 5-15 Hz (QRS band) and rectify; candidate beats are local maxima
# above 30% of the 99th percentile, thinned largest-first to >= 0.4 s apart
# (respkit enforce_min_distance), so an R wave always beats the T wave that
# follows it. Score = share of RR intervals in 0.4-1.5 s (40-150 bpm) times the
# share whose change from the previous interval is under 0.2 s, on up to three
# 2-minute windows (median); a window whose beats do not stand >= `min_ratio`
# times above the baseline scores 0. A lead below `min_sd` mV is unworn whatever its
# regularity: an unconnected input picks up a faint, perfectly regular copy of
# the other lead's heartbeat through crosstalk.
ecg_quality <- function(x, fs, win_s = 120, min_sd = 0.01, min_ratio = 6) {
  n <- length(x)
  s_all <- stats::sd(x, na.rm = TRUE)
  if (n < fs * 30 || !is.finite(s_all) || s_all < min_sd) return(0)
  bf <- signal::butter(2, c(5, 15) / (fs / 2), type = "pass")
  starts <- unique(round(seq(1, max(1, n - win_s * fs), length.out = 3)))
  scores <- vapply(starts, function(s0) {
    seg <- x[s0:min(n, s0 + win_s * fs - 1)]
    seg[!is.finite(seg)] <- 0
    y  <- abs(as.numeric(signal::filtfilt(bf, seg - stats::median(seg))))
    pk <- which(diff(sign(diff(y))) == -2) + 1
    pk <- pk[y[pk] > 0.3 * stats::quantile(y, 0.99)]
    if (length(pk) < 10) return(0)
    pk <- pk[enforce_min_distance(pk, y[pk], 0.4 * fs)]
    # Morphology: R waves stand far above the band-passed baseline (12-28x on
    # worn leads here); noise or mains oscillation picked at a forced spacing
    # does not (2-3x). Below `min_ratio` the window is not an ECG.
    if (stats::median(y[pk]) / stats::median(y) < min_ratio) return(0)
    rr <- diff(pk) / fs
    if (length(rr) < 10) return(0)
    mean(rr >= 0.4 & rr <= 1.5) * mean(abs(diff(rr)) < 0.2)
  }, numeric(1))
  stats::median(scores)
}

# ── Paced-breathing trial features (used by 05c) ──────────────────────────────
#
# The BCAT pacer (Task/*_lastrun.py, routine "trial"): a circle grows over the
# first half of each cycle (inhale) and shrinks over the second (exhale). A
# trial is numBreaths = 4 cycles starting at resetCycleDuration = 4 s. With
# changeVal c = 1 + direction * level (direction -1 accelerate, +1 decelerate,
# 0 no change; level = the participant's threshold, or the QUEST level at
# baseline):
#   high salience (changeSalience 1; all baseline trials): 4, 4, 4c, 4c s
#     (the cycle is multiplied by c once breath midPoint = 2 has ended)
#   low salience (changeSalience 0): 4, 4a, 4a^2, 4a^3 s with a = c^(1/3)
#     (the same total change, as a geometric ramp)
PACER_BASE_S  <- 4
PACER_NBREATH <- 4L
PACER_MID     <- 2L

pacer_durations <- function(level, direction, high_salience) {
  c_val <- 1 + direction * level
  if (!is.finite(c_val) || c_val <= 0) return(rep(NA_real_, PACER_NBREATH))
  if (high_salience) {
    PACER_BASE_S * c(rep(1, PACER_MID), rep(c_val, PACER_NBREATH - PACER_MID))
  } else {
    PACER_BASE_S * c_val^((seq_len(PACER_NBREATH) - 1) / (PACER_NBREATH - 1))
  }
}

# Circle-size waveform of one trial, sampled at times `t` (s from pacer onset):
# a triangle rising from 0 to 1 over each inhale and back to 0 over the exhale.
pacer_waveform <- function(t, durations) {
  starts <- c(0, cumsum(durations))
  k <- findInterval(t, starts, rightmost.closed = TRUE)
  out <- rep(NA_real_, length(t))
  ok <- k >= 1 & k <= length(durations)
  ph <- (t[ok] - starts[k[ok]]) / durations[k[ok]]
  out[ok] <- ifelse(ph <= 0.5, 2 * ph, 2 * (1 - ph))
  out
}

# Respiratory lag of one participant: median delay from pacer cycle onset to
# the nearest observed inhale onset (breath start), over all paced cycles,
# searched in [-1, 2.5] s.
pacer_lag <- function(bt_start, cycle_starts, lo = -1, hi = 2.5) {
  d <- vapply(cycle_starts, function(s) {
    x <- bt_start - s
    x <- x[x >= lo & x <= hi]
    if (length(x)) x[which.min(abs(x - 0.5))] else NA_real_
  }, numeric(1))
  stats::median(d, na.rm = TRUE)
}

# Features of one paced trial.
#   bt     respkit breath table for the participant (acq clock)
#   rec    the detection-copy recording (for the waveform correlation)
#   t0     pacer onset on the acq clock (the trial's code-3 trigger)
#   d      expected cycle durations from pacer_durations()
#   lag    the participant's respiratory lag
breath_trial_features <- function(bt, rec, t0, d, lag, amp_ref) {
  na <- list(n_matched = 0L, obs_d1 = NA, obs_d2 = NA, obs_d3 = NA, obs_d4 = NA,
             obs_d4_trough = NA_real_, obs_ratio = NA_real_, exp_ratio = NA_real_,
             obs_ratio3 = NA_real_, exp_ratio3 = NA_real_, dur_mae = NA_real_,
             pacer_r = NA_real_, amp_rel = NA_real_, amp_ratio = NA_real_,
             obs_rate_bpm = NA_real_)
  if (any(!is.finite(d)) || is.null(bt) || !nrow(bt)) return(na)
  starts <- t0 + c(0, cumsum(d))[seq_along(d)] + lag
  # Each pacer cycle takes the breath starting nearest its expected start,
  # within 40% of the cycle; a breath is used once at most.
  used <- integer(0)
  idx <- vapply(seq_along(d), function(k) {
    dist <- abs(bt$t_start - starts[k])
    dist[used] <- Inf
    j <- which.min(dist)
    if (length(j) && dist[j] <= 0.4 * d[k]) { used <<- c(used, j); j } else NA_integer_
  }, integer(1))
  D  <- bt$duration[idx]
  A  <- bt$amplitude[idx]
  on <- bt$t_start[idx]
  pk <- bt$t_peak[idx]
  out <- na
  out$n_matched <- sum(!is.na(idx))
  # Every pacer event is paced except the last: the closing trough of cycle 4
  # falls after the pacer stops, where the exhale slows and tails off into the
  # pause before the rating screen, and lands ~0.5 s late in every condition
  # (no-change median ~4.5 s against a 4.00 s pacer; 3.92-3.96 s for cycles
  # 1-3). Cycle 4 is therefore measured up to its inhale peak, the last paced
  # event, and its trough-to-trough duration is kept only as obs_d4_trough.
  # obs_d4 is the peak-to-peak interval from breath 3 to breath 4.
  last <- length(d); mid <- PACER_MID
  out$obs_d4_trough <- D[last]
  D[last] <- pk[last] - pk[last - 1L]
  for (k in seq_along(d)) out[[paste0("obs_d", k)]] <- D[k]
  # Change. Mean cycle length after the change point against before it, with
  # "after" = cycle-3 onset to cycle-4 peak (1.5 cycles) and "before" = cycle-1
  # onset to cycle-3 onset (2 cycles; cycle 2 alone if breath 1 is unmatched).
  # The prescribed ratio is the pacer's over the same spans, so both are 1 on
  # a no-change trial, whatever the participant's duty cycle. Cycle 3 alone
  # against 1-2 (the earlier measure) is kept as obs_ratio3 / exp_ratio3.
  pre_from <- if (!is.na(on[1])) 1L else 2L
  pre_n    <- mid + 1L - pre_from
  exp_pre  <- sum(d[pre_from:mid]) / pre_n
  out$exp_ratio  <- ((d[mid + 1L] + d[last] / 2) / 1.5) / exp_pre
  out$exp_ratio3 <- d[mid + 1L] / exp_pre
  if (!is.na(on[pre_from]) && !is.na(on[mid + 1L])) {
    obs_pre <- (on[mid + 1L] - on[pre_from]) / pre_n
    if (!is.na(pk[last])) out$obs_ratio <- ((pk[last] - on[mid + 1L]) / 1.5) / obs_pre
    if (!is.na(D[mid + 1L])) {
      out$obs_ratio3 <- D[mid + 1L] / obs_pre
      out$amp_ratio  <- A[mid + 1L] / mean(A[1:mid], na.rm = TRUE)
    }
  }
  # Fit and rate use cycles 1-3, whose durations are trough to trough.
  used_k <- 1:(mid + 1L)
  if (any(!is.na(idx[used_k]))) {
    out$dur_mae <- mean(abs(D[used_k] - d[used_k]), na.rm = TRUE)
    out$amp_rel <- mean(A[used_k], na.rm = TRUE) / amp_ref
    out$obs_rate_bpm <- 60 / mean(D[used_k], na.rm = TRUE)
  }
  # Belt-pacer synchrony: correlation of the detection-copy belt signal with
  # the circle waveform, shifted by the participant's lag.
  tt <- seq(0, sum(d), by = 1 / rec$fs)
  ii <- round((t0 + lag + tt - rec$t0) * rec$fs) + 1L
  ok <- ii >= 1 & ii <= length(rec$signal)
  if (sum(ok) > 2 * rec$fs) {
    w <- pacer_waveform(tt[ok], d)
    s <- rec$signal[ii[ok]]
    if (stats::sd(s, na.rm = TRUE) > 0) out$pacer_r <- stats::cor(w, s, use = "complete.obs")
  }
  out
}

# ── Cardiac: R peaks, RR cleaning, windowed HR / HRV (used by 05d) ────────────

# R-peak detection on a single ECG lead.
#  1. Band-pass 5-20 Hz (QRS band), differentiate, square, integrate over
#     150 ms (centred, so the integrated peak is not delayed).
#  2. Candidates: local maxima of the integrated signal above 30% of a running
#     reference (median of 2-s block maxima over the surrounding ~20 s), so
#     the threshold follows slow amplitude changes from movement or electrode
#     contact.
#  3. Refractory 0.3 s (200 bpm): of two candidates closer than that, the
#     larger wins.
#  4. Each beat is refined to the R wave itself: the extreme of the band-passed
#     ECG (5-30 Hz) within +-75 ms, in the lead's dominant polarity, with
#     parabolic interpolation, so timing is not limited to the 4-ms sample grid.
# Returns beat times (s, recording clock) with attribute "polarity".
detect_r_peaks <- function(x, fs, refract_s = 0.3) {
  n <- length(x)
  if (n < 10 * fs || !is.finite(stats::sd(x, na.rm = TRUE))) return(numeric(0))
  x[!is.finite(x)] <- stats::median(x, na.rm = TRUE)
  x <- x - stats::median(x)
  bq <- signal::butter(2, c(5, 20) / (fs / 2), type = "pass")
  y  <- as.numeric(signal::filtfilt(bq, x))
  w  <- max(1L, round(0.15 * fs))
  e  <- as.numeric(stats::filter(c(0, diff(y))^2, rep(1 / w, w), sides = 2))
  e[!is.finite(e)] <- 0

  blk  <- round(2 * fs)
  nb   <- ceiling(n / blk)
  bmax <- vapply(seq_len(nb), function(b) max(e[((b - 1) * blk + 1):min(n, b * blk)]), numeric(1))
  ref  <- stats::runmed(bmax, k = min(11L, 2L * floor((nb - 1) / 2) + 1L), endrule = "median")
  ref_s <- stats::approx((seq_len(nb) - 0.5) * blk, ref, xout = seq_len(n), rule = 2)$y

  cand <- which(diff(sign(diff(e))) == -2) + 1L
  cand <- cand[e[cand] > 0.3 * ref_s[cand]]
  if (length(cand) < 10) return(numeric(0))
  # Linear refractory pass: within refract_s keep the larger candidate.
  rs <- refract_s * fs
  keep <- integer(0)
  for (k in cand) {
    m <- length(keep)
    if (m && k - keep[m] < rs) { if (e[k] > e[keep[m]]) keep[m] <- k }
    else keep <- c(keep, k)
  }

  bw <- signal::butter(2, c(5, 30) / (fs / 2), type = "pass")
  z  <- as.numeric(signal::filtfilt(bw, x))
  half <- round(0.075 * fs)
  pol <- sign(sum(vapply(keep, function(k) {
    s <- z[max(1, k - half):min(n, k + half)]; if (max(s) >= -min(s)) 1 else -1 }, numeric(1))))
  if (pol == 0) pol <- 1
  zz <- pol * z
  fit <- vapply(keep, function(k) {
    lo <- max(2, k - half); hi <- min(n - 1, k + half)
    j <- lo - 1 + which.max(zz[lo:hi])
    a <- zz[j - 1]; b <- zz[j]; c3 <- zz[j + 1]
    den <- a - 2 * b + c3
    off <- if (is.finite(den) && den < 0) 0.5 * (a - c3) / den else 0
    c((j - 1 + max(-0.5, min(0.5, off))) / fs, b)
  }, numeric(2))
  o <- order(fit[1, ]); o <- o[!duplicated(fit[1, o])]
  t_pk <- fit[1, o]
  # R-wave height relative to the recording's median beat: a detached lead
  # still yields "beats" in its noise, at a small fraction of real R height.
  attr(t_pk, "amp_rel") <- fit[2, o] / stats::median(fit[2, o])
  attr(t_pk, "polarity") <- pol
  t_pk
}

# RR intervals with artefact flags. An interval is flagged if either bounding
# beat is under 30% of the median R height (lead off, noise), if it lies
# outside 0.3-2.0 s (200-30 bpm), or if it departs by more than 20% from the
# median of the surrounding 11 intervals (a local-median rule of the kind
# recommended for short-term HRV when ectopic and missed beats are not
# hand-edited).
clean_rr <- function(t_pk, min_amp_rel = 0.3) {
  if (length(t_pk) < 3) return(data.frame(t = numeric(0), rr = numeric(0), ok = logical(0)))
  amp <- attr(t_pk, "amp_rel"); if (is.null(amp)) amp <- rep(1, length(t_pk))
  good_beat <- is.finite(amp) & amp >= min_amp_rel
  rr  <- diff(as.numeric(t_pk))
  med <- stats::runmed(rr, k = min(11L, 2L * floor((length(rr) - 1) / 2) + 1L), endrule = "median")
  ok  <- good_beat[-1] & good_beat[-length(good_beat)] &
    rr >= 0.3 & rr <= 2.0 & abs(rr - med) <= 0.2 * med
  data.frame(t = as.numeric(t_pk)[-1], rr = rr, ok = ok)   # t = time of the beat ending the interval
}

# HR and HRV over [from, to] (s, recording clock).
#  hr_bpm   mean of 60/RR over clean intervals ending in the window
#  rmssd_ms, sdnn_ms from clean intervals; successive differences only where
#           both intervals are clean
#  hf_lnms2 ln power 0.15-0.40 Hz of the clean RR series resampled at 4 Hz
#           (Welch, 60-s Hann segments, 50% overlap); windows >= 60 s
#  pct_artifact share of intervals in the window that were flagged
hrv_window <- function(rr, from, to, freq = TRUE) {
  na <- data.frame(n_ibi = 0L, pct_artifact = NA_real_, hr_bpm = NA_real_,
                   rmssd_ms = NA_real_, sdnn_ms = NA_real_, hf_lnms2 = NA_real_)
  if (!is.finite(from) || !is.finite(to) || to <= from || !nrow(rr)) return(na)
  w <- rr[rr$t > from & rr$t <= to, ]
  if (nrow(w) < 3) return(na)
  g <- w$rr[w$ok]
  both <- w$ok[-1] & w$ok[-nrow(w)]
  sd_succ <- diff(w$rr)[both]
  out <- data.frame(
    n_ibi = nrow(w), pct_artifact = 100 * mean(!w$ok),
    hr_bpm = if (length(g)) mean(60 / g) else NA_real_,
    rmssd_ms = if (length(sd_succ) >= 2) 1000 * sqrt(mean(sd_succ^2)) else NA_real_,
    sdnn_ms = if (length(g) >= 3) 1000 * stats::sd(g) else NA_real_,
    hf_lnms2 = NA_real_)
  if (freq && (to - from) >= 60 && sum(w$ok) >= 50) {
    tt <- seq(from, to, by = 0.25)
    s  <- stats::approx(w$t[w$ok], w$rr[w$ok], xout = tt, rule = 2)$y * 1000
    s  <- s - mean(s)
    seg <- 240L; step <- 120L                     # 60 s at 4 Hz, 50% overlap
    if (length(s) >= seg) {
      hann <- 0.5 - 0.5 * cos(2 * pi * (0:(seg - 1)) / (seg - 1))
      starts <- seq(1L, length(s) - seg + 1L, by = step)
      P <- rowMeans(sapply(starts, function(a) {
        v <- s[a:(a + seg - 1L)]; v <- (v - mean(v)) * hann
        Mod(stats::fft(v))^2 / (4 * sum(hann^2))   # fs = 4 Hz
      }))
      f <- (0:(seg - 1)) * 4 / seg
      band <- f >= 0.15 & f <= 0.40
      hf <- 2 * sum(P[band]) * (4 / seg)
      out$hf_lnms2 <- if (hf > 0) log(hf) else NA_real_
    }
  }
  out
}

# Mean HR over [from, to] from clean intervals (for short windows: trials, clips).
hr_mean <- function(rr, from, to) {
  w <- rr[rr$t > from & rr$t <= to & rr$ok, ]
  if (!nrow(w)) NA_real_ else mean(60 / w$rr)
}

# Snap predicted event times to the nearest trigger of a given code.
snap_to <- function(pred, ev, tol = 0.2) {
  vapply(pred, function(p) {
    if (!is.finite(p) || !length(ev)) return(NA_real_)
    j <- which.min(abs(ev - p)); if (abs(ev[j] - p) <= tol) ev[j] else NA_real_
  }, numeric(1))
}

# ── Paced-trial schedule on the acq clock (used by 05c, 05d) ──────────────────
SNAP_TOL <- 0.15   # s: PsychoPy onset + clock offset to the nearest code-3 trigger

# One row per paced BCAT trial of a task file, with the trial parameters.
read_trial_rows <- function(task_file, task) {
  d <- read.csv(file.path(taskDataPath, task_file), stringsAsFactors = FALSE)
  keep <- if (task == "combined") !is.na(d$level) else !is.na(d$trials.thisN)
  keep <- keep & !is.na(suppressWarnings(as.numeric(d$trial.started)))
  d <- d[keep, ]
  num <- function(col) if (col %in% names(d)) suppressWarnings(as.numeric(d[[col]])) else NA_real_
  data.frame(
    task          = task,
    task_file     = task_file,
    trial         = seq_len(nrow(d)),
    block         = if (task == "combined") num("OuterLoop.thisN") + 1 else NA_real_,
    level         = num("level"),
    direction     = num("Direction"),
    high_salience = if (task == "combined") num("changeSalience") == 1 else TRUE,
    accuracy      = num("Accuracy"),
    arousal       = if (task == "combined") num("arousalSlider.response") else num("ArousalRating"),
    confidence    = num("confidenceSlider.response"),
    started       = num("trial.started"),
    stringsAsFactors = FALSE)
}

# The participant's paced trials on the acq clock, with expected durations
# (attribute "durations"); onset snapped to the code-3 trigger when one lies
# within SNAP_TOL.
trial_schedule <- function(px) {
  tc <- px$task_clock
  tc <- tc[tc$task %in% c("bcat_baseline", "combined"), ]
  tr <- do.call(rbind, lapply(seq_len(nrow(tc)), function(k) {
    rows <- read_trial_rows(tc$task_file[k], tc$task[k])
    rows$pred_onset   <- rows$started + tc$offset[k]
    rows$clock_source <- tc$source[k]
    rows
  }))
  if (is.null(tr) || !nrow(tr)) return(NULL)
  ev3 <- px$events$time[px$events$code == 3L]
  snap <- vapply(tr$pred_onset, function(p) {
    if (!length(ev3)) return(NA_real_)
    j <- which.min(abs(ev3 - p)); if (abs(ev3[j] - p) <= SNAP_TOL) ev3[j] else NA_real_
  }, numeric(1))
  tr$onset_source <- ifelse(is.na(snap), "psychopy_clock", "trigger")
  tr$t0 <- ifelse(is.na(snap), tr$pred_onset, snap)
  attr(tr, "durations") <- lapply(seq_len(nrow(tr)), function(k)
    pacer_durations(tr$level[k], tr$direction[k], isTRUE(tr$high_salience[k])))
  tr
}

