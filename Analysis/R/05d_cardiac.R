# R/05d_cardiac.R
# Cardiac measures for every participant with physio: R peaks and RR series,
# resting HR/HRV, heartbeat-counting (Schandry) accuracy, task-level HRV, and
# HR on every BCAT trial and GERT clip.
#
# Writes to physioQCPath:
#   cardiac_participants.csv  one row per participant
#   cardiac_hbd.csv           one row per heartbeat-counting interval
#   cardiac_bcat_trials.csv   one row per paced BCAT trial (baseline + combined)
#   cardiac_gert_clips.csv    one row per GERT clip (baseline + combined)
# Reads physioRdsPath/<id>_cardiac.rds (beat times, R amplitude, RR with
# flags), written by 05e from the ECG.
# Methods: Methods/cardiac_features.md.
# ---------------------------------------------------------------

message("05d | Cardiac features...")

load_respkit()
source(file.path(analysisPath, "R", "physio_functions.R"))

REST_PRE_S   <- 125   # rest window: up to this long before the BCAT script starts...
REST_GAP_S   <- 5     # ...ending this long before it
REST_MIN_S   <- 60
HBD_SNAP_TOL <- 0.2
CLIP_PRE_S   <- 2     # pre-clip reference window for clip HR change

num <- function(d, col) if (col %in% names(d)) suppressWarnings(as.numeric(d[[col]])) else rep(NA_real_, nrow(d))

# The GERT baseline file the behavioural pipeline uses (03: > 30 KB, earliest).
gert_files <- list.files(taskDataPath, pattern = "_GERT_baseline_.*[.]csv$")
gert_files <- gert_files[file.info(file.path(taskDataPath, gert_files))$size > 30000]
gert_files <- sort(gert_files)
gert_by_id <- tapply(gert_files, sub("^([^_]+)_.*$", "\\1", gert_files), function(x) x[1])

clip_rows <- function(task_file) {
  d <- read.csv(file.path(taskDataPath, task_file), stringsAsFactors = FALSE)
  d <- d[!is.na(d$vidCorrect) & d$vidCorrect != "" & !is.na(num(d, "testVideo.started")), ]
  data.frame(task_file = task_file, clip = seq_len(nrow(d)),
             block = if ("OuterLoop.thisN" %in% names(d)) num(d, "OuterLoop.thisN") + 1 else NA_real_,
             FileName = d$FileName, start = num(d, "testVideo.started"),
             # testVideo.stopped is missing in many files; the routine that
             # shows the clip always logs its end.
             stop = num(d, "showVideo.stopped"), stringsAsFactors = FALSE)
}

load_beats <- function(pid) {
  f <- file.path(physioRdsPath, paste0(pid, "_cardiac.rds"))
  if (!file.exists(f)) return(list(pk = structure(numeric(0), amp_rel = numeric(0), polarity = NA_real_),
                                   rr = clean_rr(numeric(0)), channel = NA_character_))
  b <- readRDS(f)
  list(pk = structure(as.numeric(b$r_peaks), amp_rel = b$r_amp_rel, polarity = b$polarity),
       rr = b$rr, channel = b$channel)
}

files <- sort(list.files(physioRdsPath, "_physio[.]rds$", full.names = TRUE))
only  <- strsplit(Sys.getenv("PHYSIO_IDS", ""), ",")[[1]]
sfx   <- ""
if (length(only)) { files <- files[sub("_physio[.]rds$", "", basename(files)) %in% only]; sfx <- "_subset" }
message(sprintf("  %d participant physio files", length(files)))

P <- list(); H <- list(); B <- list(); G <- list()
for (f in files) {
  px  <- readRDS(f)
  pid <- px$id
  # Beat times come from 05e (R-peak detection on the ECG, lab only); the
  # de-identified dataset carries them in place of the ECG waveform.
  beats <- load_beats(pid)
  pk <- beats$pk; rr <- beats$rr
  row <- list(id = pid, heart_source = px$heart_source, card_channel = beats$channel,
              ecg_quality = if (px$heart_source == "own_seat") px$checks$ecg_quality_own
                            else px$checks$ecg_quality_other)
  row$n_beats      <- length(pk)
  row$pct_flagged  <- if (nrow(rr)) 100 * mean(!rr$ok) else NA_real_
  row$ecg_polarity <- if (length(pk)) attr(pk, "polarity") else NA_real_

  tc <- px$task_clock
  base <- tc[tc$task == "bcat_baseline", ]
  base <- base[order(-base$n_matched), ][seq_len(min(1, nrow(base))), ]
  comb <- tc[tc$task == "combined", ]
  comb <- comb[order(-comb$n_matched), ][seq_len(min(1, nrow(comb))), ]

  # ── Rest: the minutes between hook-up and the BCAT script ─────
  if (nrow(base)) {
    to <- base$offset - REST_GAP_S; from <- max(5, base$offset - REST_PRE_S)
    row$rest_window_s <- to - from
    if (to - from >= REST_MIN_S) {
      hv <- hrv_window(rr, from, to)
      names(hv) <- paste0("rest_", names(hv)); row <- c(row, as.list(hv))
      if (!is.null(px$resp) && identical(px$breath_source %in% c("own_seat", "other_seat"), TRUE)) {
        rres <- tryCatch(suppressWarnings(suppressMessages(resp_analyse(px$resp))), error = function(e) NULL)
        if (!is.null(rres)) {
          b <- rres$breaths[rres$breaths$t_start >= from & rres$breaths$t_end <= to, ]
          row$rest_resp_rate_bpm <- if (nrow(b) >= 5) 60 / stats::median(b$duration) else NA_real_
        }
      }
    }
  }

  # ── Heartbeat counting (3 intervals, BCAT baseline script) ────
  if (nrow(base)) {
    d <- read.csv(file.path(taskDataPath, base$task_file), stringsAsFactors = FALSE)
    h <- d[!is.na(num(d, "heartLoop.thisN")), ]
    if (nrow(h)) {
      ev7 <- px$events$time[px$events$code == 7L]; ev8 <- px$events$time[px$events$code == 8L]
      on  <- num(h, "countScreen.started") + base$offset
      off <- num(h, "countScreen.stopped") + base$offset
      on_s <- snap_to(on, ev7, HBD_SNAP_TOL); off_s <- snap_to(off, ev8, HBD_SNAP_TOL)
      hb <- data.frame(id = pid, interval = seq_len(nrow(h)), condition = h$thisCondition,
                       nominal_s = num(h, "countDuration"),
                       start = ifelse(is.na(on_s), on, on_s), end = ifelse(is.na(off_s), off, off_s),
                       timing = ifelse(is.na(on_s) | is.na(off_s), "psychopy_clock", "trigger"),
                       reported = num(h, "HRreport"), stringsAsFactors = FALSE)
      good_pk <- as.numeric(pk)[attr(pk, "amp_rel") >= 0.3]
      hb$actual <- vapply(seq_len(nrow(hb)), function(k)
        sum(good_pk >= hb$start[k] & good_pk < hb$end[k]), numeric(1))
      hb$pct_artifact <- vapply(seq_len(nrow(hb)), function(k)
        hrv_window(rr, hb$start[k], hb$end[k], freq = FALSE)$pct_artifact, numeric(1))
      hb$schandry <- ifelse(hb$actual > 0 & is.finite(hb$reported),
                            pmax(0, 1 - abs(hb$actual - hb$reported) / hb$actual), NA_real_)
      hb$valid <- is.finite(hb$schandry) & hb$pct_artifact <= 10
      H[[pid]] <- hb
      row$hbd_n_valid <- sum(hb$valid)
      row$hbd_accuracy <- if (sum(hb$valid) == nrow(hb)) mean(hb$schandry) else NA_real_
    }
  }

  # ── BCAT trials: HR before and after the change, time-locked ──
  # HR falls ~4 bpm over every trial (attentional slowing), and trials differ
  # in length (accelerated ~13.7 s, decelerated ~18.7 s), so windows defined by
  # pacer cycles compare different latencies. Fixed latency bins do not:
  # 0-8 s is cycles 1-2, identical in all conditions; 8-13 s lies inside every
  # trial, after the change.
  tr <- tryCatch(trial_schedule(px), error = function(e) NULL)
  if (!is.null(tr)) {
    dur <- attr(tr, "durations")
    bt <- do.call(rbind, lapply(seq_len(nrow(tr)), function(k) {
      t0 <- tr$t0[k]; end <- t0 + sum(dur[[k]])
      data.frame(hr_0_8 = hr_mean(rr, t0, t0 + 8), hr_8_13 = hr_mean(rr, t0 + 8, t0 + 13),
                 hr_trial = hr_mean(rr, t0, end),
                 pct_artifact = hrv_window(rr, t0, t0 + 13, freq = FALSE)$pct_artifact)
    }))
    B[[pid]] <- cbind(data.frame(id = pid), tr[, c("task", "task_file", "trial", "block", "level",
                                                   "direction", "high_salience", "accuracy",
                                                   "arousal", "onset_source", "t0")], bt)
  }

  # ── GERT clips and task-level HRV ─────────────────────────────
  clips <- list()
  if (nrow(comb)) {
    cr <- tryCatch(clip_rows(comb$task_file), error = function(e) NULL)
    if (!is.null(cr) && nrow(cr)) { cr$task <- "combined"; cr$offset <- comb$offset
                                    cr$clock <- comb$source; clips$combined <- cr }
  }
  gf <- if (pid %in% names(gert_by_id)) gert_by_id[[pid]] else NA
  if (!is.na(gf)) {
    cr <- tryCatch(clip_rows(gf), error = function(e) NULL)
    ev11 <- px$events$time[px$events$code == 11L]
    if (!is.null(cr) && nrow(cr) >= 5 && length(ev11) >= 5) {
      m <- match_clock(ev11, cr$start, tol = 0.1)
      if (m$frac >= 0.8) {
        cr$task <- "gert_baseline"; cr$offset <- m$offset; cr$clock <- "trigger"
        clips$gert_baseline <- cr
      }
      row$gert_baseline_clock_frac <- m$frac
    }
  }
  if (length(clips)) {
    cl <- do.call(rbind, clips)
    cl$on  <- cl$start + cl$offset; cl$off <- cl$stop + cl$offset
    cl$hr_clip <- vapply(seq_len(nrow(cl)), function(k) hr_mean(rr, cl$on[k], cl$off[k]), numeric(1))
    cl$hr_pre  <- vapply(seq_len(nrow(cl)), function(k) hr_mean(rr, cl$on[k] - CLIP_PRE_S, cl$on[k]), numeric(1))
    cl$pct_artifact <- vapply(seq_len(nrow(cl)), function(k)
      hrv_window(rr, cl$on[k] - CLIP_PRE_S, cl$off[k], freq = FALSE)$pct_artifact, numeric(1))
    G[[pid]] <- cbind(data.frame(id = pid), cl[, c("task", "task_file", "block", "clip", "FileName",
                                                   "on", "off", "hr_pre", "hr_clip", "pct_artifact")])
    for (tk in names(clips)) {
      s <- cl[cl$task == tk, ]
      span_from <- if (tk == "combined" && !is.null(tr))
        min(tr$t0[tr$task == "combined"], s$on, na.rm = TRUE) else min(s$on, na.rm = TRUE)
      span_to <- max(s$off, na.rm = TRUE)
      hv <- hrv_window(rr, span_from, span_to)
      names(hv) <- paste0(tk, "_", names(hv)); row <- c(row, as.list(hv))
      row[[paste0(tk, "_span_min")]] <- (span_to - span_from) / 60
    }
  }
  P[[pid]] <- as.data.frame(row, stringsAsFactors = FALSE)
}

out <- list(cardiac_participants = P, cardiac_hbd = H, cardiac_bcat_trials = B, cardiac_gert_clips = G)
for (nm in names(out)) {
  df <- dplyr::bind_rows(out[[nm]])
  write.csv(df, file.path(physioQCPath, paste0(nm, sfx, ".csv")), row.names = FALSE)
  message(sprintf("  -> %s%s.csv (%d rows)", nm, sfx, nrow(df)))
}
message("05d | Cardiac features complete.")
