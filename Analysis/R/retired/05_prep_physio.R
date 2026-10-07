# R/05_prep_physio.R
# Processes BIOPAC .acq physio files for the dual-workstation lab setup.
#
# ── Lab setup ──────────────────────────────────────────────────────────────────
# Two participants run simultaneously at Left (L) and Right (R) computers.
# Both workstations share a single BIOPAC MP160 acquisition unit.
#
# Filename convention:  L<leftID>_R<rightID>.acq
#   e.g.  L11111_R22222.acq
#   An ID of 33333 means that seat was empty during the session.
#
# ── Physio channel assignment (fixed hardware wiring) ─────────────────────────
#   Ch 0  Breath 1  →  L seat respiration belt
#   Ch 1  Breath 2  →  R seat respiration belt
#   Ch 2  Heart 1   →  L seat ECG
#   Ch 3  Heart 2   →  R seat ECG
#   Ch 12 Experiment Triggers  (combined parallel-port channel, see below)
#
# Belt and ECG lead order is consistent across all sessions; channel assignment
# is determined by hardware wiring, not inferred from signal quality.
#
# ── Trigger channel encoding ───────────────────────────────────────────────────
# The MP160 receives both workstations' parallel-port output on a single 8-bit
# channel.  The port pins are split by nibble:
#
#   Bits 0-3  (mask 0x0F, values   1-15)  =  L computer
#   Bits 4-7  (mask 0xF0, values  16-240) =  R computer
#
# The two nibbles are mutually exclusive within any sample (empirically
# confirmed: no mixed-nibble events observed).  Both workstations send
# identical marker codes for the same task events; R-side codes are
# normalised to the same 1-15 scale by right-shifting 4 bits: (raw >> 4).
#
# Known marker codes (identical for both computers after normalisation):
#   1  = experiment start
#   3  = BCAT trial onset
#   4  = BCAT trial offset
#   7  = heartbeat task start
#   8  = heartbeat task stop
#   9  = heartbeat task end signal
#   11 = GERT trial onset
#   12 = GERT trial offset
#
# ── Output ─────────────────────────────────────────────────────────────────────
# Three .rds files per non-empty seat, written to rdsPath:
#   <pid>_resp.rds     respiration signal at 25 Hz  (downsample factor 80)
#   <pid>_card.rds     ECG signal at 250 Hz          (downsample factor 8)
#   <pid>_triggers.rds event onset list, rate-agnostic
#
# See physio_preprocessing_methods.md for spectral justification of these rates.
#
# _resp.rds and _card.rds structure:
#   $id              character   participant ID
#   $seat            character   "L" or "R"
#   $signal_type     character   "resp" or "card"
#   $hz              integer     sample rate after downsampling
#   $duration_sec    numeric     recording length in seconds
#   $signal          numeric     downsampled signal (Volts resp / mV card)
#   $raw_channel     integer     0-based channel index in original .acq
#   $notes           character   QC warnings; empty string if clean
#
# _triggers.rds structure (data.frame):
#   $sample_idx_raw  integer     onset sample in original 2000 Hz recording
#   $time_sec        numeric     onset time in seconds from recording start
#   $code            integer     event code (1-15, seat-normalised)
#
# To align triggers to a downsampled signal:
#   sample_idx_downsampled <- round(triggers$time_sec * hz)
#
# physio_summary.csv: one row per participant per signal type (resp/card/triggers).
# ───────────────────────────────────────────────────────────────────────────────

message("05 | Physio prep...")

# ── Python environment check ───────────────────────────────────
if (!reticulate::py_module_available("bioread")) {
  message("  Installing bioread in Python environment...")
  reticulate::py_install("bioread", pip = TRUE)
}
bioread <- reticulate::import("bioread")

# Fallback constants in case config.R predates these variables.
# Canonical definitions belong in config.R; these are guards only.
if (!exists("PHYSIO_RESP_HZ")) PHYSIO_RESP_HZ <- 25L
if (!exists("PHYSIO_CARD_HZ")) PHYSIO_CARD_HZ <- 250L
if (!exists("PHYSIO_RAW_HZ"))  PHYSIO_RAW_HZ  <- 2000L

if (!dir.exists(rdsPath)) dir.create(rdsPath, recursive = TRUE)

# ── Helpers ────────────────────────────────────────────────────

# Integer-factor decimation (keep every Nth sample)
.downsample <- function(x, factor) x[seq(1L, length(x), by = factor)]

# Extract onset events from a per-sample integer trigger vector.
# Returns data.frame(sample_idx, time_sec, code).
.trigger_onsets <- function(trig, hz) {
  prev  <- c(0L, trig[-length(trig)])
  idx   <- which(prev == 0L & trig != 0L)
  data.frame(sample_idx = idx, time_sec = idx / hz, code = trig[idx])
}

# QC check: warn if a channel looks flat (likely disconnected).
# Returns a warning string, or "" if the signal looks healthy.
.check_signal <- function(x, label, min_std = CHANNEL_FLAT_STD) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s))
    sprintf("WARNING: %s has no valid samples (all NA)", label)
  else if (s < min_std)
    sprintf("WARNING: %s appears flat (std = %.4f)", label, s)
  else
    ""
}

# ── File discovery ─────────────────────────────────────────────
acq_files <- list.files(physioPath, pattern = "\\.acq$",
                        full.names = FALSE, ignore.case = TRUE)

if (length(acq_files) == 0) {
  message("  No .acq files found in physioPath. Skipping.")
} else {
  message(sprintf("  Found %d .acq file(s)", length(acq_files)))
}

# ── Filename parser ────────────────────────────────────────────
# Returns list(L_id, R_id); NA if that seat was empty (ID was 44444).
# Normalises path separators before parsing so this works on Windows,
# where list.files() may return backslash-separated paths that
# basename() does not always strip correctly.
.parse_filename <- function(fname) {
  # Normalise to forward slashes then extract stem
  stem <- sub("\\.[^.]+$", "", basename(gsub("\\\\", "/", fname)))
  
  # Match the L<digits>_R<digits> pattern explicitly
  # Separator between L/R and digits may be "." or "_" (or absent).
  # File order may be L...R... or R...L... -- try both.
  if (grepl("L[._]?\\d+[._]R[._]?\\d+", stem, perl = TRUE)) {
    m     <- regmatches(stem, regexpr("L[._]?(\\d+)[._]R[._]?(\\d+)", stem, perl = TRUE))
    L_raw <- sub("L[._]?(\\d+)[._]R[._]?(\\d+)", "\\1", m, perl = TRUE)
    R_raw <- sub("L[._]?(\\d+)[._]R[._]?(\\d+)", "\\2", m, perl = TRUE)
  } else if (grepl("R[._]?\\d+[._]L[._]?\\d+", stem, perl = TRUE)) {
    m     <- regmatches(stem, regexpr("R[._]?(\\d+)[._]L[._]?(\\d+)", stem, perl = TRUE))
    R_raw <- sub("R[._]?(\\d+)[._]L[._]?(\\d+)", "\\1", m, perl = TRUE)
    L_raw <- sub("R[._]?(\\d+)[._]L[._]?(\\d+)", "\\2", m, perl = TRUE)
  } else {
    warning("Cannot parse L/R IDs from filename: ", fname)
    return(list(L_id = NA_character_, R_id = NA_character_))
  }
  
  # 55555 (all zeros) means that seat was empty
  to_id <- function(x) if (as.integer(x) == 0L) NA_character_ else x
  
  list(L_id = to_id(L_raw), R_id = to_id(R_raw))
}

# ── Main loop ─────────────────────────────────────────────────
summary_rows <- list()

for (fname in acq_files) {
  
  ids  <- .parse_filename(fname)
  L_id <- ids$L_id
  R_id <- ids$R_id
  
  if (is.na(L_id) && is.na(R_id)) {
    message(sprintf("  [SKIP] %s: both seats empty", fname))
    next
  }
  
  # Which seats still need processing?
  # A seat is complete only when all three .rds files exist.
  .seat_done <- function(pid) {
    all(file.exists(file.path(rdsPath, paste0(pid, c("_resp.rds", "_card.rds", "_triggers.rds")))))
  }
  seats_needed <- c(
    if (!is.na(L_id) && !.seat_done(L_id)) "L",
    if (!is.na(R_id) && !.seat_done(R_id)) "R"
  )
  if (length(seats_needed) == 0) {
    message(sprintf("  [SKIP] %s (all rds exist)", fname))
    next
  }
  
  message(sprintf("  Loading %s  (L=%s, R=%s)...",
                  fname,
                  if (is.na(L_id)) "empty" else L_id,
                  if (is.na(R_id)) "empty" else R_id))
  
  acq <- tryCatch(
    bioread$read_file(file.path(physioPath, fname)),
    error = function(e) { warning("Failed to read ", fname, ": ", e$message); NULL }
  )
  if (is.null(acq)) next
  
  ch_names <- vapply(acq$channels, function(ch) ch$name, character(1))
  raw_hz   <- acq$samples_per_second
  # Locate channels by name
  breath_idx <- which(grepl("^Breath [12]$", ch_names))   # expect indices 1, 2
  heart_idx  <- which(grepl("^Heart [12]$",  ch_names))   # expect indices 3, 4
  trig_idx   <- which(grepl("Experiment Triggers", ch_names, ignore.case = TRUE))[1]
  
  if (length(breath_idx) < 2 || length(heart_idx) < 2 || is.na(trig_idx)) {
    warning("Unexpected channel layout in ", fname,
            ". Channels: ", paste(ch_names, collapse = ", "))
    next
  }
  
  # Fixed assignment: index 1 = L seat, index 2 = R seat
  breath_L <- as.numeric(acq$channels[[breath_idx[1]]]$data)
  breath_R <- as.numeric(acq$channels[[breath_idx[2]]]$data)
  heart_L  <- as.numeric(acq$channels[[heart_idx[1]]]$data)
  heart_R  <- as.numeric(acq$channels[[heart_idx[2]]]$data)
  dur_sec  <- length(breath_L) / raw_hz
  
  # Raw trigger channel -> split into L and R normalised codes
  trig_raw <- as.integer(round(as.numeric(acq$channels[[trig_idx]]$data)))
  trig_L   <- bitwAnd(trig_raw, 0x0FL)                          # bits 0-3
  trig_R   <- bitwShiftR(bitwAnd(trig_raw, 0xF0L), 4L)         # bits 4-7, normalised
  
  # ── Per-seat processing ────────────────────────────────────
  seat_specs <- list(
    L = list(id = L_id, breath = breath_L, heart = heart_L, trig = trig_L,
             breath_ch = breath_idx[1] - 1L, heart_ch = heart_idx[1] - 1L),
    R = list(id = R_id, breath = breath_R, heart = heart_R, trig = trig_R,
             breath_ch = breath_idx[2] - 1L, heart_ch = heart_idx[2] - 1L)
  )
  
  for (seat in seats_needed) {
    s   <- seat_specs[[seat]]
    pid <- s$id
    if (is.na(pid)) next
    
    # QC warnings -- checked separately per signal so the CSV logs them independently
    resp_notes <- .check_signal(s$breath, paste("Breath", seat))
    card_notes <- .check_signal(s$heart,  paste("Heart",  seat))
    for (qc_msg in Filter(nchar, c(resp_notes, card_notes)))
      message("    QC: ", qc_msg)
    
    # If both signals are unreadable, log to summary and skip rather than error
    if (grepl("all NA", resp_notes) && grepl("all NA", card_notes)) {
      message(sprintf("    [SKIP] %s: both channels all NA -- recording issue, not processed", pid))
      for (sig in c("resp", "card", "triggers")) {
        summary_rows[[length(summary_rows) + 1]] <- data.frame(
          id           = pid,
          seat         = seat,
          signal_type  = sig,
          source_file  = fname,
          duration_min = NA_real_,
          n_triggers   = NA_integer_,
          raw_channel  = switch(sig, resp = s$breath_ch, card = s$heart_ch, triggers = NA_integer_),
          qc_notes     = "SKIPPED: both channels all NA",
          flag         = TRUE,
          stringsAsFactors = FALSE
        )
      }
      next
    }
    
    # Downsample at signal-specific target rates
    # resp: 25 Hz (factor 80) -- respiration is < 2 Hz; see physio_preprocessing_methods.md
    # card: 250 Hz (factor 8)  -- ECG needs >= 150 Hz for QRS capture
    resp_ds <- .downsample(s$breath, as.integer(raw_hz / PHYSIO_RESP_HZ))
    card_ds <- .downsample(s$heart,  as.integer(raw_hz / PHYSIO_CARD_HZ))
    
    # Extract trigger onsets at raw rate; event list is sampling-rate-agnostic.
    # To align to a downsampled signal: round(time_sec * target_hz)
    events <- .trigger_onsets(s$trig, raw_hz)
    
    # Respiration file
    saveRDS(list(
      id           = pid,
      seat         = seat,
      signal_type  = "resp",
      hz           = PHYSIO_RESP_HZ,
      duration_sec = dur_sec,
      signal       = resp_ds,
      raw_channel  = s$breath_ch,
      notes        = resp_notes
    ), file.path(rdsPath, paste0(pid, "_resp.rds")))
    
    # Cardiac file
    saveRDS(list(
      id           = pid,
      seat         = seat,
      signal_type  = "card",
      hz           = PHYSIO_CARD_HZ,
      duration_sec = dur_sec,
      signal       = card_ds,
      raw_channel  = s$heart_ch,
      notes        = card_notes
    ), file.path(rdsPath, paste0(pid, "_card.rds")))
    
    # Trigger event list (shared across both signals; no resampling)
    saveRDS(events, file.path(rdsPath, paste0(pid, "_triggers.rds")))
    
    message(sprintf("    -> %s_resp.rds + _card.rds + _triggers.rds  seat=%s  n_events=%d  dur=%.1f min",
                    pid, seat, nrow(events), dur_sec / 60))
    
    # One summary row per signal type; notes are signal-specific
    for (sig in c("resp", "card", "triggers")) {
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        id           = pid,
        seat         = seat,
        signal_type  = sig,
        source_file  = fname,
        duration_min = round(dur_sec / 60, 1),
        n_triggers   = nrow(events),
        raw_channel  = switch(sig, resp = s$breath_ch, card = s$heart_ch, triggers = NA_integer_),
        qc_notes     = switch(sig, resp = resp_notes, card = card_notes, triggers = ""),
        flag         = switch(sig, resp = nchar(resp_notes) > 0,
                              card = nchar(card_notes) > 0,
                              triggers = FALSE),
        stringsAsFactors = FALSE
      )
    }
    
  }
}

# ── Write / append summary ────────────────────────────────────
if (length(summary_rows) > 0) {
  new_rows <- dplyr::bind_rows(summary_rows)
  if (file.exists(physioSummaryFile)) {
    new_rows <- dplyr::bind_rows(
      read.csv(physioSummaryFile, stringsAsFactors = FALSE),
      new_rows
    )
  }
  write.csv(new_rows, physioSummaryFile, row.names = FALSE)
  message(sprintf("  -> physio_summary.csv updated (%d new entries)", length(summary_rows)))
} else {
  message("  No new files processed.")
}

message("05 | Physio prep complete.")