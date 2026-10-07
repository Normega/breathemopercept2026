# qc_physio_rds.R
# QC checks on processed physio RDS files.
# Sourced from main.R after 05_prep_physio.R has completed.
# Assumes config.R, packages, and functions.R already loaded by main.R.
# Writes qc_physio_report.csv to resultsPath and prints a console summary.
# ---------------------------------------------------------------

message("QC | Physio RDS checks...")

# ── Discover RDS files ─────────────────────────────────────────
resp_files     <- list.files(rdsPath, pattern = "_resp\\.rds$",     full.names = TRUE)
card_files     <- list.files(rdsPath, pattern = "_card\\.rds$",     full.names = TRUE)
trigger_files  <- list.files(rdsPath, pattern = "_triggers\\.rds$", full.names = TRUE)

resp_ids    <- sub("_resp\\.rds$",     "", basename(resp_files))
card_ids    <- sub("_card\\.rds$",     "", basename(card_files))
trigger_ids <- sub("_triggers\\.rds$", "", basename(trigger_files))

message(sprintf("Found: %d resp, %d card, %d trigger files",
                length(resp_files), length(card_files), length(trigger_files)))

# ── Check file-set completeness ────────────────────────────────
all_ids     <- union(union(resp_ids, card_ids), trigger_ids)
missing_resp    <- setdiff(all_ids, resp_ids)
missing_card    <- setdiff(all_ids, card_ids)
missing_trigger <- setdiff(all_ids, trigger_ids)

if (length(missing_resp)    > 0) message("Missing _resp.rds:     ", paste(missing_resp,    collapse = ", "))
if (length(missing_card)    > 0) message("Missing _card.rds:     ", paste(missing_card,    collapse = ", "))
if (length(missing_trigger) > 0) message("Missing _triggers.rds: ", paste(missing_trigger, collapse = ", "))

# ── Per-participant QC ─────────────────────────────────────────
qc_rows <- list()

for (pid in sort(all_ids)) {
  
  resp_path <- file.path(rdsPath, paste0(pid, "_resp.rds"))
  card_path <- file.path(rdsPath, paste0(pid, "_card.rds"))
  trig_path <- file.path(rdsPath, paste0(pid, "_triggers.rds"))
  
  row <- data.frame(
    id                = pid,
    # File existence
    has_resp          = file.exists(resp_path),
    has_card          = file.exists(card_path),
    has_triggers      = file.exists(trig_path),
    # Populated below
    duration_min      = NA_real_,
    resp_hz           = NA_real_,
    card_hz           = NA_real_,
    resp_n_samples    = NA_integer_,
    card_n_samples    = NA_integer_,
    resp_mean         = NA_real_,
    resp_sd           = NA_real_,
    resp_pct_na       = NA_real_,
    card_mean         = NA_real_,
    card_sd           = NA_real_,
    card_pct_na       = NA_real_,
    n_triggers        = NA_integer_,
    trigger_codes     = NA_character_,
    has_exp_start     = NA,   # code 1
    has_bcat_trials   = NA,   # codes 3/4
    has_gert_trials   = NA,   # codes 11/12
    seat              = NA_character_,
    rds_notes_resp    = NA_character_,
    rds_notes_card    = NA_character_,
    flag              = FALSE,
    flag_reasons      = NA_character_,
    stringsAsFactors  = FALSE
  )
  
  flags <- character(0)
  
  # ── Resp ──────────────────────────────────────────────────────
  if (file.exists(resp_path)) {
    resp <- tryCatch(readRDS(resp_path), error = function(e) NULL)
    if (is.null(resp)) {
      flags <- c(flags, "resp RDS unreadable")
    } else {
      row$seat           <- resp$seat
      row$duration_min   <- round(resp$duration_sec / 60, 1)
      row$resp_hz        <- resp$hz
      row$resp_n_samples <- length(resp$signal)
      row$resp_mean      <- round(mean(resp$signal, na.rm = TRUE), 4)
      row$resp_sd        <- round(sd(resp$signal,   na.rm = TRUE), 4)
      row$resp_pct_na    <- round(100 * mean(is.na(resp$signal)), 2)
      row$rds_notes_resp <- resp$notes
      
      # Expected sample count given duration and hz
      expected_resp <- round(resp$duration_sec * resp$hz)
      if (abs(row$resp_n_samples - expected_resp) > resp$hz)
        flags <- c(flags, sprintf("resp length mismatch (%d vs expected %d)", row$resp_n_samples, expected_resp))
      
      if (row$resp_sd < 0.05)
        flags <- c(flags, sprintf("resp flat (sd=%.4f)", row$resp_sd))
      
      if (row$resp_pct_na > 5)
        flags <- c(flags, sprintf("resp %.1f%% NA", row$resp_pct_na))
      
      # Plausibility: resp signal should be positive-biased (belt voltage)
      if (!is.na(row$resp_mean) && row$resp_mean < -1)
        flags <- c(flags, sprintf("resp mean implausibly negative (%.2f)", row$resp_mean))
      
      if (nchar(resp$notes) > 0)
        flags <- c(flags, paste0("resp note: ", resp$notes))
    }
  } else {
    flags <- c(flags, "missing _resp.rds")
  }
  
  # ── Card ──────────────────────────────────────────────────────
  if (file.exists(card_path)) {
    card <- tryCatch(readRDS(card_path), error = function(e) NULL)
    if (is.null(card)) {
      flags <- c(flags, "card RDS unreadable")
    } else {
      row$card_hz        <- card$hz
      row$card_n_samples <- length(card$signal)
      row$card_mean      <- round(mean(card$signal, na.rm = TRUE), 4)
      row$card_sd        <- round(sd(card$signal,   na.rm = TRUE), 4)
      row$card_pct_na    <- round(100 * mean(is.na(card$signal)), 2)
      row$rds_notes_card <- card$notes
      
      expected_card <- round(card$duration_sec * card$hz)
      if (abs(row$card_n_samples - expected_card) > card$hz)
        flags <- c(flags, sprintf("card length mismatch (%d vs expected %d)", row$card_n_samples, expected_card))
      
      if (row$card_sd < 0.05)
        flags <- c(flags, sprintf("card flat (sd=%.4f)", row$card_sd))
      
      if (row$card_pct_na > 5)
        flags <- c(flags, sprintf("card %.1f%% NA", row$card_pct_na))
      
      if (nchar(card$notes) > 0)
        flags <- c(flags, paste0("card note: ", card$notes))
    }
  } else {
    flags <- c(flags, "missing _card.rds")
  }
  
  # ── Triggers ──────────────────────────────────────────────────
  if (file.exists(trig_path)) {
    triggers <- tryCatch(readRDS(trig_path), error = function(e) NULL)
    if (is.null(triggers)) {
      flags <- c(flags, "triggers RDS unreadable")
    } else {
      row$n_triggers      <- nrow(triggers)
      row$trigger_codes   <- paste(sort(unique(triggers$code)), collapse = ",")
      row$has_exp_start   <- 1L  %in% triggers$code
      row$has_bcat_trials <- all(c(3L, 4L) %in% triggers$code)
      row$has_gert_trials <- all(c(11L, 12L) %in% triggers$code)
      
      if (row$n_triggers == 0)
        flags <- c(flags, "no trigger events found")
      if (!isTRUE(row$has_exp_start))
        flags <- c(flags, "missing experiment-start trigger (code 1)")
      if (!isTRUE(row$has_bcat_trials))
        flags <- c(flags, "missing BCAT trial triggers (codes 3/4)")
      if (!isTRUE(row$has_gert_trials))
        flags <- c(flags, "missing GERT trial triggers (codes 11/12)")
      
      # Duration sanity: triggers should not extend beyond the recording
      if (!is.null(resp) && !is.null(triggers) && nrow(triggers) > 0) {
        if (max(triggers$time_sec) > resp$duration_sec)
          flags <- c(flags, "trigger time exceeds recording duration")
      }
    }
  } else {
    flags <- c(flags, "missing _triggers.rds")
  }
  
  row$flag         <- length(flags) > 0
  row$flag_reasons <- if (length(flags) > 0) paste(flags, collapse = " | ") else ""
  
  qc_rows[[length(qc_rows) + 1]] <- row
}

# ── Compile and write report ───────────────────────────────────
qc_report <- dplyr::bind_rows(qc_rows)

report_path <- physioQCReportFile
write.csv(qc_report, report_path, row.names = FALSE)

# ── Console summary ────────────────────────────────────────────
n_total   <- nrow(qc_report)
n_flagged <- sum(qc_report$flag, na.rm = TRUE)
n_clean   <- n_total - n_flagged

message(sprintf("\n── Physio QC Summary ───────────────────────────────"))
message(sprintf("  Total participants: %d", n_total))
message(sprintf("  Clean:             %d", n_clean))
message(sprintf("  Flagged:           %d", n_flagged))

if (n_flagged > 0) {
  message("\n  Flagged participants:")
  flagged <- qc_report[qc_report$flag == TRUE, c("id", "seat", "duration_min", "flag_reasons")]
  for (i in seq_len(nrow(flagged))) {
    message(sprintf("    %s (seat=%s, dur=%.1f min): %s",
                    flagged$id[i], flagged$seat[i],
                    flagged$duration_min[i], flagged$flag_reasons[i]))
  }
}

message(sprintf("\n  Report written to: %s", report_path))