# R/02_prep_bcat_baseline.R
# Loads baseline BCAT files, extracts trial data and heartbeat
# counting reports. Writes BCAT_baseline_data.csv and hr_data.csv.
# ---------------------------------------------------------------

message("02 | BCAT baseline prep...")

taskIndex <- task_file_index()
# The written index must not carry personal data: one file was saved under an
# e-mail address (corrected in id_corrections.csv).
idx_out <- taskIndex
redact  <- grepl("@", idx_out$raw_id, fixed = TRUE)
idx_out$raw_id[redact] <- "<e-mail redacted>"
idx_out$file[redact]   <- sub("^[^_]+", "<e-mail redacted>", idx_out$file[redact])
write.csv(idx_out, taskFileIndexFile, row.names = FALSE)
bcat_files <- select_task_files(taskIndex, "bcat_baseline", min_size = 20000)
bcat_files$filename <- bcat_files$file

message(sprintf("  Found %d unique BCAT baseline files", nrow(bcat_files)))

trial_vars <- c("trials.label", "trials.thisRepN", "level",
                "Direction", "DirectionLabel", "Correct", "Accuracy",
                "Response", "ArousalRating", "confidenceSlider.response",
                "trial.started", "trial.stopped")

longData <- data.frame()
hrRating <- data.frame()

for (i in seq_len(nrow(bcat_files))) {
  thisId <- bcat_files$id[i]
  mydata <- tryCatch(
    read.csv(file.path(taskDataPath, bcat_files$filename[i])),
    error = function(e) { warning("BCAT: failed to read ", bcat_files$filename[i]); NULL }
  )
  if (is.null(mydata)) next

  if (!"trials.thisN" %in% names(mydata)) next   # stopped before the first trial
  # A partial run (kept so 06 can see how far it got) may lack the threshold
  # or heartbeat-counting columns entirely.
  thresh  <- if (all(c("ACCthresh", "DECthresh") %in% names(mydata)))
    mydata[!is.na(mydata$ACCthresh), c("ACCthresh", "DECthresh")] else data.frame()
  trials  <- mydata[!is.na(mydata$trials.thisN), intersect(trial_vars, names(mydata)), drop = FALSE]
  trials$id       <- thisId
  trials$complete_run <- bcat_files$complete_run[i]
  trials$ACCthresh <- if (nrow(thresh) > 0) thresh$ACCthresh[1] else NA
  trials$DECthresh <- if (nrow(thresh) > 0) thresh$DECthresh[1] else NA

  hr_count <- function(cond) {
    if (!all(c("heartLoop.thisN", "thisCondition", "HRreport") %in% names(mydata))) return(NA)
    h <- mydata[!is.na(mydata$heartLoop.thisN) & mydata$thisCondition == cond, "HRreport"]
    if (length(h)) h[1] else NA
  }
  hrRow  <- data.frame(id = thisId, short25Count = hr_count("short"),
                       medium35Count = hr_count("medium"), long55Count = hr_count("long"))

  longData <- dplyr::bind_rows(longData, trials)
  hrRating <- dplyr::bind_rows(hrRating, hrRow)
}

names(longData)[names(longData) == "trials.label"]              <- "taskCondition"
names(longData)[names(longData) == "ArousalRating"]             <- "Arousal"
names(longData)[names(longData) == "trials.thisRepN"]           <- "Trial"
names(longData)[names(longData) == "confidenceSlider.response"] <- "Confidence"

longData$Change    <- longData$level * longData$Direction
longData$Direction <- factor(longData$Direction, labels = c("Faster", "NoChange", "Slower"))

message(sprintf("  Rows: %d | Participants: %d",
                nrow(longData), length(unique(longData$id))))

write.csv(longData, BCAT_baseline_DataFile, row.names = FALSE)
write.csv(hrRating, hrReportFile,           row.names = FALSE)
message("  -> Written: BCAT_baseline_data.csv, hr_data.csv")
