# R/04_prep_combined.R
# Loads combined BCAT-GERT task files and writes:
#   Combined_data.csv          (block-level summary)
#   Combined_fullGERT_data.csv (trial-level GERT)
#   Combined_fullBCAT_data.csv (trial-level BCAT)
# ---------------------------------------------------------------

message("04 | Combined task prep...")

taskIndex  <- task_file_index()
comb_files <- select_task_files(taskIndex, "combined", min_size = 72000)
comb_files$filename <- comb_files$file

message(sprintf("  Found %d unique combined task files", nrow(comb_files)))

bcat_vars <- c("OuterLoop.thisN", "changeDirection", "salience", "thisCondition",
               "confidenceSlider.response", "arousalSlider.response",
               "trial.started", "trial.stopped",
               "level", "Direction", "Response", "Correct", "Accuracy")

gert_vars <- c("OuterLoop.thisN", "gertLoop.thisN",
               "vidCorrect", "emoResponse", "emoAccuracy", "IntensityRating", "FileName")

longGERT <- data.frame()
longBCAT <- data.frame()

for (i in seq_len(nrow(comb_files))) {
  thisId <- comb_files$id[i]
  mydata <- tryCatch(
    read.csv(file.path(taskDataPath, comb_files$filename[i])),
    error = function(e) { warning("Combined: failed to read ", comb_files$filename[i]); NULL }
  )
  if (is.null(mydata)) next
  # Partial runs (kept so 06 can count their blocks) may stop before some
  # columns are ever written; take what exists.
  if ("level" %in% names(mydata)) {
    bcatData    <- mydata[!is.na(mydata$level), intersect(bcat_vars, names(mydata)), drop = FALSE]
    bcatData$id <- thisId
    longBCAT <- dplyr::bind_rows(longBCAT, bcatData)
  }
  if ("gertLoop.thisN" %in% names(mydata)) {
    gertData    <- mydata[!is.na(mydata$gertLoop.thisN), intersect(gert_vars, names(mydata)), drop = FALSE]
    gertData$id <- thisId
    longGERT <- dplyr::bind_rows(longGERT, gertData)
  }
}

names(longGERT)[names(longGERT) == "OuterLoop.thisN"]            <- "Block"
names(longGERT)[names(longGERT) == "gertLoop.thisN"]             <- "Trial"
names(longBCAT)[names(longBCAT) == "OuterLoop.thisN"]            <- "Block"
names(longBCAT)[names(longBCAT) == "salience"]                   <- "Salience"
names(longBCAT)[names(longBCAT) == "thisCondition"]              <- "Condition"
names(longBCAT)[names(longBCAT) == "changeDirection"]            <- "DirectionLabel"
names(longBCAT)[names(longBCAT) == "confidenceSlider.response"]  <- "Confidence"
names(longBCAT)[names(longBCAT) == "arousalSlider.response"]     <- "Arousal"

longBCAT$Change    <- longBCAT$level * longBCAT$Direction
longBCAT$Direction <- factor(longBCAT$Direction, labels = c("Faster", "NoChange", "Slower"))
longGERT$emo_distance <- circular_distance(longGERT$emoResponse, longGERT$vidCorrect)

# Convert 0-indexed block numbers to 1-indexed
longGERT$Block <- longGERT$Block + 1
longBCAT$Block <- longBCAT$Block + 1

gert4merge <- longGERT |>
  dplyr::group_by(id, Block) |>
  dplyr::summarise(
    emoAccuracy  = mean(emoAccuracy,     na.rm = TRUE),
    emoIntensity = mean(IntensityRating, na.rm = TRUE),
    emoDistance  = mean(emo_distance,    na.rm = TRUE),
    .groups = "drop"
  )

bcat4merge <- longBCAT |>
  dplyr::group_by(id, Block, Salience, Condition, DirectionLabel) |>
  dplyr::summarise(
    Confidence   = mean(Confidence, na.rm = TRUE),
    Arousal      = mean(Arousal,    na.rm = TRUE),
    bcatAccuracy = mean(Accuracy,   na.rm = TRUE),
    .groups = "drop"
  )

combinedData <- dplyr::left_join(gert4merge, bcat4merge, by = c("id", "Block"))
combinedData <- combinedData[!is.na(combinedData$Block), ]

message(sprintf("  Combined blocks: %d | Participants: %d",
                nrow(combinedData), length(unique(combinedData$id))))

write.csv(combinedData, combined_DataFile, row.names = FALSE)
write.csv(longBCAT,     combined_fullBCAT, row.names = FALSE)
write.csv(longGERT,     combined_fullGERT, row.names = FALSE)
message("  -> Written: Combined_data.csv, Combined_fullBCAT_data.csv, Combined_fullGERT_data.csv")
