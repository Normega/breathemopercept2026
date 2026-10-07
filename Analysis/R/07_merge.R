# R/07_merge.R
# Joins all cleaned sources into analysis-ready data frames.
#
# Outputs (global environment):
#   baseGERTwithQ_data  GERT baseline + questionnaires
#   bothBaseQ_data      both baselines + questionnaires
#   fullData            combined task (block-level) + trait data
#   traitData           1 row per participant, all trait measures
# ---------------------------------------------------------------

message("07 | Merging...")

# Ensure id columns are character throughout
pData_clean$id              <- as.character(pData_clean$id)
gertBase_clean$id           <- as.character(gertBase_clean$id)
bcatBase_clean$id           <- as.character(bcatBase_clean$id)
combData_clean$id           <- as.character(combData_clean$id)

# Participant-level summaries
baselineGERT_1perID <- gertBase_clean |>
  dplyr::group_by(id) |>
  dplyr::summarise(
    basegertAccuracy  = mean(emoAccuracy,     na.rm = TRUE),
    basegertDistance  = mean(emo_distance,    na.rm = TRUE),
    basegertIntensity = mean(IntensityRating, na.rm = TRUE),
    .groups = "drop"
  )

baselineBCAT_1perID <- bcatBase_clean |>
  dplyr::group_by(id) |>
  dplyr::summarise(
    ACCthresh        = mean(ACCthresh,  na.rm = TRUE),
    DECthresh        = mean(DECthresh,  na.rm = TRUE),
    baseConfidence   = mean(Confidence, na.rm = TRUE),
    baseArousal      = mean(Arousal,    na.rm = TRUE),
    baseBCATaccuracy = mean(Accuracy,   na.rm = TRUE),
    .groups = "drop"
  )

combined_1perID <- combData_clean |>
  dplyr::group_by(id) |>
  dplyr::summarise(
    combinedEmoAccuracy  = mean(emoAccuracy,  na.rm = TRUE),
    combinedIntensity    = mean(emoIntensity, na.rm = TRUE),
    combinedDistance     = mean(emoDistance,  na.rm = TRUE),
    combinedConfidence   = mean(Confidence,   na.rm = TRUE),
    combinedArousal      = mean(Arousal,      na.rm = TRUE),
    combinedBCATAccuracy = mean(bcatAccuracy, na.rm = TRUE),
    .groups = "drop"
  )

# Build analysis frames
baseGERTwithQ_data <- dplyr::left_join(pData_clean, baselineGERT_1perID, by = "id")

bothBaseQ_data <- dplyr::left_join(baseGERTwithQ_data, baselineBCAT_1perID, by = "id")
bothBaseQ_data$baseThresh <- (bothBaseQ_data$ACCthresh + bothBaseQ_data$DECthresh) / 2

fullData  <- dplyr::left_join(combData_clean, bothBaseQ_data, by = "id")
traitData <- dplyr::left_join(combined_1perID, bothBaseQ_data, by = "id")

message(sprintf("  baseGERTwithQ_data N: %d", nrow(baseGERTwithQ_data)))
message(sprintf("  bothBaseQ_data N: %d",     nrow(bothBaseQ_data)))
message(sprintf("  fullData rows: %d (%d participants)",
                nrow(fullData), length(unique(fullData$id))))
message(sprintf("  traitData N: %d",          nrow(traitData)))
message("07 | Merge complete.")
