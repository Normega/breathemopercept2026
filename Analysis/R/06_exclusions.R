# R/06_exclusions.R
# One exclusion step for every analysis frame (2026-10-05). Builds a
# participant table with each criterion (exclusions.csv), a sequential flow
# (participant_flow.csv), and the cleaned frames used downstream.
#
# Criteria, in flow order:
#   1. Data: a non-test participant ID with any task file or questionnaire
#      (test IDs, non-numeric IDs and pre-collection pilots are dropped in the
#      task-file index; IDs corrected per id_corrections.csv).
#   2. Questionnaire: a complete Qualtrics submission (Progress >= 96). It
#      carries the attention checks and every trait measure, so a participant
#      without one cannot be screened and is excluded.
#   3. Attention: both instructed-response items passed, on the FIRST complete
#      submission (later submissions are dropped in 01, not counted here).
#   4. BCAT technical failure: no baseline BCAT, or no thresholds from it.
#      Applies to BCAT and combined-task analyses (the combined task is
#      calibrated from these thresholds), not to baseline-GERT analyses.
#   5. Combined task: fewer than 6 of 12 blocks with GERT data.
# Baseline-GERT analyses additionally need a GERT baseline run.
# ---------------------------------------------------------------

message("06 | Applying exclusions...")

pData    <- read.csv(questionnaireFile,      stringsAsFactors = FALSE, colClasses = c(id = "character"))
bcatBase <- read.csv(BCAT_baseline_DataFile, stringsAsFactors = FALSE, colClasses = c(id = "character"))
gertBase <- read.csv(GERT_baseline_DataFile, stringsAsFactors = FALSE, colClasses = c(id = "character"))
combData <- read.csv(combined_DataFile,      stringsAsFactors = FALSE, colClasses = c(id = "character"))
combBCAT <- read.csv(combined_fullBCAT,      stringsAsFactors = FALSE, colClasses = c(id = "character"))
combGERT <- read.csv(combined_fullGERT,      stringsAsFactors = FALSE, colClasses = c(id = "character"))
taskIndex <- read.csv(taskFileIndexFile,     stringsAsFactors = FALSE, colClasses = c(id = "character", raw_id = "character"))

MIN_BLOCKS <- 6L

ids <- sort(unique(c(pData$id, taskIndex$id[!taskIndex$test])))
ex <- data.frame(id = ids, stringsAsFactors = FALSE)

q <- pData[match(ex$id, pData$id), ]
ex$has_questionnaire      <- !is.na(q$id)
ex$attention_pass         <- ex$has_questionnaire & q$attention_pass %in% TRUE
ex$n_complete_submissions <- ifelse(ex$has_questionnaire, q$n_complete_submissions, 0L)

thr <- bcatBase |>
  dplyr::group_by(id) |>
  dplyr::summarise(bcat_trials = dplyr::n(),
                   bcat_thresholds = any(!is.na(ACCthresh)) & any(!is.na(DECthresh)),
                   .groups = "drop")
ex$has_bcat        <- ex$id %in% thr$id
ex$bcat_thresholds <- ex$id %in% thr$id[thr$bcat_thresholds]
ex$bcat_technical  <- !ex$bcat_thresholds

g <- gertBase |> dplyr::group_by(id) |> dplyr::summarise(gert_clips = dplyr::n(), .groups = "drop")
ex$gert_clips <- g$gert_clips[match(ex$id, g$id)]; ex$gert_clips[is.na(ex$gert_clips)] <- 0L
ex$has_gert   <- ex$gert_clips > 0

blk <- combGERT |> dplyr::group_by(id) |>
  dplyr::summarise(combined_blocks = dplyr::n_distinct(Block),
                   combined_max_block = max(Block), .groups = "drop")
ex$combined_blocks <- blk$combined_blocks[match(ex$id, blk$id)]
ex$combined_blocks[is.na(ex$combined_blocks)] <- 0L
ex$combined_lt6    <- ex$combined_blocks < MIN_BLOCKS
ex$combined_over12 <- ex$id %in% blk$id[blk$combined_max_block > 12]

ex$corrected_id <- ex$id %in% taskIndex$id[nzchar(taskIndex$correction)]

# Analysis sets
ex$set_questionnaire <- ex$has_questionnaire & ex$attention_pass
ex$set_gert_baseline <- ex$set_questionnaire & ex$has_gert               # H1
ex$set_bcat_baseline <- ex$set_questionnaire & !ex$bcat_technical        # H2
ex$set_combined      <- ex$set_bcat_baseline & !ex$combined_lt6          # H3, H4, exploratory
write.csv(ex, exclusionsFile, row.names = FALSE)

# Sequential flow
flow_step <- function(label, keep, prev) data.frame(step = label, excluded = sum(prev & !keep),
                                               remaining = sum(prev & keep), stringsAsFactors = FALSE)
s0 <- rep(TRUE, nrow(ex))
f1 <- flow_step("Participants with data (non-test IDs)", s0, s0)
s1 <- ex$has_questionnaire;      f2 <- flow_step("No complete questionnaire", s1, s0)
s2 <- s1 & ex$attention_pass;    f3 <- flow_step("Failed attention check (first submission)", ex$attention_pass, s1)
f3b <- flow_step("  of whom have a GERT baseline run", ex$has_gert, s2)
s3 <- s2 & !ex$bcat_technical;   f4 <- flow_step("BCAT technical failure (no thresholds)", !ex$bcat_technical, s2)
s4 <- s3 & !ex$combined_lt6;     f5 <- flow_step("Fewer than 6 combined-task blocks", !ex$combined_lt6, s3)
flow <- rbind(f1, f2, f3, f3b, f4, f5)
flow$note <- c("", "", sprintf("%d participants had repeat complete submissions; the first was kept",
                               sum(ex$n_complete_submissions > 1)),
               "H1 sample (baseline GERT)", "H2 sample (BCAT thresholds)", "Combined-task sample (H3, H4)")
write.csv(flow, participantFlowFile, row.names = FALSE)
message("  Participant flow:")
for (k in seq_len(nrow(flow)))
  message(sprintf("    %-45s excluded %3d  remaining %3d  %s", flow$step[k], flow$excluded[k],
                  flow$remaining[k], flow$note[k]))
if (any(ex$combined_over12))
  message("  Note: combined task with > 12 blocks: ", paste(ex$id[ex$combined_over12], collapse = ", "))

# Cleaned frames (names kept for 07-10)
keep_q <- ex$id[ex$set_questionnaire]
pData_clean    <- pData[pData$id %in% keep_q, ]
gertBase_clean <- gertBase[gertBase$id %in% ex$id[ex$set_gert_baseline], ]
bcatBase_clean <- bcatBase[bcatBase$id %in% ex$id[ex$set_bcat_baseline], ]
combData_clean <- combData[combData$id %in% ex$id[ex$set_combined], ]
combBCAT_clean <- combBCAT[combBCAT$id %in% ex$id[ex$set_combined], ]
combGERT_clean <- combGERT[combGERT$id %in% ex$id[ex$set_combined], ]

message(sprintf("  Cleaned: questionnaire %d, GERT baseline %d, BCAT baseline %d, combined %d",
                length(unique(pData_clean$id)), length(unique(gertBase_clean$id)),
                length(unique(bcatBase_clean$id)), length(unique(combData_clean$id))))

assign("exclusions",     ex,             envir = .GlobalEnv)
assign("pData_clean",    pData_clean,    envir = .GlobalEnv)
assign("gertBase_clean", gertBase_clean, envir = .GlobalEnv)
assign("bcatBase_clean", bcatBase_clean, envir = .GlobalEnv)
assign("combData_clean", combData_clean, envir = .GlobalEnv)
assign("combBCAT_clean", combBCAT_clean, envir = .GlobalEnv)
assign("combGERT_clean", combGERT_clean, envir = .GlobalEnv)
