# R/03_prep_gert_baseline.R
# Loads baseline GERT files, computes emotion distance,
# and writes GERT_baseline_data.csv.
# ---------------------------------------------------------------

message("03 | GERT baseline prep...")

taskIndex  <- task_file_index()
gert_files <- select_task_files(taskIndex, "gert_baseline", min_size = 30000)
gert_files$filename <- gert_files$file

message(sprintf("  Found %d unique GERT baseline files", nrow(gert_files)))

all_data     <- list()
failed_files <- character(0)

for (i in seq_len(nrow(gert_files))) {
  result <- tryCatch({
    thisData <- read.csv(file.path(taskDataPath, gert_files$filename[i]))
    thisData |>
      dplyr::select(trials.thisN,
                    vidCorrect, emoResponse, emoAccuracy,
                    IntensityRating, FileName) |>
      dplyr::filter(!is.na(vidCorrect) & vidCorrect != "") |>
      dplyr::mutate(emo_distance = circular_distance(emoResponse, vidCorrect),
                    # the ID from the file index (after id_corrections.csv),
                    # not the one typed into PsychoPy
                    id = gert_files$id[i],
                    complete_run = gert_files$complete_run[i])
  }, error = function(e) {
    warning("GERT: failed for ", gert_files$filename[i], ": ", e$message)
    failed_files <<- c(failed_files, gert_files$filename[i])
    NULL
  })
  if (!is.null(result)) all_data[[i]] <- result
}

allData_GERT <- dplyr::bind_rows(all_data)
allData_GERT <- allData_GERT[, c("id", setdiff(names(allData_GERT), "id"))]

if (length(failed_files) > 0)
  message("  Failed files: ", paste(failed_files, collapse = ", "))

message(sprintf("  Rows: %d | Participants: %d",
                nrow(allData_GERT), length(unique(allData_GERT$id))))

write.csv(allData_GERT, GERT_baseline_DataFile, row.names = FALSE)
message("  -> Written: GERT_baseline_data.csv")
