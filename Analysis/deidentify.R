# deidentify.R  (lab mode)
# Builds the de-identified dataset, the only input to the analysis pipeline
# (run_analysis.R), from the private intermediate files in Data/processed/.
#
# What is removed or recoded:
#   - SONA IDs -> random study codes (P001...). The key (deid_key.csv) stays on
#     the lab drive so withdrawal requests can still be honoured; it is never
#     published. Codes are stable across reruns: existing assignments are kept.
#   - Dates and clock times: questionnaire start dates, task-file dates and
#     wall-clock fields (PsychoPy `date`, `expStart`), recording start times.
#     Times within a task or recording are kept, relative to its own start.
#   - File names that carry IDs or dates (.acq names, PsychoPy file names) are
#     replaced by <code>_<task>_run<k>.csv.
#   - PsychoPy `participant` fields (one file stored an e-mail address there).
#   - Demographics are coarsened: age is top-coded at 25 ("25 or older"; 4
#     participants), and gender is coded Female, Male, or "Other or not
#     reported" (11 participants). No other demographic, socioeconomic or
#     free-text field is released.
#   - The ECG waveform is replaced by its beat series (R-peak times, relative
#     R amplitude, RR intervals with artefact flags; from 05e). Respiration
#     belts are released at 25 Hz.
#
# Output (deidPath): data/*.csv, task/*.csv, physio/<code>_physio.rds and
# physio/<code>_cardiac.rds, plus manifest.csv with a checksum for every file.
# ---------------------------------------------------------------

message("deidentify | Building the de-identified dataset...")
stopifnot(PIPELINE_MODE == "lab")
out_data   <- file.path(deidPath, "data")
out_task   <- file.path(deidPath, "task")
out_physio <- file.path(deidPath, "physio")
for (d in c(out_data, out_task, out_physio)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

rdc <- function(f) read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, colClasses = c(id = "character"))

# ── Study codes ───────────────────────────────────────────────
items  <- rdc(questionnaireItemsFile)
tindex <- read.csv(taskFileIndexFile, stringsAsFactors = FALSE, colClasses = c(id = "character", raw_id = "character"))
tindex <- tindex[!tindex$test, ]
physio_ids <- sub("_physio[.]rds$", "", list.files(physioRdsPath, "_physio[.]rds$"))
inter <- c(BCAT_baseline_DataFile, GERT_baseline_DataFile, combined_DataFile, combined_fullBCAT,
           combined_fullGERT, hrReportFile)
ids <- sort(unique(c(items$id, tindex$id, physio_ids, unlist(lapply(inter, function(f) rdc(f)$id)))))
ids <- ids[!is.na(ids) & nzchar(ids)]

key <- if (file.exists(deidKeyFile)) read.csv(deidKeyFile, stringsAsFactors = FALSE, colClasses = "character") else
  data.frame(id = character(0), code = character(0), stringsAsFactors = FALSE)
new_ids <- setdiff(ids, key$id)
if (length(new_ids)) {
  used <- as.integer(sub("^P", "", key$code))
  pool <- setdiff(seq_len(max(999L, length(ids) * 3L)), used)
  set.seed(NULL)                                   # codes must not be reproducible from the IDs
  key <- rbind(key, data.frame(id = new_ids, code = sprintf("P%03d", sample(pool, length(new_ids))),
                               stringsAsFactors = FALSE))
  write.csv(key, deidKeyFile, row.names = FALSE)
}
code_of <- function(x) { x <- as.character(x); out <- key$code[match(x, key$id)]; out[is.na(x)] <- NA; out }
stopifnot(!anyNA(code_of(ids)))

# ── Questionnaires ────────────────────────────────────────────
items$id <- code_of(items$id)
items$Age <- pmin(items$Age, 25)
items$Gender <- ifelse(items$Gender %in% c("Female", "Male"), items$Gender, "Other or not reported")
write.csv(items, file.path(out_data, "questionnaire_items.csv"), row.names = FALSE)

# ── Task-level intermediate files (02-04) ─────────────────────
for (f in inter) {
  d <- rdc(f); d$id <- code_of(d$id)
  write.csv(d, file.path(out_data, basename(f)), row.names = FALSE)
}

# ── Task files used by the physiology steps ───────────────────
TASK_LABEL <- c(combined = "CombinedTask", bcat_baseline = "Intero2025", gert_baseline = "GERT_baseline")
DROP_COLS  <- c("participant", "Participant", "date", "expStart", "session")
task_map <- data.frame(src = character(0), dst = character(0), stringsAsFactors = FALSE)
add_task <- function(src, code, task) {
  if (src %in% task_map$src) return(task_map$dst[task_map$src == src])
  k <- sum(grepl(sprintf("^%s_%s_run", code, TASK_LABEL[[task]]), task_map$dst)) + 1L
  dst <- sprintf("%s_%s_run%d.csv", code, TASK_LABEL[[task]], k)
  d <- read.csv(file.path(rawTaskDataPath, src), stringsAsFactors = FALSE, check.names = FALSE,
                colClasses = "character", na.strings = character(0))
  d <- d[, !(names(d) %in% DROP_COLS), drop = FALSE]
  write.csv(d, file.path(out_task, dst), row.names = FALSE)
  task_map <<- rbind(task_map, data.frame(src = src, dst = dst, stringsAsFactors = FALSE))
  dst
}

# ── Physiology ────────────────────────────────────────────────
clean_rec <- function(r, code) {
  if (is.null(r)) return(NULL)
  r$id <- code
  r$meta$source_file <- NULL; r$meta$start_time <- NULL
  r$provenance <- sub("^read_acq\\([^,]*, ", "read_acq(", r$provenance)
  r$provenance <- sub(", started .*$", "", r$provenance)
  r
}
for (pid in physio_ids) {
  px <- readRDS(file.path(physioRdsPath, paste0(pid, "_physio.rds")))
  code <- code_of(pid)
  tc <- px$task_clock
  tc$id <- code
  tc$task_file <- vapply(seq_len(nrow(tc)), function(k) add_task(tc$task_file[k], code, tc$task[k]), character(1))
  tc$extract <- NULL; tc$source_file <- NULL
  own <- if (!is.null(px$resp_own)) px$resp_own else px$resp
  out <- list(schema = "aya_physio_deid_v1", id = code, seat = px$seat, method = px$method,
              heart_source = px$heart_source,
              resp_own = clean_rec(own, code), resp_alt = clean_rec(px$resp_alt, code),
              alt_occupied_by = if (is.null(px$alt_occupied_by) || is.na(px$alt_occupied_by)) NA_character_
                                else code_of(px$alt_occupied_by),
              events = px$events, task_clock = tc, checks = px$checks, respkit_version = px$respkit_version)
  saveRDS(out, file.path(out_physio, paste0(code, "_physio.rds")), compress = "xz")
  cf <- file.path(physioRdsPath, paste0(pid, "_cardiac.rds"))
  if (file.exists(cf)) {
    cr <- readRDS(cf); cr$id <- code
    saveRDS(cr, file.path(out_physio, paste0(code, "_cardiac.rds")), compress = "xz")
  }
}

# GERT baseline file per participant, chosen as 05d chooses it (earliest file
# over 30 KB)
gert_files <- list.files(rawTaskDataPath, pattern = "_GERT_baseline_.*[.]csv$")
gert_files <- sort(gert_files[file.info(file.path(rawTaskDataPath, gert_files))$size > 30000])
gert_pref <- sub("^([^_]+)_.*$", "\\1", gert_files)
for (pid in intersect(physio_ids, gert_pref)) add_task(gert_files[gert_pref == pid][1], code_of(pid), "gert_baseline")

# Task file index (for 06): one row per non-test task file
tindex_out <- data.frame(file = NA_character_, task = tindex$task, raw_id = code_of(tindex$id),
                         id = code_of(tindex$id), correction = ifelse(!is.na(tindex$correction) & nzchar(tindex$correction), "corrected", ""),
                         test = FALSE, stringsAsFactors = FALSE)
tindex_out$file <- task_map$dst[match(tindex$file, task_map$src)]
write.csv(tindex_out, file.path(out_data, "task_file_index.csv"), row.names = FALSE)

# ── Checks and manifest ───────────────────────────────────────
# No SONA ID, e-mail address or date may remain in any text output.
txt <- list.files(c(out_data, out_task), full.names = TRUE)
pat_id <- paste0("\\b(", paste(ids[nchar(ids) >= 4], collapse = "|"), ")\\b")
for (f in txt) {
  l <- readLines(f, warn = FALSE)
  bad <- c(any(grepl("@", l, fixed = TRUE)), any(grepl("20(25|26)-[01][0-9]-[0-3][0-9]", l)))
  if (any(bad)) stop("De-identification check failed (e-mail or date) in ", basename(f))
}
hdr_ok <- vapply(txt, function(f) !any(grepl(pat_id, readLines(f, n = 1), perl = TRUE)), logical(1))
if (!all(hdr_ok)) stop("A SONA ID appears in a header: ", paste(basename(txt[!hdr_ok]), collapse = ", "))
all_files <- list.files(deidPath, recursive = TRUE, full.names = TRUE)
all_files <- all_files[!grepl("[.]git/", all_files)]
man <- data.frame(file = sub(paste0("^", deidPath, "/?"), "", all_files),
                  bytes = file.info(all_files)$size, md5 = unname(tools::md5sum(all_files)))
write.csv(man[man$file != "manifest.csv", ], file.path(deidPath, "manifest.csv"), row.names = FALSE)
message(sprintf("deidentify | %d participants, %d task files, %d physio files -> %s",
                length(ids), nrow(task_map), length(list.files(out_physio)), deidPath))
