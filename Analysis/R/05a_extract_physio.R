# R/05a_extract_physio.R
# Reads every BIOPAC .acq once and writes one extract per seat to
# physioExtractPath (<file stem>__<L|R>.rds), plus extract_manifest.csv.
#
# This step makes no claim about who sat where: it applies the hardware seat
# map in physio_functions.R and leaves identity to 05b, which checks it against
# the PsychoPy logs. It is the slow step (~30 s per file, ~230 files); re-runs
# skip files whose extracts already exist.
#
# Parallel use: set PHYSIO_WORKER = "k/n" (e.g. "1/4") in the environment to
# process every n-th file starting at k; each worker writes its own manifest
# part, and the parts are combined at the end of a plain (unsliced) run or by
# 05b.
# ---------------------------------------------------------------

message("05a | Physio extraction...")

reticulate::py_require(c("bioread", "numpy"))
load_respkit()
source(file.path(analysisPath, "R", "physio_functions.R"))

acq_files <- sort(list.files(physioPath, pattern = "\\.acq$",
                             full.names = TRUE, ignore.case = TRUE))
acq_files <- acq_files[!grepl("Template", basename(acq_files), ignore.case = TRUE)]

worker <- Sys.getenv("PHYSIO_WORKER", "")
part_tag <- "all"
if (nzchar(worker)) {
  kn <- as.integer(strsplit(worker, "/")[[1]])
  acq_files <- acq_files[seq(kn[1], length(acq_files), by = kn[2])]
  part_tag <- paste0(kn[1], "of", kn[2])
}
message(sprintf("  %d .acq file(s) to check (worker %s)", length(acq_files), part_tag))

part_file <- file.path(physioQCPath, paste0("extract_manifest_part_", part_tag, ".csv"))
for (f in acq_files) {
  t0 <- Sys.time()
  rows <- tryCatch(extract_acq(f, physioExtractPath),
                   error = function(e) {
                     message("  [ERROR] ", basename(f), ": ", conditionMessage(e))
                     data.frame(source_file = basename(f), seat = NA,
                                error = conditionMessage(e))
                   })
  if (is.null(rows)) next   # already extracted
  write.table(rows, part_file, sep = ",", row.names = FALSE,
              col.names = !file.exists(part_file), append = file.exists(part_file))
  message(sprintf("  %s  %.0f s", basename(f),
                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

if (!nzchar(worker)) {
  parts <- list.files(physioQCPath, "^extract_manifest_part_.*[.]csv$", full.names = TRUE)
  man <- dplyr::bind_rows(lapply(parts, read.csv, stringsAsFactors = FALSE))
  if (file.exists(physioExtractManifest))
    man <- dplyr::bind_rows(read.csv(physioExtractManifest, stringsAsFactors = FALSE), man)
  man <- man[!duplicated(man[c("source_file", "seat")], fromLast = TRUE), ]
  write.csv(man, physioExtractManifest, row.names = FALSE)
  file.remove(parts)
  message(sprintf("  -> %s (%d seat rows)", basename(physioExtractManifest), nrow(man)))
}
message("05a | Physio extraction complete.")
