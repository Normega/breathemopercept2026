# R/05e_detect_r_peaks.R  (lab mode)
# R-peak detection and RR cleaning on each participant's chosen ECG lead.
# Writes physioRdsPath/<id>_cardiac.rds: beat times, relative R amplitude,
# polarity, and the RR series with artefact flags. The ECG waveform itself is
# not part of the de-identified dataset; these beat series are. Methods:
# Methods/cardiac_features.md.
# ---------------------------------------------------------------

message("05e | R-peak detection...")

source(file.path(analysisPath, "R", "physio_functions.R"))

files <- sort(list.files(physioRdsPath, "_physio[.]rds$", full.names = TRUE))
for (f in files) {
  px <- readRDS(f)
  pk <- if (is.null(px$card$signal)) numeric(0) else
    tryCatch(detect_r_peaks(px$card$signal, px$card$fs), error = function(e) numeric(0))
  rr <- clean_rr(pk)
  saveRDS(list(id = px$id, fs = px$card$fs, r_peaks = as.numeric(pk), r_amp_rel = attr(pk, "amp_rel"),
               polarity = attr(pk, "polarity"), rr = rr, channel = px$card$channel),
          file.path(physioRdsPath, paste0(px$id, "_cardiac.rds")))
}
message(sprintf("05e | R peaks for %d participants.", length(files)))
