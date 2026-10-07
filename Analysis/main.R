# main.R  (lab)
# Full pipeline on the lab drive: raw, identifying data -> private
# intermediate files -> the de-identified dataset -> every analysis, run from
# the de-identified dataset only (run_analysis.R).
#
# Anyone without the raw data starts at run_analysis.R with a clone of the
# public data repository.
#
# ── Setup: edit mainPath for your machine, then run this file ──
mainPath <- "I:/Shared drives/Aya/"
PIPELINE_MODE <- "lab"
analysisPath <- file.path(mainPath, "Repo", "Analysis")
source(file.path(analysisPath, "config.R"))
source(file.path(analysisPath, "R", "00_packages.R"))
reticulate::py_require(c("bioread", "numpy"))  # .acq reader, must precede first Python use
source(file.path(analysisPath, "functions.R"))

# RAW -> PRIVATE INTERMEDIATE FILES (Data/processed/) ------------
source(file.path(analysisPath, "R", "01_prep_questionnaires.R"))   # item-level responses
source(file.path(analysisPath, "R", "02_prep_bcat_baseline.R"))
source(file.path(analysisPath, "R", "03_prep_gert_baseline.R"))
source(file.path(analysisPath, "R", "04_prep_combined.R"))

# 05a: .acq -> per-seat extracts (slow; skips files already extracted)
# 05b: identity + clock alignment from trigger timing -> <id>_physio.rds
# 05e: R-peak detection on the chosen ECG lead -> <id>_cardiac.rds
source(file.path(analysisPath, "R", "physio_functions.R"))
source(file.path(analysisPath, "R", "05a_extract_physio.R"))
source(file.path(analysisPath, "R", "05b_assign_physio.R"))
source(file.path(analysisPath, "R", "05e_detect_r_peaks.R"))

# PRIVATE -> DE-IDENTIFIED DATASET (deidPath) --------------------
source(file.path(analysisPath, "deidentify.R"))

# DE-IDENTIFIED DATASET -> EVERY ANALYSIS --------------------------
source(file.path(analysisPath, "run_analysis.R"))
