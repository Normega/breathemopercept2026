# run_analysis.R
# Every analysis in the manuscript, from the de-identified dataset only.
#
# Needs: this repository, a clone of the data repository
# (github.com/Normega/breathemopercept2026-data) at deidPath, and respkit 0.2.0
# (github.com/Normega/respkit, tag v0.2.0) at RESPKIT_PATH. By default both sit
# next to Repo/ under mainPath; set deidPath / RESPKIT_PATH before sourcing to
# put them elsewhere. Outputs go to Repo/Results/.
# ---------------------------------------------------------------

if (!exists("mainPath")) mainPath <- "I:/Shared drives/Aya/"
PIPELINE_MODE <- "analysis"
analysisPath <- file.path(mainPath, "Repo", "Analysis")
source(file.path(analysisPath, "config.R"))
source(file.path(analysisPath, "R", "00_packages.R"))
source(file.path(analysisPath, "functions.R"))
if (!dir.exists(deidPath)) stop("De-identified dataset not found at ", deidPath, call. = FALSE)

# Inputs: the de-identified tables into Results/ (read there by every
# script), and a working copy of the physiology files (05c updates it).
message("run_analysis | Copying the de-identified inputs...")
file.copy(list.files(file.path(deidPath, "data"), full.names = TRUE), resultsPath, overwrite = TRUE)
file.copy(list.files(file.path(deidPath, "physio"), full.names = TRUE), physioRdsPath, overwrite = TRUE)

# Questionnaire scoring, physiology features, exclusions, confirmatory tests
source(file.path(analysisPath, "R", "01b_score_questionnaires.R"))
source(file.path(analysisPath, "R", "physio_functions.R"))
source(file.path(analysisPath, "R", "05c_breath_trials.R"))
source(file.path(analysisPath, "R", "05d_cardiac.R"))
source(file.path(analysisPath, "R", "06_exclusions.R"))
source(file.path(analysisPath, "R", "07_merge.R"))
source(file.path(analysisPath, "R", "08_hypotheses.R"))
source(file.path(analysisPath, "confirmatory_rerun.R"))

# Exploratory analyses and figures, each writing to Results/<name>/
for (f in c("results_numbers.R", "breath_manipulation_check.R", "cardiac_analysis.R", "body_vs_awareness.R",
            "h2b_robustness.R", "burnout_entrainment.R", "clip_level_models.R", "equivalence_tests.R",
            "random_slopes.R", "make_figures.R")) {
  message("run_analysis | ", f)
  source(file.path(analysisPath, f), local = new.env(parent = globalenv()))
}
message("run_analysis | Complete.")
