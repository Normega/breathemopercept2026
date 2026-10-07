# config.R
# Centralized paths and study constants.
# Edit mainPath for your machine; all other paths derive from it.
# ---------------------------------------------------------------

# ── Two pipeline modes ─────────────────────────────────────────
# "lab" (main.R): raw, identifying data -> intermediate files in
#   Data/processed/ (private, never published) -> the de-identified dataset
#   (deidentify.R). Needs the raw Qualtrics export, PsychoPy files and .acq
#   recordings on the lab drive.
# "analysis" (run_analysis.R; the default): everything from the de-identified
#   dataset onward. It reads nothing identifying and writes aggregate outputs
#   to Repo/Results/. Anyone with the public data repository can run it.
if (!exists("PIPELINE_MODE")) PIPELINE_MODE <- "analysis"
stopifnot(PIPELINE_MODE %in% c("lab", "analysis"))

# ── Paths ──────────────────────────────────────────────────────
# Use file.path() throughout (not paste0): handles OS separators correctly
# and avoids double-slash bugs when components have trailing slashes.
analysisPath <- file.path(mainPath, "Repo", "Analysis")
resultsPath  <- file.path(mainPath, "Repo", "Results")   # analysis outputs (aggregate)
# The de-identified dataset: a clone of the public data repository
# (github.com/Normega/breathemopercept2026-data).
if (!exists("deidPath")) deidPath <- file.path(mainPath, "breathemopercept2026-data")

# Lab only: raw data, private intermediate files, and the key from study code
# back to SONA ID (needed to honour withdrawal requests; never published).
dataPath        <- file.path(mainPath, "Data")
rawTaskDataPath <- file.path(mainPath, "Task", "Data")
physioPath      <- file.path(dataPath, "Physio")            # folder of .acq files
privatePath     <- file.path(dataPath, "processed")
deidKeyFile     <- file.path(dataPath, "deid_key.csv")

if (PIPELINE_MODE == "lab") {
  procPath     <- privatePath
  taskDataPath <- rawTaskDataPath
  physioQCPath <- file.path(privatePath, "physio")
} else {
  procPath     <- resultsPath                    # de-identified inputs, copied in by run_analysis.R
  taskDataPath <- file.path(deidPath, "task")
  physioQCPath <- file.path(resultsPath, "physio")
}
questionnaireQCPath <- file.path(resultsPath, "questionnaires")
physioExtractPath   <- file.path(physioQCPath, "extract")  # one file per .acq seat (lab)
physioRdsPath       <- file.path(physioQCPath, "rds")      # one set per participant
physioExtractManifest <- file.path(physioQCPath, "extract_manifest.csv")
physioAssignmentFile  <- file.path(physioQCPath, "physio_assignment.csv")

# respkit 0.2.0, the respiration toolbox (github.com/Normega/respkit). An
# installed package is used if present, otherwise the source at RESPKIT_PATH.
if (!exists("RESPKIT_PATH")) RESPKIT_PATH <- file.path(dirname(mainPath), "TechnicalReference", "respkit")

# ── Raw input files (lab) ──────────────────────────────────────
qualtricsFile <- file.path(dataPath, "QualtricsMar5.xlsx")
# Documented participant-ID corrections for task files (evidence in the file)
idCorrectionsFile <- file.path(dataPath, "id_corrections.csv")   # private: contains participant-pool IDs
# Task files dated before this are pilot sessions
DATA_START <- as.Date("2025-11-01")

# ── Intermediate files ─────────────────────────────────────────
# Lab mode writes these with SONA IDs to Data/processed/; deidentify.R recodes
# them; analysis mode reads the de-identified copies.
questionnaireItemsFile <- file.path(procPath, "questionnaire_items.csv")
questionnaireFile      <- file.path(procPath, "questionnaireFile.csv")
BCAT_baseline_DataFile <- file.path(procPath, "BCAT_baseline_data.csv")
hrReportFile           <- file.path(procPath, "hr_data.csv")
GERT_baseline_DataFile <- file.path(procPath, "GERT_baseline_data.csv")
combined_DataFile      <- file.path(procPath, "Combined_data.csv")
combined_fullGERT      <- file.path(procPath, "Combined_fullGERT_data.csv")
combined_fullBCAT      <- file.path(procPath, "Combined_fullBCAT_data.csv")
taskFileIndexFile      <- file.path(procPath, "task_file_index.csv")
exclusionsFile         <- file.path(resultsPath, "exclusions.csv")
participantFlowFile    <- file.path(resultsPath, "participant_flow.csv")

# ── QC output files ────────────────────────────────────────────
scaleReliabilityFile   <- file.path(questionnaireQCPath, "scale_reliability.csv")
itemDescriptivesFile   <- file.path(questionnaireQCPath, "item_descriptives.csv")
scaleDescriptivesFile  <- file.path(questionnaireQCPath, "scale_descriptives.csv")

# ── Create subdirectories if they don't exist ──────────────────
for (d in c(questionnaireQCPath, physioQCPath, physioRdsPath,
            if (PIPELINE_MODE == "lab") c(privatePath, physioExtractPath))) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# ── Test / dummy IDs to exclude ────────────────────────────────
TEST_IDS <- c("99999", "99999.0", "1111", "1111.0", "2222", "2222.0", "0001")

# ── Physio processing constants ────────────────────────────────
PHYSIO_RAW_HZ  <- 2000L  # acquisition rate of .acq files
PHYSIO_RESP_HZ <- 25L    # respiration downsample target (signal bandwidth < 2 Hz)
PHYSIO_CARD_HZ <- 250L   # cardiac downsample target (QRS capture needs >= 150 Hz)
# Downsample factors must be integers
stopifnot(PHYSIO_RAW_HZ %% PHYSIO_RESP_HZ == 0L)
stopifnot(PHYSIO_RAW_HZ %% PHYSIO_CARD_HZ == 0L)

# ── Analysis constants ─────────────────────────────────────────
BH_ALPHA           <- 0.05   # familywise alpha after BH correction
AWARE_BLOCK_CUTOFF <- 2/3    # proportion correct to classify a block as "aware"