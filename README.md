# breathemopercept2026

Analysis code for a preregistered laboratory study of breath awareness, felt arousal and emotion perception
in university students (preregistration: [osf.io/wz32n](https://osf.io/wz32n), registered 2026-01-20;
data collected November 2025 to February 2026).

Participants detected externally paced changes in their breathing (Breath Change Awareness Task, BCAT) and
then rated others' emotional expressions (Geneva Emotion Recognition Test, GERT), with respiration and ECG
recorded throughout. Burnout (BAT-C) and interoceptive self-report (MAIA-2) were measured by questionnaire.

## Reproducing the analyses

Every result, table and figure is regenerated from the de-identified dataset alone:

1. Clone this repository as `Repo/`, the data repository
   [breathemopercept2026-data](https://github.com/Normega/breathemopercept2026-data) as
   `breathemopercept2026-data/` next to it, and [respkit](https://github.com/Normega/respkit) at tag `v0.2.0`.
2. In R:

   ```r
   mainPath <- "path/to/folder/"     # holds Repo/ and breathemopercept2026-data/
   RESPKIT_PATH <- "path/to/respkit"
   source(file.path(mainPath, "Repo", "Analysis", "run_analysis.R"))
   ```

`run_analysis.R` reads nothing identifying. It scores the questionnaires, computes the breathing and cardiac features, applies the
exclusions, runs the confirmatory and exploratory analyses, and draws the figures into `Results/`.

Requirements: R >= 4.5 with the packages in `Analysis/R/00_packages.R`, and respkit 0.2.0.

## Pipeline

```
LAB ONLY (raw, identifying data; Analysis/main.R, PIPELINE_MODE = "lab")
  R/01-04          Qualtrics and PsychoPy files -> intermediate tables     (Data/processed/, private)
  R/05a, 05b       .acq recordings -> per-participant physiology, identity and task clocks
  R/05e            R-peak detection on the ECG
  deidentify.R     -> the de-identified dataset (study codes; no dates, file names or ECG waveform;
                      coarsened demographics), checked for residual identifiers

PUBLIC (de-identified data only; Analysis/run_analysis.R, PIPELINE_MODE = "analysis")
  R/01b            questionnaire scoring, reliability, descriptives
  R/05c, 05d       breathing (belt choice, entrainment, breathing change) and cardiac features
  R/06-08          exclusions, merge, preregistered hypotheses; confirmatory_rerun.R
  *.R              exploratory analyses, each writing to Results/<name>/
  make_figures.R   manuscript Figures 1-6 and captions
```

`config.R` sets every path for both modes. `Methods/` documents each processing decision, and `docs/`
holds status notes and hand-offs. `R/retired/` and `retired/` hold superseded scripts.

## Data

No participant-level data are in this repository. Raw data (Qualtrics export, PsychoPy files, BIOPAC
`.acq` recordings), the private intermediate files and the key from study code to participant-pool ID stay
on the lab drive. The de-identified dataset is in
[breathemopercept2026-data](https://github.com/Normega/breathemopercept2026-data). Documentation refers to
participants by study code (P001...).
