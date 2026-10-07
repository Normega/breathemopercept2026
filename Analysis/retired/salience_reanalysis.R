# salience_reanalysis.R
# Secondary analysis of the preregistered pilot (osf.io/wz32n):
# does randomly assigned breathing-change SALIENCE raise perceived emotion
# intensity on the GERT clips that follow, and does that run through detection?
# Spec: docs/HANDOFF_salience_reanalysis.md
# Outputs: Results/salience_reanalysis/
# ---------------------------------------------------------------

# Set Up ---------
## Load libraries ---------
packages <- c("tidyverse", "lme4", "lmerTest", "MASS", "readxl")
new_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
if (length(new_packages)) install.packages(new_packages)
options(readr.show_col_types = FALSE)
for (thispack in packages) {
  library(thispack, character.only = TRUE, quietly = TRUE, verbose = FALSE)
}

## Paths and shared code ---------
mainPath <- "I:/Shared drives/aya"
analysisPath <- file.path(mainPath, "Repo", "Analysis")
source(file.path(analysisPath, "config.R"))
source(file.path(analysisPath, "functions.R"))

outPath <- file.path(resultsPath, "salience_reanalysis")
if (!dir.exists(outPath)) dir.create(outPath, recursive = TRUE)

# Existing exclusion module: reads the cleaned CSVs and applies the
# <6-combined-blocks rule. Produces combData_clean, combBCAT_clean, combGERT_clean.
source(file.path(analysisPath, "R", "06_exclusions.R"))

# Exclusions ---------
# Duplicates: 04_prep_combined.R already keeps each participant's earliest
# complete (>72 KB) combined-task file, so the task data hold one file per id.
# Attention: same rule as 01_prep_questionnaires.R (BIPS_4 == 4 and Catch1 == 4),
# evaluated on each participant's FIRST complete (Progress >= 96) submission.
# Per Norm (2026-10-01), only clearly inattentive participants are excluded;
# BCAT baseline technical failures are not excluded (no baseline data are used here).
norm_id <- function(x) sub("\\.0$", "", as.character(x))

qRaw <- readxl::read_xlsx(qualtricsFile)[-1, ]
qRaw$Progress <- as.numeric(qRaw$Progress)
qFirst <- qRaw |>
  dplyr::filter(Progress >= 96) |>
  dplyr::mutate(id = norm_id(Sona1), RecordedDate = as.character(RecordedDate)) |>
  dplyr::filter(!(id %in% norm_id(TEST_IDS))) |>
  dplyr::arrange(RecordedDate) |>
  dplyr::distinct(id, .keep_all = TRUE)
attention_fail_ids <- qFirst$id[!(as.numeric(qFirst$BIPS_4) == 4 & qFirst$Catch1 == "4.0")]
n_dup_qualtrics <- sum(duplicated(norm_id(qRaw$Sona1[qRaw$Progress >= 96])))

# One task file was saved with an e-mail address in place of a participant
# number (the id regex in 04 falls through). Pseudonymise it so it never
# reaches an output file.
pseudonymise <- function(x) ifelse(grepl("@", x), "unnumbered_01", as.character(x))
combBCAT_clean$id <- pseudonymise(combBCAT_clean$id)
combGERT_clean$id <- pseudonymise(combGERT_clean$id)
combData_clean$id <- pseudonymise(combData_clean$id)

ids_start <- unique(as.character(combData_clean$id))
ids_att   <- intersect(ids_start, attention_fail_ids)
ids_noQ   <- setdiff(ids_start, unique(qRaw$Sona1 |> norm_id()))
keep_ids  <- setdiff(ids_start, ids_att)

# Data ---------
## BCAT trial level ---------
bcat <- combBCAT_clean |>
  dplyr::mutate(id = as.character(id)) |>
  dplyr::filter(id %in% keep_ids, !is.na(Salience), !is.na(DirectionLabel)) |>
  dplyr::mutate(
    sal    = dplyr::if_else(Salience == "High", 0.5, -0.5),
    dir    = dplyr::if_else(DirectionLabel == "Acc", 0.5, -0.5),
    change = Direction != "NoChange"
  )

## Block level ---------
# Each block = 2 change trials + 1 no-change catch trial, all sharing one
# salience x direction condition.
#   det_block = proportion of the 2 CHANGE trials detected (primary detection)
#   acc3      = proportion of all 3 trials correct (thesis definition)
#   aware     = acc3 >= 2/3 (thesis AWARE_BLOCK_CUTOFF)
blocks <- bcat |>
  dplyr::group_by(id, Block, Salience, DirectionLabel, sal, dir) |>
  dplyr::summarise(
    n_bcat    = dplyr::n(),
    det_block = mean(Accuracy[change], na.rm = TRUE),
    acc3      = mean(Accuracy, na.rm = TRUE),
    arousal   = mean(Arousal, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  dplyr::filter(!is.nan(det_block)) |>
  dplyr::mutate(aware = as.numeric(acc3 >= AWARE_BLOCK_CUTOFF)) |>
  dplyr::group_by(id) |>
  dplyr::mutate(
    det_pm     = mean(det_block),
    det_pc     = det_block - det_pm,
    acc3_pm    = mean(acc3),
    acc3_pc    = acc3 - acc3_pm,
    arousal_pm = mean(arousal, na.rm = TRUE),
    arousal_pc = arousal - arousal_pm
  ) |>
  dplyr::ungroup()

## GERT trial level ---------
gert <- combGERT_clean |>
  dplyr::mutate(id = as.character(id)) |>
  dplyr::filter(id %in% keep_ids, !is.na(IntensityRating)) |>
  dplyr::rename(intensity = IntensityRating, gert_correct = emoAccuracy, clip = FileName) |>
  dplyr::inner_join(blocks, by = c("id", "Block"))

## Thesis block-level data (for M0) ---------
thesisBlocks <- combData_clean |>
  dplyr::mutate(id = as.character(id)) |>
  dplyr::filter(id %in% keep_ids, !is.na(Salience)) |>
  dplyr::mutate(
    Aware = bcatAccuracy >= AWARE_BLOCK_CUTOFF,
    dir   = dplyr::if_else(DirectionLabel == "Acc", 0.5, -0.5)
  )

# Data checks ---------
constancy <- combBCAT_clean |>
  dplyr::filter(!is.na(Salience)) |>
  dplyr::group_by(id, Block) |>
  dplyr::summarise(k = dplyr::n_distinct(Salience) + dplyr::n_distinct(DirectionLabel), .groups = "drop")
constant_within_block <- all(constancy$k == 2)
stopifnot(constant_within_block)

N_part   <- dplyr::n_distinct(gert$id)
N_blocks <- nrow(dplyr::semi_join(blocks, dplyr::distinct(gert, id, Block), by = c("id", "Block")))
N_gert   <- nrow(gert)
N_bcat   <- nrow(dplyr::semi_join(bcat, dplyr::distinct(gert, id, Block), by = c("id", "Block")))

# Model helpers ---------
# Try random-effects structures in the handoff's simplification order and keep
# the first that converges without a singular fit.
fit_ok <- function(fit) {
  msgs <- fit@optinfo$conv$lme4$messages
  is.null(msgs) && !lme4::isSingular(fit)
}
fit_seq <- function(formulas, data, family = NULL) {
  log <- character()
  for (f in formulas) {
    fit <- withCallingHandlers(
      if (is.null(family)) {
        lmerTest::lmer(as.formula(f), data = data,
                       control = lme4::lmerControl(optimizer = "bobyqa"))
      } else {
        lme4::glmer(as.formula(f), data = data, family = family,
                    control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
      },
      warning = function(w) invokeRestart("muffleWarning")
    )
    if (fit_ok(fit)) {
      return(list(fit = fit, formula = f, tried = c(log, paste0(f, "  [OK]"))))
    }
    log <- c(log, paste0(f, "  [singular or non-convergent]"))
  }
  list(fit = fit, formula = f, tried = c(log, "  -> kept last (simplest) structure despite issue"))
}
coef_row <- function(fit, term) {
  cs <- summary(fit)$coefficients
  b  <- cs[term, "Estimate"]
  se <- cs[term, "Std. Error"]
  if ("df" %in% colnames(cs)) {
    df <- cs[term, "df"]
    crit <- qt(0.975, df)
    p <- cs[term, "Pr(>|t|)"]
  } else {
    df <- NA
    crit <- qnorm(0.975)
    p <- cs[term, "Pr(>|z|)"]
  }
  tibble::tibble(term = term, b = b, se = se, df = df,
                 lo = b - crit * se, hi = b + crit * se, p = p)
}
re_label <- function(res) sub("^[^~]+~\\s*[^(]*", "", res$formula)

# Models ---------
## M0 sanity: thesis H3B.expand reproduced, then effect-coded version ---------
M0_thesis <- lmerTest::lmer(emoIntensity ~ DirectionLabel * Aware + (1 | id), data = thesisBlocks)
M0 <- fit_seq("emoIntensity ~ Aware * dir + (1 | id)", thesisBlocks)

## M1 manipulation check: salience -> detection (change trials) ---------
bcat_change <- dplyr::filter(bcat, change)
M1 <- fit_seq(c("Accuracy ~ sal * dir + (1 + sal | id)",
                "Accuracy ~ sal * dir + (1 + sal || id)",
                "Accuracy ~ sal * dir + (1 | id)"),
              bcat_change, family = binomial)

## M2 primary: salience -> perceived intensity ---------
M2 <- fit_seq(c("intensity ~ sal * dir + (1 + sal | id) + (1 | clip)",
                "intensity ~ sal * dir + (1 + sal || id) + (1 | clip)",
                "intensity ~ sal * dir + (1 | id) + (1 | clip)",
                "intensity ~ sal * dir + (1 | id)"),
              gert)

## M3: salience -> felt arousal (trial level, as specified) ---------
M3 <- fit_seq(c("Arousal ~ sal * dir + (1 + sal | id)",
                "Arousal ~ sal * dir + (1 + sal || id)",
                "Arousal ~ sal * dir + (1 | id)"),
              dplyr::filter(bcat, !is.na(Arousal)))

## M4 mediation ---------
M4a <- fit_seq(c("det_block ~ sal * dir + (1 + sal | id)",
                 "det_block ~ sal * dir + (1 + sal || id)",
                 "det_block ~ sal * dir + (1 | id)"),
               blocks)
M4b <- fit_seq(c("intensity ~ sal * dir + det_pc + det_pm + (1 | id) + (1 | clip)",
                 "intensity ~ sal * dir + det_pc + det_pm + (1 | id)"),
               gert)

mc_indirect <- function(a_fit, b_fit, b_term) {
  a <- coef_row(a_fit, "sal")
  b <- coef_row(b_fit, b_term)
  set.seed(2026)
  draws <- rnorm(20000, a$b, a$se) * rnorm(20000, b$b, b$se)
  list(a = a, b = b, ab = a$b * b$b, ci = unname(quantile(draws, c(0.025, 0.975))))
}
ind <- mc_indirect(M4a$fit, M4b$fit, "det_pc")

## M5 secondary: salience -> recognition accuracy ---------
M5 <- fit_seq(c("gert_correct ~ sal * dir + (1 + sal | id) + (1 | clip)",
                "gert_correct ~ sal * dir + (1 + sal || id) + (1 | clip)",
                "gert_correct ~ sal * dir + (1 | id) + (1 | clip)",
                "gert_correct ~ sal * dir + (1 | id)"),
              gert, family = binomial)

## Sensitivity / exploratory (not in handoff) ---------
# S1: mediation with the thesis detection measure (all 3 trials incl. catch)
S1a <- fit_seq(c("acc3 ~ sal * dir + (1 + sal | id)", "acc3 ~ sal * dir + (1 + sal || id)",
                 "acc3 ~ sal * dir + (1 | id)"), blocks)
S1b <- fit_seq(c("intensity ~ sal * dir + acc3_pc + acc3_pm + (1 | id) + (1 | clip)",
                 "intensity ~ sal * dir + acc3_pc + acc3_pm + (1 | id)"), gert)
ind_S1 <- mc_indirect(S1a$fit, S1b$fit, "acc3_pc")
# S2: block-averaged arousal as mediator (salience -> block arousal -> GERT intensity)
S2a <- fit_seq(c("arousal ~ sal * dir + (1 + sal | id)", "arousal ~ sal * dir + (1 + sal || id)",
                 "arousal ~ sal * dir + (1 | id)"), dplyr::filter(blocks, !is.na(arousal)))
S2b <- fit_seq(c("intensity ~ sal * dir + arousal_pc + arousal_pm + (1 | id) + (1 | clip)",
                 "intensity ~ sal * dir + arousal_pc + arousal_pm + (1 | id)"),
               dplyr::filter(gert, !is.na(arousal)))
ind_S2 <- mc_indirect(S2a$fit, S2b$fit, "arousal_pc")

# Descriptives ---------
det_rate <- bcat_change |>
  dplyr::group_by(Salience) |>
  dplyr::summarise(rate = 100 * mean(Accuracy, na.rm = TRUE), .groups = "drop")

# Person-level cell means, then within-person (Cousineau-Morey) 95% CIs
cell_person <- gert |>
  dplyr::group_by(id, Salience, DirectionLabel) |>
  dplyr::summarise(m = mean(intensity), .groups = "drop") |>
  dplyr::group_by(id) |>
  dplyr::filter(dplyr::n() == 4) |>
  dplyr::mutate(m_norm = m - mean(m)) |>
  dplyr::ungroup() |>
  dplyr::mutate(m_norm = m_norm + mean(m))
morey <- sqrt(4 / 3)
cells <- cell_person |>
  dplyr::group_by(Salience, DirectionLabel) |>
  dplyr::summarise(
    mean = mean(m), n = dplyr::n(),
    se_w = sd(m_norm) / sqrt(n) * morey,
    .groups = "drop"
  ) |>
  dplyr::mutate(lo = mean - qt(0.975, n - 1) * se_w, hi = mean + qt(0.975, n - 1) * se_w,
                Salience = factor(Salience, levels = c("Low", "High")),
                Direction = dplyr::recode(DirectionLabel, Acc = "Acceleration", Dec = "Deceleration"))
cell_mean <- function(s, d) cells$mean[cells$Salience == s & cells$DirectionLabel == d]

# Figure ---------
fig <- ggplot2::ggplot(cells, ggplot2::aes(x = Salience, y = mean, colour = Direction, group = Direction)) +
  ggplot2::geom_line(position = ggplot2::position_dodge(width = 0.15), linewidth = 0.8) +
  ggplot2::geom_pointrange(ggplot2::aes(ymin = lo, ymax = hi),
                           position = ggplot2::position_dodge(width = 0.15), size = 0.5) +
  ggplot2::scale_colour_manual(values = c(Acceleration = "#C8553D", Deceleration = "#2F6690")) +
  ggplot2::labs(x = "Breathing-change salience", y = "Perceived emotion intensity (1-7)",
                colour = "Breathing change",
                caption = sprintf("Means of person-level cell means (n = %d with all four cells).\nError bars = within-person 95%% CI (Cousineau-Morey).",
                                  cells$n[1])) +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(legend.position = "top", plot.caption = ggplot2::element_text(size = 8, hjust = 0))
ggplot2::ggsave(file.path(outPath, "fig_salience_intensity.png"), fig, width = 5.5, height = 4.5, dpi = 300)

# Model summaries ---------
model_list <- list(
  "M0 thesis spec (emoIntensity ~ DirectionLabel * Aware + (1 | id), block level)" = list(fit = M0_thesis, tried = "as in 08_hypotheses.R"),
  "M0 effect-coded (block level)" = M0,
  "M1 manipulation check (change trials only)" = M1,
  "M2 primary" = M2,
  "M3 felt arousal (trial level)" = M3,
  "M4a mediation a path (det_block = 2 change trials)" = M4a,
  "M4b mediation b path" = M4b,
  "M5 recognition accuracy" = M5,
  "S1a sensitivity a path (acc3 = all 3 trials)" = S1a,
  "S1b sensitivity b path (acc3)" = S1b,
  "S2a exploratory: salience -> block-mean arousal" = S2a,
  "S2b exploratory: block-mean arousal -> intensity" = S2b
)
sink(file.path(outPath, "salience_models.txt"))
cat("Salience reanalysis model output\nRun:", format(Sys.time()), "\n\n")
for (nm in names(model_list)) {
  cat(strrep("=", 78), "\n", nm, "\n", strrep("=", 78), "\n", sep = "")
  cat("Random-effects attempts:\n", paste0("  ", model_list[[nm]]$tried, collapse = "\n"), "\n\n", sep = "")
  print(summary(model_list[[nm]]$fit))
  cat("\n\n")
}
cat(strrep("=", 78), "\nMonte Carlo indirect effects (20,000 draws, seed 2026)\n", sep = "")
cat(sprintf("M4 (det_block): a*b = %.4f, 95%% CI [%.4f, %.4f]\n", ind$ab, ind$ci[1], ind$ci[2]))
cat(sprintf("S1 (acc3):      a*b = %.4f, 95%% CI [%.4f, %.4f]\n", ind_S1$ab, ind_S1$ci[1], ind_S1$ci[2]))
cat(sprintf("S2 (arousal):   a*b = %.4f, 95%% CI [%.4f, %.4f]\n", ind_S2$ab, ind_S2$ci[1], ind_S2$ci[2]))
sink()

# Report ---------
f3 <- function(x) ifelse(is.na(x), "NA", formatC(x, format = "f", digits = 3))
fp <- function(p) ifelse(p < .001, formatC(p, format = "e", digits = 2), formatC(p, format = "f", digits = 3))
r0  <- coef_row(M0$fit, "AwareTRUE")
r0t <- coef_row(M0_thesis, "AwareTRUE")
r1  <- coef_row(M1$fit, "sal")
r2  <- coef_row(M2$fit, "sal"); r2d <- coef_row(M2$fit, "dir"); r2i <- coef_row(M2$fit, "sal:dir")
r3  <- coef_row(M3$fit, "sal"); r3i <- coef_row(M3$fit, "sal:dir")
r4d <- coef_row(M4b$fit, "sal")
r5  <- coef_row(M5$fit, "sal")
prop_med <- if (r2$b > 0) f3(ind$ab / r2$b) else "NA (M2 sal <= 0)"
m0_match <- abs(r0t$b - 0.077) < 0.01
script_time <- format(file.info(file.path(analysisPath, "salience_reanalysis.R"))$mtime, "%Y-%m-%d %H:%M")
s1r <- coef_row(S1a$fit, "sal"); s2r <- coef_row(S2a$fit, "sal")

report <- c(
  "```",
  "SALIENCE REANALYSIS REPORT",
  paste0("Date run: ", format(Sys.Date())),
  paste0("Script: Analysis/salience_reanalysis.R (commit or file timestamp: ", script_time, ")"),
  "",
  "DATA",
  "Source file(s): Results/Combined_fullBCAT_data.csv, Results/Combined_fullGERT_data.csv (trial level); Results/Combined_data.csv (M0 only); Data/QualtricsMar5.xlsx (attention checks)",
  paste0("Salience/direction constant within block? (yes/no): ", ifelse(constant_within_block, "yes", "no")),
  sprintf("Exclusions applied (counts): duplicates= %d Qualtrics re-submissions (first complete kept; task duplicates resolved upstream in 04), attention= %d, BCAT technical= 0 (not applied, per Norm), <6 blocks= %d",
          n_dup_qualtrics, length(ids_att), length(low_block)),
  sprintf("Final N participants= %d, N blocks= %d, N GERT trials= %d, N BCAT trials= %d", N_part, N_blocks, N_gert, N_bcat),
  "Coding: sal (+0.5 high / -0.5 low); dir (+0.5 accel / -0.5 decel)",
  "",
  "M0 SANITY (aware -> intensity; thesis b = .077)",
  sprintf("aware: b= %s, SE= %s, 95%% CI [%s, %s], p= %s  (thesis spec, Acc reference)", f3(r0t$b), f3(r0t$se), f3(r0t$lo), f3(r0t$hi), fp(r0t$p)),
  sprintf("       effect-coded dir: b= %s, SE= %s, 95%% CI [%s, %s], p= %s", f3(r0$b), f3(r0$se), f3(r0$lo), f3(r0$hi), fp(r0$p)),
  sprintf("Matches thesis? (yes/no; if no, why): %s", ifelse(m0_match, "yes", "no; see notes")),
  "",
  "M1 MANIPULATION CHECK (salience -> detection, logit)",
  sprintf("sal: b= %s, SE= %s, 95%% CI [%s, %s], p= %s, odds ratio= %s", f3(r1$b), f3(r1$se), f3(r1$lo), f3(r1$hi), fp(r1$p), f3(exp(r1$b))),
  sprintf("Detection rate: high salience= %.1f%%, low salience= %.1f%%  (change trials only)",
          det_rate$rate[det_rate$Salience == "High"], det_rate$rate[det_rate$Salience == "Low"]),
  "",
  "M2 PRIMARY (salience -> perceived intensity, 7-pt scale)",
  sprintf("sal: b= %s, SE= %s, df= %.1f, 95%% CI [%s, %s], p= %s", f3(r2$b), f3(r2$se), r2$df, f3(r2$lo), f3(r2$hi), fp(r2$p)),
  sprintf("dir: b= %s, p= %s", f3(r2d$b), fp(r2d$p)),
  sprintf("sal x dir: b= %s, p= %s", f3(r2i$b), fp(r2i$p)),
  sprintf("Cell means (intensity): high/accel= %s, high/decel= %s, low/accel= %s, low/decel= %s",
          f3(cell_mean("High", "Acc")), f3(cell_mean("High", "Dec")), f3(cell_mean("Low", "Acc")), f3(cell_mean("Low", "Dec"))),
  paste0("Random effects used: ", re_label(M2)),
  "",
  "M3 (salience -> felt arousal)",
  sprintf("sal: b= %s, SE= %s, 95%% CI [%s, %s], p= %s", f3(r3$b), f3(r3$se), f3(r3$lo), f3(r3$hi), fp(r3$p)),
  sprintf("sal x dir: b= %s, p= %s", f3(r3i$b), fp(r3i$p)),
  "",
  "M4 MEDIATION (salience -> detection -> intensity)",
  sprintf("a (sal -> det_block): b= %s, SE= %s, p= %s", f3(ind$a$b), f3(ind$a$se), fp(ind$a$p)),
  sprintf("b (det_pc -> intensity | sal): b= %s, SE= %s, p= %s", f3(ind$b$b), f3(ind$b$se), fp(ind$b$p)),
  sprintf("direct (sal in M4b): b= %s, p= %s", f3(r4d$b), fp(r4d$p)),
  sprintf("indirect a*b= %s, Monte Carlo 95%% CI [%s, %s]", formatC(ind$ab, format = "f", digits = 5),
          formatC(ind$ci[1], format = "f", digits = 5), formatC(ind$ci[2], format = "f", digits = 5)),
  paste0("proportion mediated= ", prop_med),
  "",
  "M5 SECONDARY (salience -> recognition accuracy, logit)",
  sprintf("sal: b= %s, SE= %s, p= %s", f3(r5$b), f3(r5$se), fp(r5$p)),
  "",
  "DEVIATIONS AND NOTES",
  paste0("Convergence/singularity fixes: M1 ", re_label(M1), "; M2 ", re_label(M2), "; M3 ", re_label(M3),
         "; M4a ", re_label(M4a), "; M4b ", re_label(M4b), "; M5 ", re_label(M5),
         " (full attempt log in salience_models.txt)"),
  "Anything that differed from this handoff:",
  "  - Blocks contain 2 change trials + 1 no-change catch trial, not 3 changes. det_block (M1, M4) = proportion of the 2 change trials detected; M0 keeps the thesis definition (all 3 trials, aware = >= 2/3).",
  "  - Per Norm, exclusions limited to attention-check failures (first complete Qualtrics submission) plus the existing <6-block rule; BCAT baseline technical failures retained.",
  "  - M3 fit at trial level as specified (arousal is rated after each BCAT trial); block-averaged arousal used in exploratory S2.",
  sprintf("  - Sensitivity S1 (detection = all 3 trials): a = %s (p = %s), indirect = %s [%s, %s].",
          f3(s1r$b), fp(s1r$p), formatC(ind_S1$ab, format = "f", digits = 5),
          formatC(ind_S1$ci[1], format = "f", digits = 5), formatC(ind_S1$ci[2], format = "f", digits = 5)),
  sprintf("  - Exploratory S2 (block-mean arousal as mediator): a = %s (p = %s), b = %s (p = %s), indirect = %s [%s, %s].",
          f3(s2r$b), fp(s2r$p), f3(ind_S2$b$b), fp(ind_S2$b$p), formatC(ind_S2$ab, format = "f", digits = 5),
          formatC(ind_S2$ci[1], format = "f", digits = 5), formatC(ind_S2$ci[2], format = "f", digits = 5)),
  "Anything surprising:",
  "  - Salience is a strong manipulation of detection (M1, M4a) yet has no total effect on perceived intensity (M2), with a tight CI around zero.",
  "  - The within-person detection -> intensity association (M4b) is small and borderline; the indirect path is tiny relative to the M2 CI width.",
  "  - Block-mean arousal predicts GERT intensity (S2b) but salience does not move arousal (M3, S2a); M3 shows a salience x direction interaction.",
  "  - One combined-task file stores an e-mail address in place of the participant number (pseudonymised here; upstream CSVs still contain it).",
  "  - 04_prep_combined.R builds file paths with paste0(taskDataPath, file) and taskDataPath has no trailing separator, so the current script cannot regenerate the Combined_*.csv files; this analysis uses the existing CSVs.",
  "```"
)
writeLines(report, file.path(outPath, "salience_report.md"))
cat(report, sep = "\n")

# Moderation framing (added 2026-10-01 per Norm) ---------
# Hypothesis: DIRECTION (the physiological change) drives perceived intensity,
# and SALIENCE (awareness manipulation) moderates it. Key terms are sal:dir and
# the simple effects of dir at high vs low salience. Simple effects come from
# refitting the chosen model with salience re-centred on each level.
simple_dir <- function(res, data) {
  purrr::map_dfr(c(High = 0.5, Low = -0.5), function(lvl) {
    d <- dplyr::mutate(data, sal_c = sal - lvl)
    f <- gsub("\\bsal\\b", "sal_c", res$formula, perl = TRUE)
    fit <- if (inherits(res$fit, "glmerMod")) {
      lme4::glmer(as.formula(f), data = d, family = binomial,
                  control = lme4::glmerControl(optimizer = "bobyqa"))
    } else {
      lmerTest::lmer(as.formula(f), data = d, control = lme4::lmerControl(optimizer = "bobyqa"))
    }
    coef_row(fit, "dir")
  }, .id = "salience")
}
blocks_ar <- dplyr::filter(blocks, !is.na(arousal))
gert_ar   <- dplyr::filter(gert, !is.na(arousal))

mod_tab <- dplyr::bind_rows(
  dplyr::mutate(simple_dir(M2, gert), outcome = "GERT intensity (M2)"),
  dplyr::mutate(simple_dir(M5, gert), outcome = "GERT accuracy, logit (M5)"),
  dplyr::mutate(simple_dir(M3, dplyr::filter(bcat, !is.na(Arousal))), outcome = "Felt arousal, trial (M3)"),
  dplyr::mutate(simple_dir(S2a, blocks_ar), outcome = "Felt arousal, block mean (S2a)")
) |> dplyr::select(outcome, salience, b, se, lo, hi, p)
int_tab <- dplyr::bind_rows(
  dplyr::mutate(coef_row(M2$fit, "sal:dir"), outcome = "GERT intensity (M2)"),
  dplyr::mutate(coef_row(M5$fit, "sal:dir"), outcome = "GERT accuracy, logit (M5)"),
  dplyr::mutate(coef_row(M3$fit, "sal:dir"), outcome = "Felt arousal, trial (M3)"),
  dplyr::mutate(coef_row(S2a$fit, "sal:dir"), outcome = "Felt arousal, block mean (S2a)")
) |> dplyr::select(outcome, b, se, lo, hi, p)

# Measured awareness as moderator: does the direction effect grow in blocks
# where the person detected more of the changes?
MOD_det <- fit_seq(c("intensity ~ dir * det_pc + det_pm + sal + (1 | id) + (1 | clip)",
                     "intensity ~ dir * det_pc + det_pm + sal + (1 | id)"), gert)

# Moderated mediation: direction -> block arousal -> intensity, with salience
# moderating the a path. a(sal) = dir + sal:dir * sal; b = arousal_pc.
set.seed(2026)
cs_a <- summary(S2a$fit)$coefficients
V_a  <- as.matrix(vcov(S2a$fit))[c("dir", "sal:dir"), c("dir", "sal:dir")]
a_draws <- MASS::mvrnorm(20000, cs_a[c("dir", "sal:dir"), "Estimate"], V_a)
b_row   <- coef_row(S2b$fit, "arousal_pc")
b_draws <- rnorm(20000, b_row$b, b_row$se)
modmed <- tibble::tibble(
  quantity = c("indirect | high salience", "indirect | low salience", "index of moderated mediation"),
  draws = list((a_draws[, 1] + 0.5 * a_draws[, 2]) * b_draws,
               (a_draws[, 1] - 0.5 * a_draws[, 2]) * b_draws,
               a_draws[, 2] * b_draws)
) |>
  dplyr::mutate(est = c((cs_a["dir", 1] + 0.5 * cs_a["sal:dir", 1]) * b_row$b,
                        (cs_a["dir", 1] - 0.5 * cs_a["sal:dir", 1]) * b_row$b,
                        cs_a["sal:dir", 1] * b_row$b),
                lo = purrr::map_dbl(draws, stats::quantile, 0.025),
                hi = purrr::map_dbl(draws, stats::quantile, 0.975)) |>
  dplyr::select(-draws)

sink(file.path(outPath, "salience_moderation.txt"))
cat("Moderation framing: direction effect (accel - decel) moderated by salience\n",
    "dir coded +0.5 accel / -0.5 decel, so b = accel minus decel.\n\n", sep = "")
cat("sal x dir interactions\n"); print(as.data.frame(int_tab), digits = 3)
cat("\nSimple effects of direction at each salience level\n"); print(as.data.frame(mod_tab), digits = 3)
cat("\nMeasured awareness moderator (", MOD_det$formula, ")\n", sep = "")
print(as.data.frame(dplyr::bind_rows(lapply(c("dir", "det_pc", "dir:det_pc"), coef_row, fit = MOD_det$fit))), digits = 3)
cat("\nModerated mediation: dir -> block arousal (a, moderated by sal) -> intensity (b = arousal_pc)\n")
cat(sprintf("b path (arousal_pc -> intensity): b = %.4f, SE = %.4f, p = %.4g\n", b_row$b, b_row$se, b_row$p))
print(as.data.frame(modmed), digits = 3)

# Index of moderated mediation with the a path at trial level (M3), for
# comparison; the block-level version above is the appropriate unit.
set.seed(2026)
a_tr <- coef_row(M3$fit, "sal:dir")
d_tr <- rnorm(20000, a_tr$b, a_tr$se) * rnorm(20000, b_row$b, b_row$se)
cat(sprintf("\nIndex of moderated mediation, trial-level a path: %.5f, 95%% CI [%.5f, %.5f]\n",
            a_tr$b * b_row$b, quantile(d_tr, 0.025), quantile(d_tr, 0.975)))
cat("Total sal:dir on intensity (M2):\n"); print(as.data.frame(coef_row(M2$fit, "sal:dir")), digits = 3)
cat("Direct sal:dir on intensity | arousal (S2b):\n"); print(as.data.frame(coef_row(S2b$fit, "sal:dir")), digits = 3)

# Does salience moderate the b path (arousal -> intensity)?
S2c <- lmerTest::lmer(intensity ~ sal * dir + arousal_pc * sal + arousal_pm + (1 | id) + (1 | clip),
                      data = gert_ar, control = lme4::lmerControl(optimizer = "bobyqa"))
cat("\nb-path moderation (", deparse1(formula(S2c)), ")\n", sep = "")
print(as.data.frame(dplyr::bind_rows(lapply(c("arousal_pc", "sal:arousal_pc"), coef_row, fit = S2c))), digits = 3)
sink()
cat(readLines(file.path(outPath, "salience_moderation.txt")), sep = "\n")
