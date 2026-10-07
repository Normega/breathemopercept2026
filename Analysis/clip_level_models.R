# clip_level_models.R
# H3 (preregistered) and E2C (preregistered exploratory) at the level of the
# individual GERT clip, with random intercepts for participant AND clip (60
# clips, each seen once per person), beside the block-mean models of
# R/08_hypotheses.R and the thesis.
#
# The block-mean models treat each block's 5 clips as one observation and
# ignore that clips differ in how intense and how recognisable they are; clip
# is a crossed random factor. Random slopes for the block predictor by
# participant are attempted and kept when the fit is not singular.
#
# Sample: combined-task sample (R/06). Input: Results/ CSVs.
# Output: Results/clip_level_models/ (report.md, models.txt, estimates.csv)
# ---------------------------------------------------------------

# Set Up ---------
## Load libraries ---------
packages <- c("tidyverse", "lme4", "lmerTest")
new_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
if (length(new_packages)) install.packages(new_packages)
options(readr.show_col_types = FALSE)
for (thispack in packages) {
  library(thispack, character.only = TRUE, quietly = TRUE, verbose = FALSE)
}

## Paths ---------
if (!exists("mainPath")) mainPath <- "I:/Shared drives/Aya/"
resultsPath <- file.path(mainPath, "Repo", "Results")
outPath <- file.path(resultsPath, "clip_level_models")
if (!dir.exists(outPath)) dir.create(outPath, recursive = TRUE)
rd <- function(...) readr::read_csv(file.path(resultsPath, ...), col_types = readr::cols(id = "c", .default = "?"))

# Data ---------
excl <- rd("exclusions.csv")
blk <- rd("Combined_data.csv") |>
  dplyr::filter(id %in% excl$id[excl$set_combined]) |>
  dplyr::mutate(Aware = bcatAccuracy >= 2/3,                       # thesis / prereg definition
                AccelAware = DirectionLabel == "Acc" & Aware,
                dir_e = ifelse(DirectionLabel == "Acc", 0.5, -0.5),
                sal_e = ifelse(Salience == "High", 0.5, -0.5),
                aw_e  = ifelse(Aware, 0.5, -0.5)) |>
  dplyr::group_by(id) |>
  dplyr::mutate(ar_pm = mean(Arousal, na.rm = TRUE), ar_pc = Arousal - ar_pm) |>
  dplyr::ungroup()
clips <- rd("Combined_fullGERT_data.csv") |>
  dplyr::select(id, Block, FileName, IntensityRating, emoAccuracy, emo_distance) |>
  dplyr::inner_join(blk |> dplyr::select(id, Block, Aware, AccelAware, dir_e, sal_e, aw_e, Arousal, ar_pm, ar_pc),
                    by = c("id", "Block"))

# Fitting with a random-slope attempt ---------
fit <- function(formula_slope, formula_int, data, family = NULL) {
  f <- function(fm) suppressMessages(suppressWarnings(
    if (is.null(family)) lmer(fm, data = data, control = lmerControl(optimizer = "bobyqa"))
    else glmer(fm, data = data, family = family, control = glmerControl(optimizer = "bobyqa"))))
  m <- f(formula_slope)
  conv_bad <- length(m@optinfo$conv$lme4$messages) > 0
  if (isSingular(m) || conv_bad) { m <- f(formula_int); attr(m, "re_note") <- "intercepts only (slope singular or not converged)" }
  else attr(m, "re_note") <- "with by-participant slope"
  m
}
ex <- function(m, term, label, level) {
  s <- summary(m)$coefficients; p <- s[term, ncol(s)]
  ci <- suppressMessages(confint(m, parm = term, method = "Wald"))
  data.frame(model = label, level = level, term = term, b = s[term, 1], lo = ci[1], hi = ci[2], p = p,
             random = if (is.null(attr(m, "re_note", exact = TRUE))) "(1 | id)" else attr(m, "re_note", exact = TRUE),
             n_obs = stats::nobs(m), stringsAsFactors = FALSE)
}

rows <- list()
# H3, block level as preregistered / thesis (08_hypotheses.R)
for (y in c("emoAccuracy", "emoIntensity", "emoDistance")) {
  m <- lmer(as.formula(paste(y, "~ AccelAware + (1 | id)")), data = blk)
  rows[[length(rows) + 1]] <- ex(m, "AccelAwareTRUE", paste("H3", y), "block means")
}
# H3, clip level
m <- fit(emoAccuracy ~ AccelAware + (1 + AccelAware | id) + (1 | FileName),
         emoAccuracy ~ AccelAware + (1 | id) + (1 | FileName), clips, binomial)
rows[[length(rows) + 1]] <- ex(m, "AccelAwareTRUE", "H3 accuracy (logit)", "clip")
for (y in c("IntensityRating", "emo_distance")) {
  m <- fit(as.formula(paste(y, "~ AccelAware + (1 + AccelAware | id) + (1 | FileName)")),
           as.formula(paste(y, "~ AccelAware + (1 | id) + (1 | FileName)")), clips)
  rows[[length(rows) + 1]] <- ex(m, "AccelAwareTRUE", paste("H3", y), "clip")
}
# H3 expansion: direction x awareness, effect coded (main effect of awareness averaged over direction)
m <- lmer(emoIntensity ~ dir_e * aw_e + (1 | id), data = blk)
for (t in c("aw_e", "dir_e", "dir_e:aw_e")) rows[[length(rows) + 1]] <- ex(m, t, "H3 expansion, intensity", "block means")
m <- fit(IntensityRating ~ dir_e * aw_e + (1 + aw_e | id) + (1 | FileName),
         IntensityRating ~ dir_e * aw_e + (1 | id) + (1 | FileName), clips)
for (t in c("aw_e", "dir_e", "dir_e:aw_e")) rows[[length(rows) + 1]] <- ex(m, t, "H3 expansion, intensity", "clip")

# E2C: felt arousal -> intensity. Thesis model (raw arousal, block means), then
# within/between split at block and clip level, controlling condition.
m <- lmer(emoIntensity ~ Arousal + (1 | id), data = blk)
rows[[length(rows) + 1]] <- ex(m, "Arousal", "E2C intensity (thesis model)", "block means")
m <- lmer(emoIntensity ~ ar_pc + ar_pm + dir_e * sal_e + (1 | id), data = blk)
for (t in c("ar_pc", "ar_pm")) rows[[length(rows) + 1]] <- ex(m, t, "E2C intensity", "block means")
m <- fit(IntensityRating ~ ar_pc + ar_pm + dir_e * sal_e + (1 + ar_pc | id) + (1 | FileName),
         IntensityRating ~ ar_pc + ar_pm + dir_e * sal_e + (1 | id) + (1 | FileName), clips)
for (t in c("ar_pc", "ar_pm")) rows[[length(rows) + 1]] <- ex(m, t, "E2C intensity", "clip")
m <- fit(emoAccuracy ~ ar_pc + ar_pm + dir_e * sal_e + (1 + ar_pc | id) + (1 | FileName),
         emoAccuracy ~ ar_pc + ar_pm + dir_e * sal_e + (1 | id) + (1 | FileName), clips, binomial)
rows[[length(rows) + 1]] <- ex(m, "ar_pc", "E2C accuracy (logit)", "clip")

res <- dplyr::bind_rows(rows)
write.csv(res, file.path(outPath, "estimates.csv"), row.names = FALSE)

# The preregistered BH family with H3A/H3B replaced by their clip-level p values
cr <- read.csv(file.path(resultsPath, "confirmatory_rerun", "confirmatory_vs_thesis.csv"), stringsAsFactors = FALSE)
p7 <- c(cr$p[match(c("H1A", "H1B", "H2A", "H2B", "H2C"), cr$test)],
        res$p[res$model == "H3 accuracy (logit)"], res$p[res$model == "H3 IntensityRating"])
bh7 <- stats::p.adjust(p7, "BH")

fp <- function(p) if (p < .001) formatC(p, format = "e", digits = 1) else sprintf("%.3f", p)
md <- c("# Clip-level versions of H3 and E2C", "",
  sprintf("Run %s. Script `Analysis/clip_level_models.R`; full output in `models.txt`. %d participants, %d blocks, %d clips (combined-task sample).",
          format(Sys.Date()), length(unique(blk$id)), nrow(blk), nrow(clips)), "",
  "Clip-level models add a random intercept for clip (60 clips, each seen once per person) and attempt a by-participant slope for the block predictor (kept unless singular).", "",
  "| Model | Level | Term | b | 95% CI | p | Random effects | N |", "|---|---|---|---|---|---|---|---|",
  sprintf("| %s | %s | %s | %.3f | [%.3f, %.3f] | %s | %s | %d |", res$model, res$level, res$term, res$b, res$lo, res$hi,
          sapply(res$p, fp), res$random, res$n_obs), "",
  "Preregistered BH family (7 tests) with H3A and H3B at clip level:", "",
  "| Test | p | p (BH) |", "|---|---|---|",
  sprintf("| %s | %s | %s |", c("H1A", "H1B", "H2A", "H2B", "H2C", "H3A (clip)", "H3B (clip)"), sapply(p7, fp), sapply(bh7, fp)))
writeLines(md, file.path(outPath, "report.md"))
message("Done: ", outPath)
