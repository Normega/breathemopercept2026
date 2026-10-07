# equivalence_tests.R
# Equivalence tests (two one-sided tests, TOST; alpha = .05) for the study's
# key null results: is each effect demonstrably smaller than the smallest
# effect of interest (SESOI), rather than merely non-significant?
#
# SESOI, anchored on effects this study did find:
#   correlations     |r| = .17, the size of H2B (threshold - recognition), the
#                    one between-person association that replicated
#   intensity        0.05 scale points (1-7 scale): about the awareness-intensity
#                    association that salience was meant to produce; 0.10 also shown
#   accuracy         2 percentage points (linear probability model)
#   heart rate       1 bpm
# Equivalence = the 90% CI lies inside [-SESOI, +SESOI] (p_TOST < .05). The
# last column gives the smallest bound the data would support.
#
# Input: Results/ CSVs; Results/body_vs_awareness/estimates.csv and
# Results/cardiac_analysis/estimates.csv (written by those scripts).
# Output: Results/equivalence/report.md, equivalence.csv
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

## Paths and constants ---------
if (!exists("mainPath")) mainPath <- "I:/Shared drives/Aya/"
resultsPath <- file.path(mainPath, "Repo", "Results")
outPath <- file.path(resultsPath, "equivalence")
if (!dir.exists(outPath)) dir.create(outPath, recursive = TRUE)
rd <- function(...) readr::read_csv(file.path(resultsPath, ...), col_types = readr::cols(id = "c", .default = "?"))
SESOI_R <- 0.17; SESOI_INT <- c(0.05, 0.10); SESOI_ACC <- 0.02; SESOI_HR <- 1
ECG_MIN <- 0.6; MAX_ART_PCT <- 10

# TOST helpers ---------
tost_coef <- function(test, b, se, df, bound, unit) {
  t_lo <- (b + bound) / se; t_hi <- (b - bound) / se
  p <- max(stats::pt(t_lo, df, lower.tail = FALSE), stats::pt(t_hi, df))
  h <- stats::qt(0.95, df) * se
  data.frame(test = test, estimate = b, ci90_lo = b - h, ci90_hi = b + h, sesoi = bound, unit = unit,
             p_tost = p, equivalent = p < .05, min_bound = max(abs(b - h), abs(b + h)), stringsAsFactors = FALSE)
}
tost_r <- function(test, x, y, bound = SESOI_R) {
  ok <- is.finite(x) & is.finite(y); n <- sum(ok); r <- cor(x[ok], y[ok])
  z <- atanh(r); se <- 1 / sqrt(n - 3); zb <- atanh(bound)
  p <- max(stats::pnorm((z + zb) / se, lower.tail = FALSE), stats::pnorm((z - zb) / se))
  h <- stats::qnorm(0.95) * se
  data.frame(test = sprintf("%s (n = %d)", test, n), estimate = r, ci90_lo = tanh(z - h), ci90_hi = tanh(z + h),
             sesoi = bound, unit = "r", p_tost = p, equivalent = p < .05,
             min_bound = max(abs(tanh(z - h)), abs(tanh(z + h))), stringsAsFactors = FALSE)
}

# Data ---------
excl <- rd("exclusions.csv")
q <- rd("questionnaireFile.csv") |> dplyr::mutate(id = sub("\\.0$", "", id))
gb <- rd("GERT_baseline_data.csv") |> dplyr::group_by(id) |>
  dplyr::summarise(gert_acc = mean(emoAccuracy, na.rm = TRUE), gert_int = mean(IntensityRating, na.rm = TRUE), .groups = "drop")
th <- rd("BCAT_baseline_data.csv") |> dplyr::group_by(id) |>
  dplyr::summarise(thresh = (ACCthresh[1] + DECthresh[1]) / 2, .groups = "drop")
p1 <- q |> dplyr::filter(id %in% excl$id[excl$set_gert_baseline]) |> dplyr::left_join(gb, by = "id")
p2 <- q |> dplyr::filter(id %in% excl$id[excl$set_bcat_baseline]) |> dplyr::left_join(gb, by = "id") |>
  dplyr::left_join(th, by = "id")

blk <- rd("Combined_data.csv") |> dplyr::filter(id %in% excl$id[excl$set_combined]) |>
  dplyr::transmute(id, Block, dir_e = ifelse(DirectionLabel == "Acc", 0.5, -0.5),
                   sal_e = ifelse(Salience == "High", 0.5, -0.5))
clips <- rd("Combined_fullGERT_data.csv") |> dplyr::inner_join(blk, by = c("id", "Block"))

cp <- rd("physio", "cardiac_participants.csv") |>
  dplyr::filter(id %in% excl$id[excl$set_questionnaire], ecg_quality >= ECG_MIN,
                !is.na(rest_rmssd_ms), rest_pct_artifact <= MAX_ART_PCT) |>
  dplyr::left_join(q |> dplyr::select(id, BATtotal), by = "id") |> dplyr::left_join(gb, by = "id")

# Tests ---------
out <- list()
m_int <- lmer(IntensityRating ~ sal_e * dir_e + (1 | id) + (1 | FileName), data = clips)
m_acc <- lmer(emoAccuracy ~ sal_e * dir_e + (1 | id) + (1 | FileName), data = clips)   # linear probability
co <- function(m, term) { s <- summary(m)$coefficients; c(s[term, 1], s[term, 2], s[term, "df"]) }
for (bnd in SESOI_INT) {
  v <- co(m_int, "sal_e");       out[[length(out) + 1]] <- tost_coef("Salience (high - low) -> clip intensity", v[1], v[2], v[3], bnd, "scale points")
  v <- co(m_int, "dir_e");       out[[length(out) + 1]] <- tost_coef("Direction (accelerate - decelerate) -> clip intensity", v[1], v[2], v[3], bnd, "scale points")
  v <- co(m_int, "sal_e:dir_e"); out[[length(out) + 1]] <- tost_coef("Salience x direction -> clip intensity", v[1], v[2], v[3], bnd, "scale points")
}
v <- co(m_acc, "sal_e"); out[[length(out) + 1]] <- tost_coef("Salience (high - low) -> recognition accuracy", v[1], v[2], v[3], SESOI_ACC, "proportion")
v <- co(m_acc, "dir_e"); out[[length(out) + 1]] <- tost_coef("Direction (accelerate - decelerate) -> recognition accuracy", v[1], v[2], v[3], SESOI_ACC, "proportion")

ba <- read.csv(file.path(resultsPath, "body_vs_awareness", "estimates.csv"))
b1 <- ba[ba$estimate == "body_change_per_sd_on_intensity", ]
for (bnd in SESOI_INT)
  out[[length(out) + 1]] <- tost_coef("Breathing change made (per within-person SD) -> clip intensity", b1$b, b1$se, b1$df, bnd, "scale points")
ca <- read.csv(file.path(resultsPath, "cardiac_analysis", "estimates.csv"))
for (k in seq_len(nrow(ca)))
  out[[length(out) + 1]] <- tost_coef(c(hr_change_accel_minus_decel = "Direction (accelerate - decelerate) -> HR change",
                                        hr_change_high_minus_low_salience = "Salience (high - low) -> HR change")[[ca$estimate[k]]],
                                      ca$b[k], ca$se[k], ca$df[k], SESOI_HR, "bpm")

out[[length(out) + 1]] <- tost_r("H1A burnout - baseline recognition accuracy", p1$BATtotal, p1$gert_acc)
out[[length(out) + 1]] <- tost_r("H1B burnout - baseline perceived intensity", p1$BATtotal, p1$gert_int)
out[[length(out) + 1]] <- tost_r("H2A burnout - BCAT threshold", p2$BATtotal, p2$thresh)
out[[length(out) + 1]] <- tost_r("H2C BCAT threshold - baseline perceived intensity", p2$thresh, p2$gert_int)
out[[length(out) + 1]] <- tost_r("E1A resting lnRMSSD - baseline recognition accuracy", log(cp$rest_rmssd_ms), cp$gert_acc)
out[[length(out) + 1]] <- tost_r("Burnout - resting heart rate", cp$BATtotal, cp$rest_hr_bpm)
out[[length(out) + 1]] <- tost_r("Burnout - resting lnRMSSD", cp$BATtotal, log(cp$rest_rmssd_ms))

res <- dplyr::bind_rows(out)
write.csv(res, file.path(outPath, "equivalence.csv"), row.names = FALSE)

fmt <- function(x) sprintf("%.3f", x)
fp  <- function(p) if (p < .001) "< .001" else sprintf("%.3f", p)
md <- c("# Equivalence tests for the key null results", "",
  sprintf("Run %s. Script `Analysis/equivalence_tests.R`. Two one-sided tests (TOST), alpha = .05: an effect is *equivalent to zero* when its 90%% CI lies inside +-SESOI. Exploratory.", format(Sys.Date())), "",
  "SESOI: correlations |r| = .17 (the size of H2B, the association that replicated); clip intensity 0.05 scale points (about the awareness-intensity association; 0.10 also shown); recognition accuracy 2 percentage points; heart rate 1 bpm. *Smallest supported bound* = the narrowest symmetric bound the 90% CI fits inside.", "",
  "| Test | Estimate | 90% CI | SESOI | p (TOST) | Equivalent | Smallest supported bound |", "|---|---|---|---|---|---|---|",
  sprintf("| %s | %s | [%s, %s] | +-%s %s | %s | %s | %s |", res$test, fmt(res$estimate), fmt(res$ci90_lo), fmt(res$ci90_hi),
          format(res$sesoi), res$unit, sapply(res$p_tost, fp), ifelse(res$equivalent, "yes", "no"), fmt(res$min_bound)))
writeLines(md, file.path(outPath, "report.md"))
message("Done: ", outPath)
