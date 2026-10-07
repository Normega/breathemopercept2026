# h2b_robustness.R
# Is the preregistered H2B association (worse breath-change threshold <-> lower
# emotion-recognition accuracy) about interoception, or about engagement,
# response bias or measurement artefacts?
#
#   1. Replication on independent stimuli: the combined task's 60 GERT clips
#      (different from the 22 baseline clips) in the same people.
#   2. Response bias: no-change (catch) trials give a false-alarm rate (reporting
#      a change that did not happen), and change trials a "no change" rate.
#   3. Engagement and general covariates: mean confidence, MAIA, age, gender,
#      PHQ-4, burnout.
#   4. Pacer compliance (participants whose belt follows the pacer): synchrony
#      and entrainment gain.
#   5. Threshold measurement: direction-specific thresholds, their agreement
#      (reliability of the mean), rank correlation, the 8 thresholds at the
#      task's ceiling (1.0), and influential points.
# Input: Results/ CSVs written by the pipeline. Output: Results/h2b_robustness/
# Exploratory (H2B itself is preregistered); p values uncorrected.
# ---------------------------------------------------------------

# Set Up ---------
## Load libraries ---------
packages <- c("tidyverse", "MASS")
new_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
if (length(new_packages)) install.packages(new_packages)
options(readr.show_col_types = FALSE)
for (thispack in packages) {
  library(thispack, character.only = TRUE, quietly = TRUE, verbose = FALSE)
}

## Paths ---------
if (!exists("mainPath")) mainPath <- "I:/Shared drives/Aya/"
resultsPath <- file.path(mainPath, "Repo", "Results")
outPath <- file.path(resultsPath, "h2b_robustness")
if (!dir.exists(outPath)) dir.create(outPath, recursive = TRUE)
rd <- function(...) readr::read_csv(file.path(resultsPath, ...), col_types = readr::cols(id = "c", .default = "?"))
SYNC_MIN <- 0.4

# Data ---------
excl <- rd("exclusions.csv")
ids  <- excl$id[excl$set_bcat_baseline]          # the H2B sample

bb <- rd("BCAT_baseline_data.csv") |> dplyr::filter(id %in% ids)
thr <- bb |> dplyr::group_by(id) |>
  dplyr::summarise(acc_thr = ACCthresh[1], dec_thr = DECthresh[1],
                   base_conf = mean(Confidence, na.rm = TRUE),
                   base_fa = mean(Response[Direction == "NoChange"] != "up", na.rm = TRUE),
                   base_n_catch = sum(Direction == "NoChange"),
                   base_miss_none = mean(Response[Direction != "NoChange"] == "up", na.rm = TRUE),
                   .groups = "drop") |>
  dplyr::mutate(thresh = (acc_thr + dec_thr) / 2,
                ceiling = acc_thr >= 1 | dec_thr >= 1)

cb <- rd("Combined_fullBCAT_data.csv") |> dplyr::filter(id %in% excl$id[excl$set_combined])
comb_bias <- cb |> dplyr::group_by(id) |>
  dplyr::summarise(comb_fa = mean(Response[Direction == "NoChange"] != "up", na.rm = TRUE),
                   comb_n_catch = sum(Direction == "NoChange"),
                   comb_det = mean(Accuracy[Direction != "NoChange"], na.rm = TRUE), .groups = "drop")

gb <- rd("GERT_baseline_data.csv") |> dplyr::group_by(id) |>
  dplyr::summarise(gert_acc = mean(emoAccuracy, na.rm = TRUE), gert_int = mean(IntensityRating, na.rm = TRUE), .groups = "drop")
gc <- rd("Combined_fullGERT_data.csv") |> dplyr::group_by(id) |>
  dplyr::summarise(gert_comb_acc = mean(emoAccuracy, na.rm = TRUE), .groups = "drop")
q  <- rd("questionnaireFile.csv") |> dplyr::mutate(id = sub("\\.0$", "", id)) |>
  dplyr::select(id, MAIAtotal, Age, Gender, PHQ4total, BATtotal)
bp <- rd("physio", "breath_participants.csv") |> dplyr::select(id, sync, entrainment_gain, direction_compliance)

d <- thr |>
  dplyr::left_join(comb_bias, by = "id") |>
  dplyr::left_join(gb, by = "id") |> dplyr::left_join(gc, by = "id") |>
  dplyr::left_join(q, by = "id") |> dplyr::left_join(bp, by = "id") |>
  dplyr::mutate(
    # pooled false-alarm rate over all catch trials a participant saw
    fa = (base_fa * base_n_catch + ifelse(is.na(comb_fa), 0, comb_fa * comb_n_catch)) /
         (base_n_catch + ifelse(is.na(comb_n_catch), 0, comb_n_catch)),
    female = as.numeric(Gender == "Female"),
    belt_ok = !is.na(sync) & sync >= SYNC_MIN)

z <- function(x) as.numeric(scale(x))
ctxt <- function(x, y, method = "pearson") {
  t <- suppressWarnings(cor.test(x, y, method = method))
  if (method == "pearson")
    sprintf("r = %.3f [%.3f, %.3f], p = %s, n = %d", t$estimate, t$conf.int[1], t$conf.int[2], fp(t$p.value), sum(is.finite(x) & is.finite(y)))
  else sprintf("rho = %.3f, p = %s, n = %d", t$estimate, fp(t$p.value), sum(is.finite(x) & is.finite(y)))
}
fp <- function(p) if (p < .001) formatC(p, format = "e", digits = 1) else sprintf("%.3f", p)
beta <- function(m, term = "z(thresh)") {
  s <- summary(m)$coefficients; ci <- confint(m)[term, ]
  sprintf("beta = %.3f [%.3f, %.3f], p = %s, n = %d", s[term, 1], ci[1], ci[2], fp(s[term, 4]), stats::nobs(m))
}

# Analyses ---------
res <- list()
res$h2b       <- ctxt(d$thresh, d$gert_acc)
res$replicate <- ctxt(d$thresh, d$gert_comb_acc)
res$acc_dir   <- ctxt(d$acc_thr, d$gert_acc); res$dec_dir <- ctxt(d$dec_thr, d$gert_acc)
res$spearman  <- ctxt(d$thresh, d$gert_acc, "spearman")
res$no_ceiling <- with(dplyr::filter(d, !ceiling), ctxt(thresh, gert_acc))
rel_r <- cor(d$acc_thr, d$dec_thr, use = "complete.obs")
res$reliability <- sprintf("accelerate vs decelerate threshold r = %.3f; Spearman-Brown reliability of their mean = %.3f",
                           rel_r, 2 * rel_r / (1 + rel_r))
res$disattenuated <- sprintf("H2B corrected for threshold unreliability (GERT reliability not corrected): r = %.3f",
                             cor(d$thresh, d$gert_acc, use = "complete.obs") / sqrt(2 * rel_r / (1 + rel_r)))

m0 <- lm(z(gert_acc) ~ z(thresh), data = d)
m_bias <- lm(z(gert_acc) ~ z(thresh) + z(fa) + z(base_miss_none), data = d)
m_eng  <- lm(z(gert_acc) ~ z(thresh) + z(fa) + z(base_miss_none) + z(base_conf), data = d)
m_cov  <- lm(z(gert_acc) ~ z(thresh) + z(fa) + z(base_miss_none) + z(base_conf) +
               z(MAIAtotal) + z(Age) + female + z(PHQ4total) + z(BATtotal), data = d)
db <- dplyr::filter(d, belt_ok)
m_belt0 <- lm(z(gert_acc) ~ z(thresh), data = db)
m_belt  <- lm(z(gert_acc) ~ z(thresh) + z(sync) + z(entrainment_gain) + z(fa), data = db)
m_rob   <- MASS::rlm(z(gert_acc) ~ z(thresh), data = d)
cooks   <- cooks.distance(m0); n_infl <- sum(cooks > 4 / length(cooks))
m_noinfl <- lm(z(gert_acc) ~ z(thresh), data = d[as.integer(names(cooks))[cooks <= 4 / length(cooks)], ])
# Calibration check: thresholds were used as the combined task's change sizes.
# If they were well estimated, combined-task detection should not depend on them.
res$calibration <- ctxt(d$thresh, d$comb_det)
res$fa_thresh   <- ctxt(d$fa, d$thresh)
res$fa_gert     <- ctxt(d$fa, d$gert_acc)
res$maia_thresh <- ctxt(d$MAIAtotal, d$thresh)

sink(file.path(outPath, "models.txt"))
cat("H2B robustness -", format(Sys.time()), "\n\n")
for (nm in names(res)) cat(sprintf("%-14s %s\n", nm, res[[nm]]))
for (nm in c("m0", "m_bias", "m_eng", "m_cov", "m_belt0", "m_belt")) { cat("\n==", nm, "==\n"); print(summary(get(nm))) }
cat("\n== robust (Huber) ==\n"); print(summary(m_rob))
cat(sprintf("\nInfluential points (Cook's D > 4/n): %d\n", n_infl)); print(summary(m_noinfl))
sink()

rob_t <- summary(m_rob)$coefficients["z(thresh)", ]
rep <- c("# H2B robustness: breath-change threshold and emotion recognition", "",
  sprintf("Run %s. Script `Analysis/h2b_robustness.R`; full output in `models.txt`. H2B is preregistered; these checks are exploratory, p values uncorrected.", format(Sys.Date())), "",
  "Higher threshold = worse breath-change sensitivity. Standardised betas for the threshold, with 95% CIs.", "",
  "| Check | Threshold effect on GERT accuracy |", "|---|---|",
  sprintf("| H2B as preregistered (baseline GERT) | %s |", res$h2b),
  sprintf("| **Replication: combined-task GERT, 60 different clips** | %s |", res$replicate),
  sprintf("| Accelerate threshold only | %s |", res$acc_dir),
  sprintf("| Decelerate threshold only | %s |", res$dec_dir),
  sprintf("| Rank correlation | %s |", res$spearman),
  sprintf("| Without the %d ceiling thresholds (1.0) | %s |", sum(d$ceiling, na.rm = TRUE), res$no_ceiling),
  sprintf("| Robust regression (Huber) | beta = %.3f, t = %.2f |", rob_t[1], rob_t[3]),
  sprintf("| Without %d influential points (Cook's D > 4/n) | %s |", n_infl, beta(m_noinfl)),
  sprintf("| + response bias (false alarms, 'no change' rate) | %s |", beta(m_bias)),
  sprintf("| + bias + confidence | %s |", beta(m_eng)),
  sprintf("| + bias, confidence, MAIA, age, gender, PHQ-4, burnout | %s |", beta(m_cov)),
  sprintf("| Belt follows pacer: unadjusted | %s |", beta(m_belt0)),
  sprintf("| Belt follows pacer: + synchrony, entrainment gain, false alarms | %s |", beta(m_belt)), "",
  "Measurement checks:", "",
  sprintf("- Threshold reliability: %s.", res$reliability),
  sprintf("- %s.", res$disattenuated),
  sprintf("- Calibration: threshold vs combined-task detection rate at that threshold: %s.", res$calibration),
  sprintf("- False-alarm rate vs threshold: %s; vs GERT accuracy: %s.", res$fa_thresh, res$fa_gert),
  sprintf("- MAIA vs threshold: %s.", res$maia_thresh))
writeLines(rep, file.path(outPath, "report.md"))

# Standardised threshold effects for the figure (Analysis/make_figures.R)
std_r <- function(x, y) { t <- cor.test(x, y); c(t$estimate, t$conf.int) }
std_b <- function(m, term = "z(thresh)") c(coef(m)[term], confint(m)[term, ])
fig_est <- rbind(
  c(check = "H2B as preregistered", std_r(d$thresh, d$gert_acc)),
  c(check = "Replication: 60 independent clips", std_r(d$thresh, d$gert_comb_acc)),
  c(check = "Accelerate threshold only", std_r(d$acc_thr, d$gert_acc)),
  c(check = "Decelerate threshold only", std_r(d$dec_thr, d$gert_acc)),
  c(check = "Without ceiling thresholds", with(dplyr::filter(d, !ceiling), std_r(thresh, gert_acc))),
  c(check = "Without influential points", std_b(m_noinfl)),
  c(check = "+ response bias", std_b(m_bias)),
  c(check = "+ bias, confidence, MAIA, demographics, PHQ-4, burnout", std_b(m_cov)),
  c(check = "Belt follows pacer, + compliance", std_b(m_belt)))
fig_est <- as.data.frame(fig_est, stringsAsFactors = FALSE)
names(fig_est) <- c("check", "estimate", "lo", "hi")
write.csv(fig_est, file.path(outPath, "estimates.csv"), row.names = FALSE)

f <- ggplot(d, aes(thresh, 100 * gert_acc)) +
  geom_point(aes(shape = ceiling), alpha = 0.6, colour = "#2E4A7A") +
  geom_smooth(method = "lm", formula = y ~ x, colour = "#2E4A7A", fill = "#9FB7D9") +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 4), labels = c("", "threshold at ceiling"), name = NULL) +
  labs(x = "Breath-change threshold (mean of accelerate and decelerate; higher = less sensitive)",
       y = "Emotion recognition accuracy, baseline (%)") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
ggsave(file.path(outPath, "fig_h2b.png"), f, width = 7, height = 5, dpi = 300)
message("Done: ", outPath)
