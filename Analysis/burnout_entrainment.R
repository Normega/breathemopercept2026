# burnout_entrainment.R
# Does burnout dampen the body's response to the breathing manipulation, or
# only the felt sense of it? Higher burnout goes with lower felt arousal
# (r = -.20). If high-burnout participants also followed the pacer less, the
# lower arousal could reflect a weaker physiological response; if they followed
# it just as closely, it reflects how the change was felt.
#
#   1. Burnout and how closely breathing followed the pacer: per-participant
#      entrainment gain (slope of observed on prescribed change) and direction
#      compliance, with TOST equivalence at |r| = .17 (the SESOI used for every
#      correlation in equivalence_tests.R). Split-half reliability of the gain
#      says whether a null is interpretable.
#   2. Trial level: does burnout moderate the observed-on-prescribed slope?
#   3. Felt arousal: does burnout still predict lower felt arousal with
#      entrainment held constant, and does it blunt the arousal response to the
#      prescribed change, or only lower its level?
#
# Input:  Results/physio/breath_trials.csv, breath_participants.csv (05c);
#         Results/questionnaireFile.csv, exclusions.csv
# Output: Results/burnout_entrainment/ (report.md, models.txt, estimates.csv)
#
# Exploratory (not preregistered); p values uncorrected.
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
outPath <- file.path(resultsPath, "burnout_entrainment")
if (!dir.exists(outPath)) dir.create(outPath, recursive = TRUE)
rd <- function(...) readr::read_csv(file.path(resultsPath, ...), col_types = readr::cols(id = "c", .default = "?"))
SYNC_MIN    <- 0.4       # belt follows the pacer (as breath_manipulation_check.R)
RATIO_LIMIT <- log(2)    # |log ratio| beyond this = breath-matching failure
MIN_MATCHED <- 2L
SESOI_R     <- 0.17      # as equivalence_tests.R
ctl <- lmerControl(optimizer = "bobyqa")

# Data ---------
excl  <- rd("exclusions.csv")
quest <- rd("questionnaireFile.csv") |> dplyr::mutate(id = sub("\\.0$", "", id)) |>
  dplyr::distinct(id, .keep_all = TRUE) |> dplyr::select(id, BATtotal)
parts <- rd("physio", "breath_participants.csv")
in_sample <- excl$id[excl$set_combined]
included  <- parts$id[parts$id %in% in_sample & is.finite(parts$sync) & parts$sync >= SYNC_MIN]

# Usable paced trials of the main task file (a restarted task leaves two files;
# keep the one with the most trials), as in breath_manipulation_check.R.
tr <- rd("physio", "breath_trials.csv") |>
  dplyr::filter(id %in% included, !is.na(high_salience)) |>
  dplyr::add_count(id, task, task_file, name = "n_in_file") |>
  dplyr::group_by(id, task) |>
  dplyr::filter(task_file == task_file[which.max(n_in_file)]) |>
  dplyr::ungroup() |>
  dplyr::mutate(lr_exp = log(exp_ratio), lr_obs = log(obs_ratio), change = direction != 0,
                spd_exp = -lr_exp, sal_e = ifelse(high_salience, 0.5, -0.5)) |>
  dplyr::filter(n_matched >= MIN_MATCHED, is.finite(lr_obs), abs(lr_obs) <= RATIO_LIMIT) |>
  dplyr::left_join(quest, by = "id") |>
  dplyr::filter(is.finite(BATtotal))
bat_mean <- mean(tr$BATtotal[!duplicated(tr$id)]); bat_sd <- sd(tr$BATtotal[!duplicated(tr$id)])
tr$bat_z <- (tr$BATtotal - bat_mean) / bat_sd
comb <- tr |> dplyr::filter(task == "combined")

# 1. Participant-level entrainment ---------
slope <- function(y, x) if (sum(is.finite(x) & is.finite(y)) >= 6 && sd(x) > 0) unname(coef(lm(y ~ x))[2]) else NA_real_
pp <- comb |>
  dplyr::group_by(id, BATtotal) |>
  dplyr::summarise(
    gain       = slope(lr_obs, lr_exp),
    gain_odd   = slope(lr_obs[trial %% 2 == 1], lr_exp[trial %% 2 == 1]),
    gain_even  = slope(lr_obs[trial %% 2 == 0], lr_exp[trial %% 2 == 0]),
    compliance = mean(sign(lr_obs[change]) == sign(lr_exp[change])),
    arousal    = mean(arousal, na.rm = TRUE),
    n_trials   = dplyr::n(), .groups = "drop") |>
  dplyr::left_join(parts |> dplyr::select(id, sync), by = "id") |>
  dplyr::filter(is.finite(gain))
r_half <- cor(pp$gain_odd, pp$gain_even, use = "complete.obs")
rel_gain <- 2 * r_half / (1 + r_half)            # Spearman-Brown

ct <- function(x, y) { t <- cor.test(x, y); c(r = unname(t$estimate), lo = t$conf.int[1], hi = t$conf.int[2], p = t$p.value, n = sum(is.finite(x) & is.finite(y))) }
tost_r <- function(x, y, bound = SESOI_R) {
  ok <- is.finite(x) & is.finite(y); n <- sum(ok); z <- atanh(cor(x[ok], y[ok])); se <- 1 / sqrt(n - 3)
  p <- max(pnorm((z + atanh(bound)) / se, lower.tail = FALSE), pnorm((z - atanh(bound)) / se))
  h <- qnorm(0.95) * se
  c(ci90_lo = tanh(z - h), ci90_hi = tanh(z + h), p_tost = p)
}
c_gain <- ct(pp$BATtotal, pp$gain);       e_gain <- tost_r(pp$BATtotal, pp$gain)
c_comp <- ct(pp$BATtotal, pp$compliance); e_comp <- tost_r(pp$BATtotal, pp$compliance)
c_sync <- ct(pp$BATtotal, pp$sync);       e_sync <- tost_r(pp$BATtotal, pp$sync)
c_ar   <- ct(pp$BATtotal, pp$arousal)
c_ar_g <- ct(pp$gain, pp$arousal)

# Does burnout -> felt arousal survive holding entrainment constant?
m_ar_pp <- lm(scale(arousal) ~ scale(BATtotal) + scale(gain) + scale(compliance), data = pp)

# 2. Trial level: burnout x prescribed change on observed change ---------
m_ent <- lmer(lr_obs ~ lr_exp * bat_z + (1 + lr_exp | id), data = comb, control = ctl)
m_ent_base <- lmer(lr_obs ~ lr_exp * bat_z + (1 + lr_exp | id),
                   data = dplyr::filter(tr, task == "bcat_baseline"), control = ctl)

# 3. Felt arousal: level vs response to the breathing change ---------
m_ar <- lmer(arousal ~ spd_exp * bat_z + sal_e + (1 + spd_exp | id), data = comb, control = ctl)
# Sensitivity: condition as direction (accelerate vs decelerate), change trials only
comb_chg <- comb |> dplyr::filter(change) |> dplyr::mutate(dir_e = ifelse(direction == -1, 0.5, -0.5))
m_ar_dir <- lmer(arousal ~ dir_e * bat_z + sal_e + (1 + dir_e | id), data = comb_chg, control = ctl)

sink(file.path(outPath, "models.txt"))
cat("Burnout and entrainment -", format(Sys.time()), "\n\n")
cat(sprintf("Participants (combined sample, belt follows pacer, burnout available): %d; usable combined trials %d\n",
            nrow(pp), nrow(comb)))
cat(sprintf("Burnout (BAT total) in this subsample: M = %.1f, SD = %.1f, range %.0f-%.0f\n\n",
            bat_mean, bat_sd, min(pp$BATtotal), max(pp$BATtotal)))
cat(sprintf("Per-participant gain: M = %.3f, SD = %.3f; split-half r = %.3f, Spearman-Brown = %.3f\n\n",
            mean(pp$gain), sd(pp$gain), r_half, rel_gain))
print(rbind(gain = c(c_gain, e_gain), compliance = c(c_comp, e_comp), sync = c(c_sync, e_sync)))
cat("\nBurnout - felt arousal:\n"); print(c_ar); cat("Gain - felt arousal:\n"); print(c_ar_g)
cat("\n== Felt arousal ~ burnout + gain + compliance (participant level, standardised) ==\n"); print(summary(m_ar_pp))
cat("\n== Combined task: lr_obs ~ lr_exp * bat_z + (1 + lr_exp | id) ==\n"); print(summary(m_ent))
cat("\n== Baseline BCAT: lr_obs ~ lr_exp * bat_z + (1 + lr_exp | id) ==\n"); print(summary(m_ent_base))
cat("\n== Felt arousal: arousal ~ spd_exp * bat_z + sal + (1 + spd_exp | id) ==\n"); print(summary(m_ar))
cat("\n== Sensitivity: arousal ~ dir * bat_z + sal + (1 + dir | id), change trials ==\n"); print(summary(m_ar_dir))
sink()

# Report ---------
fp <- function(p) if (p < .001) "p < .001" else sprintf("p = %s", sub("^0", "", sprintf("%.3f", p)))
fr <- function(c, e) sprintf("r = %s, 95%% CI [%s, %s], %s, n = %d; 90%% CI [%s, %s], equivalence %s",
  sub("^(-?)0", "\\1", sprintf("%.3f", c["r"])), sub("^(-?)0", "\\1", sprintf("%.3f", c["lo"])),
  sub("^(-?)0", "\\1", sprintf("%.3f", c["hi"])), fp(c["p"]), as.integer(c["n"]),
  sub("^(-?)0", "\\1", sprintf("%.3f", e["ci90_lo"])), sub("^(-?)0", "\\1", sprintf("%.3f", e["ci90_hi"])),
  if (e["p_tost"] < .05) sprintf("yes (p_TOST %s)", sub("^p ", "", fp(e["p_tost"]))) else sprintf("not shown (p_TOST = %.3f)", e["p_tost"]))
fx <- function(m, term) {
  s <- summary(m)$coefficients
  ci <- tryCatch(suppressMessages(confint(m, parm = term, method = "Wald")), error = function(e) c(NA, NA))
  sprintf("b = %.3f, 95%% CI [%.3f, %.3f], %s", s[term, 1], ci[1], ci[2], fp(s[term, ncol(s)]))
}
rep <- c(
  "# Burnout and entrainment", "",
  sprintf("Run %s. Script `Analysis/burnout_entrainment.R`; full output in `models.txt`.", format(Sys.Date())),
  "Exploratory (not preregistered); p values uncorrected. Breathing change is the four-cycle measure (`Methods/breath_trial_features.md`).", "",
  "## Sample", "",
  sprintf("- %d combined-task participants whose belt followed the pacer and who have a burnout score; %d usable paced trials.", nrow(pp), nrow(comb)),
  sprintf("- Burnout (BAT total) here: M = %.1f, SD = %.1f, range %.0f-%.0f.", bat_mean, bat_sd, min(pp$BATtotal), max(pp$BATtotal)),
  sprintf("- Per-participant entrainment gain: M = %.2f, SD = %.2f; split-half reliability (Spearman-Brown) %.2f.", mean(pp$gain), sd(pp$gain), rel_gain), "",
  "## Did burnout weaken the body's response to the pacer?", "",
  sprintf("- Burnout and entrainment gain: %s.", fr(c_gain, e_gain)),
  sprintf("- Burnout and direction compliance: %s.", fr(c_comp, e_comp)),
  sprintf("- Burnout and belt-pacer synchrony: %s.", fr(c_sync, e_sync)),
  sprintf("- Trial level, combined task, burnout (per SD) x prescribed change: %s (main effect of prescribed change, i.e. the gain at mean burnout: %s).",
          fx(m_ent, "lr_exp:bat_z"), fx(m_ent, "lr_exp")),
  sprintf("- Trial level, baseline BCAT: burnout x prescribed change %s.", fx(m_ent_base, "lr_exp:bat_z")), "",
  "## Felt arousal", "",
  sprintf("- Burnout and mean felt arousal in this subsample: r = %.3f, %s.", c_ar["r"], fp(c_ar["p"])),
  sprintf("- Entrainment gain and mean felt arousal: r = %.3f, %s.", c_ar_g["r"], fp(c_ar_g["p"])),
  sprintf("- Holding gain and compliance constant, burnout still predicted lower felt arousal: standardised beta = %.3f, %s.",
          coef(m_ar_pp)[2], fp(summary(m_ar_pp)$coefficients[2, 4])),
  sprintf("- Trial level: felt arousal rose with the prescribed speeding (%s). Burnout (per SD) lowered its level: %s. Burnout x prescribed speeding: %s.",
          fx(m_ar, "spd_exp"), fx(m_ar, "bat_z"), fx(m_ar, "spd_exp:bat_z")),
  sprintf("- Sensitivity, change trials, direction coding: accelerate vs decelerate %s; burnout %s; burnout x direction %s.",
          fx(m_ar_dir, "dir_e"), fx(m_ar_dir, "bat_z"), fx(m_ar_dir, "dir_e:bat_z"))
)
writeLines(rep, file.path(outPath, "report.md"))

est <- data.frame(
  estimate = c("burnout_gain_r", "burnout_compliance_r", "burnout_sync_r", "burnout_x_prescribed_on_observed",
               "burnout_on_arousal_level_per_sd", "burnout_x_prescribed_on_arousal"),
  b = c(c_gain["r"], c_comp["r"], c_sync["r"], fixef(m_ent)["lr_exp:bat_z"], fixef(m_ar)["bat_z"], fixef(m_ar)["spd_exp:bat_z"]),
  p = c(c_gain["p"], c_comp["p"], c_sync["p"], summary(m_ent)$coefficients["lr_exp:bat_z", 5],
        summary(m_ar)$coefficients["bat_z", 5], summary(m_ar)$coefficients["spd_exp:bat_z", 5]))
write.csv(est, file.path(outPath, "estimates.csv"), row.names = FALSE)
message("Done: ", outPath)
