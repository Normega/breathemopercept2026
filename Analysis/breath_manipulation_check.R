# breath_manipulation_check.R
# Physiological manipulation check for the BCAT: did breathing follow the
# pacer, did it change by condition, and does the change people actually made
# (not the one the pacer prescribed) predict detection and felt arousal?
#
# Input:  Results/physio/breath_trials.csv, breath_participants.csv (05c)
# Output: Results/breath_manipulation/ (models.txt, report.md, figures)
#
# Exploratory: the preregistration (osf.io/wz32n) lists physiological
# measures but no breathing-compliance hypotheses. p values are uncorrected.
# ---------------------------------------------------------------

# Set Up ---------
## Load libraries ---------
packages <- c("tidyverse", "lme4", "lmerTest", "patchwork")
new_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
if (length(new_packages)) install.packages(new_packages)
options(readr.show_col_types = FALSE)
for (thispack in packages) {
  library(thispack, character.only = TRUE, quietly = TRUE, verbose = FALSE)
}

## Paths ---------
if (!exists("mainPath")) mainPath <- "I:/Shared drives/Aya/"
resultsPath <- file.path(mainPath, "Repo", "Results")
physioQCPath <- file.path(resultsPath, "physio")
outPath <- file.path(resultsPath, "breath_manipulation")
if (!dir.exists(outPath)) dir.create(outPath, recursive = TRUE)

## Constants ---------
SYNC_MIN    <- 0.4        # belt follows the pacer (bimodal split; see report)
RATIO_LIMIT <- log(2)     # |log ratio| beyond this = breath-matching failure
MIN_MATCHED <- 2L         # pacer cycles matched (obs_ratio also needs onsets 1 or 2, 3, and peak 4)

# Data ---------
parts  <- readr::read_csv(file.path(physioQCPath, "breath_participants.csv"))
trials <- readr::read_csv(file.path(physioQCPath, "breath_trials.csv"),
                          col_types = readr::cols(id = "c", .default = "?"))
parts$id <- as.character(parts$id)

# The same participants as every other combined-task analysis (R/06_exclusions.R)
excl <- readr::read_csv(file.path(resultsPath, "exclusions.csv"), col_types = readr::cols(id = "c", .default = "?"))
in_sample <- excl$id[excl$set_combined]
parts <- parts |> dplyr::filter(id %in% in_sample)
included <- parts |> dplyr::filter(!is.na(sync), sync >= SYNC_MIN) |> dplyr::pull(id)

# A restarted task leaves two matched files for one participant; keep the one
# with the most trials (the completed run).
# Breathing change (05c): mean cycle length from the cycle-3 onset to the
# cycle-4 inhale peak against cycles 1-2, observed and prescribed. The earlier
# cycle-3-only measure (obs_ratio3) is refitted below as a sensitivity check.
prep <- function(trials, obs_col = "obs_ratio", exp_col = "exp_ratio") trials |>
  dplyr::filter(id %in% included) |>
  dplyr::add_count(id, task, task_file, name = "n_in_file") |>
  dplyr::group_by(id, task) |>
  dplyr::filter(task_file == task_file[which.max(n_in_file)]) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    lr_exp  = log(.data[[exp_col]]),   # prescribed log change in cycle duration (post/pre)
    lr_obs  = log(.data[[obs_col]]),   # observed
    spd_exp = -lr_exp,                 # positive = faster
    spd_obs = -lr_obs,
    change  = direction != 0,
    dir_e   = dplyr::case_when(direction == -1 ~ 0.5, direction == 1 ~ -0.5),   # +acc / -dec
    sal_e   = ifelse(high_salience, 0.5, -0.5),
    sal_ok  = !is.na(high_salience),
    usable  = sal_ok & n_matched >= MIN_MATCHED & is.finite(lr_obs) & abs(lr_obs) <= RATIO_LIMIT,
    # change actually made in the prescribed direction (positive = followed)
    aligned = ifelse(change, lr_obs * sign(lr_exp), NA_real_)
  )
# Usable trials, with within-person centring for the measured predictors
usable_trials <- function(d) d |>
  dplyr::filter(usable) |>
  dplyr::group_by(id, task) |>
  dplyr::mutate(
    aligned_pm = mean(aligned, na.rm = TRUE), aligned_pc = aligned - aligned_pm,
    spd_obs_pm = mean(spd_obs, na.rm = TRUE), spd_obs_pc = spd_obs - spd_obs_pm
  ) |>
  dplyr::ungroup()

d <- prep(trials)
n_trial_all <- nrow(d)
n_unusable  <- sum(!d$usable)
du <- usable_trials(d)

comb <- du |> dplyr::filter(task == "combined")
base <- du |> dplyr::filter(task == "bcat_baseline")

sink(file.path(outPath, "models.txt"))
cat("Breath manipulation check -", format(Sys.time()), "\n\n")
cat(sprintf("Participants with physio: %d; belt follows pacer (sync >= %.1f): %d\n",
            nrow(parts), SYNC_MIN, length(included)))
cat(sprintf("Trials from included participants: %d; unusable (matched < %d or |log ratio| > log 2): %d\n\n",
            n_trial_all, MIN_MATCHED, n_unusable))

# M1 Entrainment: observed vs prescribed change, all paced trials ---------
m1 <- lmer(lr_obs ~ lr_exp + (1 + lr_exp | id), data = du,
           control = lmerControl(optimizer = "bobyqa"))
cat("== M1 entrainment: lr_obs ~ lr_exp + (1 + lr_exp | id), both tasks ==\n"); print(summary(m1))

# M2 Did breathing change by condition? Combined task, change trials ---------
m2 <- lmer(spd_obs ~ dir_e * sal_e + (1 | id), data = dplyr::filter(comb, change))
cat("\n== M2 condition: spd_obs ~ dir * sal + (1 | id), combined change trials ==\n"); print(summary(m2))
cell <- comb |>
  dplyr::mutate(cond = ifelse(change, paste(ifelse(direction == -1, "accel", "decel"),
                                            ifelse(high_salience, "high", "low")), "no change")) |>
  dplyr::group_by(cond) |>
  dplyr::summarise(n = dplyr::n(),
                   prescribed_pct = 100 * (exp(mean(lr_exp)) - 1),
                   observed_pct   = 100 * (exp(mean(lr_obs)) - 1),
                   followed_dir   = mean(sign(lr_obs) == sign(lr_exp)),
                   .groups = "drop")
cat("\nCell means (cycle-duration change, cycle-3 onset to cycle-4 peak vs cycles 1-2; negative = faster):\n"); print(as.data.frame(cell))

# M2b Same total change for the gradual ramp as for the step? ---------
m2b <- lmer(lr_obs ~ lr_exp * sal_e + (1 | id), data = comb)
cat("\n== M2b: lr_obs ~ lr_exp * sal + (1 | id), combined ==\n"); print(summary(m2b))

# M3 Does the change actually made predict detection? ---------
# In the combined task the prescribed magnitude is fixed per participant and
# direction (their threshold), so trial-to-trial variation in `aligned` is
# the body's, not the pacer's.
m3 <- glmer(accuracy ~ aligned_pc + aligned_pm + dir_e * sal_e + (1 | id),
            data = dplyr::filter(comb, change), family = binomial,
            control = glmerControl(optimizer = "bobyqa"))
cat("\n== M3 detection (combined): accuracy ~ aligned_pc + aligned_pm + dir * sal + (1 | id) ==\n"); print(summary(m3))
# Baseline: QUEST varies the prescribed level, so control it explicitly.
m3b <- glmer(accuracy ~ aligned_pc + aligned_pm + abs(lr_exp) + dir_e + (1 | id),
             data = dplyr::filter(base, change), family = binomial,
             control = glmerControl(optimizer = "bobyqa"))
cat("\n== M3b detection (baseline): accuracy ~ aligned_pc + aligned_pm + |lr_exp| + dir + (1 | id) ==\n"); print(summary(m3b))

# M4 Does the change actually made predict felt arousal? ---------
m4 <- lmer(arousal ~ spd_exp + sal_e + spd_obs_pc + spd_obs_pm + (1 | id), data = comb)
cat("\n== M4 arousal (combined, all trials): arousal ~ spd_exp + sal + spd_obs_pc + spd_obs_pm + (1 | id) ==\n"); print(summary(m4))

# M5 Breath depth: does faster breathing get shallower? ---------
m5 <- lmer(log(amp_ratio) ~ dir_e * sal_e + (1 | id),
           data = dplyr::filter(comb, change, is.finite(log(amp_ratio))))
cat("\n== M5 depth: log(amp_ratio) ~ dir * sal + (1 | id), combined change trials ==\n"); print(summary(m5))

# Sensitivity: the cycle-3-only measure ---------
du3 <- usable_trials(prep(trials, "obs_ratio3", "exp_ratio3"))
comb3 <- du3 |> dplyr::filter(task == "combined"); base3 <- du3 |> dplyr::filter(task == "bcat_baseline")
s1 <- lmer(lr_obs ~ lr_exp + (1 + lr_exp | id), data = du3, control = lmerControl(optimizer = "bobyqa"))
s3 <- glmer(accuracy ~ aligned_pc + aligned_pm + dir_e * sal_e + (1 | id), data = dplyr::filter(comb3, change),
            family = binomial, control = glmerControl(optimizer = "bobyqa"))
s3b <- glmer(accuracy ~ aligned_pc + aligned_pm + abs(lr_exp) + dir_e + (1 | id), data = dplyr::filter(base3, change),
             family = binomial, control = glmerControl(optimizer = "bobyqa"))
s4 <- lmer(arousal ~ spd_exp + sal_e + spd_obs_pc + spd_obs_pm + (1 | id), data = comb3)
cat("\n== Sensitivity, cycle 3 vs cycles 1-2: M1, M3, M3b, M4 ==\n")
print(summary(s1)$coefficients); print(summary(s3)$coefficients)
print(summary(s3b)$coefficients); print(summary(s4)$coefficients)
sink()

# Report ---------
fx <- function(m, term) {
  s <- summary(m)$coefficients
  ci <- tryCatch(suppressMessages(confint(m, parm = term, method = "Wald")), error = function(e) c(NA, NA))
  p <- s[term, ncol(s)]
  sprintf("b = %.3f, 95%% CI [%.3f, %.3f], p = %s", s[term, 1], ci[1], ci[2],
          if (p < .001) formatC(p, format = "e", digits = 1) else sprintf("%.3f", p))
}
or <- function(m, term) sprintf("OR per 0.1 log-unit = %.2f", exp(0.1 * fixef(m)[term]))
gain_sd <- attr(VarCorr(m1)$id, "stddev")["lr_exp"]

rep <- c(
  "# Breath manipulation check", "",
  sprintf("Run %s. Script `Analysis/breath_manipulation_check.R`; full output in `models.txt`.", format(Sys.Date())),
  "Exploratory (not preregistered); p values uncorrected.", "",
  "## Sample", "",
  sprintf("- %d participants in the combined-task sample with physio; %d have a belt that follows the pacer (median belt-pacer correlation >= %.1f). The distribution is bimodal: belts either do not follow at all (about 0) or follow well (mostly .6-.9).",
          nrow(parts), length(included), SYNC_MIN),
  sprintf("- %d paced trials from those participants; %d (%.1f%%) dropped because fewer than %d of 4 pacer cycles were matched to a breath, or the observed change was beyond a factor of 2.",
          n_trial_all, n_unusable, 100 * n_unusable / n_trial_all, MIN_MATCHED),
  "", "## Results", "",
  sprintf("1. **Entrainment.** Observed change tracks prescribed change with gain %s (1 = perfect following; between-person SD of the gain %.2f).", fx(m1, "lr_exp"), gain_sd),
  sprintf("2. **Direction.** Acceleration trials sped breathing relative to deceleration trials: %s (log units of cycle duration).", fx(m2, "dir_e")),
  sprintf("   Salience: %s; direction x salience %s.", fx(m2, "sal_e"), fx(m2, "dir_e:sal_e")),
  sprintf("3. **Ramp vs step.** Gain differs by salience (lr_exp x sal): %s.", fx(m2b, "lr_exp:sal_e")),
  sprintf("4. **Detection follows the body.** Holding the prescribed change fixed, trials where the participant's breathing changed more in the prescribed direction were detected more often: combined task %s (%s); baseline %s (%s).",
          fx(m3, "aligned_pc"), or(m3, "aligned_pc"), fx(m3b, "aligned_pc"), or(m3b, "aligned_pc")),
  sprintf("5. **Felt arousal and the body.** Felt arousal rose with the prescribed speed change (%s); the participant's own speeding of breathing beyond it (within person): %s.",
          fx(m4, "spd_exp"), fx(m4, "spd_obs_pc")),
  sprintf("6. **Depth.** Breath amplitude ratio (post/pre) by direction: %s.", fx(m5, "dir_e")),
  "", "Breathing change is measured from the cycle-3 onset to the cycle-4 inhale peak (the last paced event), against cycles 1-2; see `Methods/breath_trial_features.md`.", "",
  "**Sensitivity: cycle 3 only against cycles 1-2** (the earlier measure):", "",
  sprintf("- Entrainment gain %s.", fx(s1, "lr_exp")),
  sprintf("- Detection follows the body: combined %s (%s); baseline %s (%s).",
          fx(s3, "aligned_pc"), or(s3, "aligned_pc"), fx(s3b, "aligned_pc"), or(s3b, "aligned_pc")),
  sprintf("- Own speeding of breathing -> felt arousal: %s.", fx(s4, "spd_obs_pc")),
  "", "Cell means (percent change in mean cycle length, cycle-3 onset to cycle-4 peak vs cycles 1-2):", "",
  paste(capture.output(print(as.data.frame(cell), digits = 3, row.names = FALSE)), collapse = "\n")
)
writeLines(rep, file.path(outPath, "report.md"))

# Figure ---------
p1 <- ggplot(parts |> dplyr::filter(!is.na(sync)), aes(sync)) +
  geom_histogram(binwidth = 0.05, boundary = 0, fill = "#2E4A7A") +
  geom_vline(xintercept = SYNC_MIN, linetype = 2) +
  labs(x = "Belt-pacer synchrony (median r over trials)", y = "Participants") +
  theme_minimal(base_size = 12)
p2 <- ggplot(du |> dplyr::filter(task == "combined"),
             aes(100 * (exp(lr_exp) - 1), 100 * (exp(lr_obs) - 1))) +
  geom_point(alpha = 0.05, size = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "#D7191C") +
  labs(x = "Prescribed change in cycle duration (%)", y = "Observed change (%)") +
  theme_minimal(base_size = 12)
ggsave(file.path(outPath, "fig_entrainment.png"), p1 + p2, width = 10, height = 4, dpi = 300)

message("Done: ", outPath)
