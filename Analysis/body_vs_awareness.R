# body_vs_awareness.R
# Does perceived emotion intensity follow the breathing change a participant
# actually made, their awareness of it, or both?
#
# Combined task, block level, within person. Each block has 2 change trials
# (+1 no-change catch trial). For each block:
#   awareness  detection rate on its 2 change trials (0, .5, 1)
#   body       the change actually made, in the prescribed direction:
#              log(observed cycle-3 / cycles-1-2 ratio) x sign(prescribed),
#              averaged over the block's change trials with a usable breath
#              measure (05c); positive = breathing followed the pacer
#   arousal    mean felt arousal over the block's 3 trials
# Outcome: intensity of each of the 5 GERT clips that follow the block, with
# random intercepts for participant and clip. Every predictor is split into a
# within-person (block deviation) and a between-person (person mean) part.
#
# Sample: combined-task sample (R/06) whose belt follows the pacer (05c).
# Input:  Results/ CSVs written by the pipeline. Output: Results/body_vs_awareness/
# Exploratory (not preregistered); p values uncorrected.
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

## Paths and constants ---------
if (!exists("mainPath")) mainPath <- "I:/Shared drives/Aya/"
resultsPath <- file.path(mainPath, "Repo", "Results")
outPath <- file.path(resultsPath, "body_vs_awareness")
if (!dir.exists(outPath)) dir.create(outPath, recursive = TRUE)
SYNC_MIN    <- 0.4
RATIO_LIMIT <- log(2)
rd <- function(...) readr::read_csv(file.path(resultsPath, ...), col_types = readr::cols(id = "c", .default = "?"))

# Data ---------
excl  <- rd("exclusions.csv")
bpart <- rd("physio", "breath_participants.csv")
ids   <- intersect(excl$id[excl$set_combined], bpart$id[!is.na(bpart$sync) & bpart$sync >= SYNC_MIN])

btr <- rd("physio", "breath_trials.csv") |>
  dplyr::filter(task == "combined", id %in% ids) |>
  # a restarted task leaves two files; keep the completed run
  dplyr::add_count(id, task_file, name = "n_in_file") |>
  dplyr::group_by(id) |> dplyr::filter(task_file == task_file[which.max(n_in_file)]) |> dplyr::ungroup()

blocks <- btr |>
  dplyr::mutate(change = direction != 0,
                lr_obs = log(obs_ratio), lr_exp = log(exp_ratio),
                ok = change & n_matched >= 2 & is.finite(lr_obs) & abs(lr_obs) <= RATIO_LIMIT,
                followed = ifelse(ok, lr_obs * sign(lr_exp), NA_real_)) |>
  dplyr::group_by(id, block) |>
  dplyr::summarise(
    dir_e   = ifelse(any(direction == -1), 0.5, -0.5),          # +accelerate / -decelerate
    sal_e   = ifelse(all(high_salience[change]), 0.5, -0.5),
    aware   = mean(accuracy[change], na.rm = TRUE),             # 0, .5, 1
    aware3  = mean(accuracy, na.rm = TRUE) >= 2/3,              # thesis definition (incl. catch)
    body    = mean(followed, na.rm = TRUE),
    n_body  = sum(ok),
    arousal = mean(arousal, na.rm = TRUE),
    .groups = "drop") |>
  dplyr::filter(n_body >= 1, is.finite(aware), is.finite(arousal)) |>
  dplyr::group_by(id) |>
  dplyr::mutate(dplyr::across(c(aware, body, arousal), list(pm = ~ mean(.x), pc = ~ .x - mean(.x)))) |>
  dplyr::ungroup()

clips <- rd("Combined_fullGERT_data.csv") |>
  dplyr::filter(id %in% ids) |>
  dplyr::select(id, Block, FileName, IntensityRating, emoAccuracy) |>
  dplyr::inner_join(blocks, by = c("id", "Block" = "block"))

# Models ---------
fx <- function(m, term) {
  s <- summary(m)$coefficients; p <- s[term, ncol(s)]
  ci <- suppressMessages(confint(m, parm = term, method = "Wald"))
  c(b = s[term, 1], se = s[term, 2], lo = ci[1], hi = ci[2], p = p)
}
txt <- function(v) sprintf("b = %.3f, 95%% CI [%.3f, %.3f], p = %s", v["b"], v["lo"], v["hi"],
                           if (v["p"] < .001) formatC(v["p"], format = "e", digits = 1) else sprintf("%.3f", v["p"]))
ctl <- lmerControl(optimizer = "bobyqa")
f_rand <- "+ dir_e * sal_e + (1 | id) + (1 | FileName)"

m_aware <- lmer(as.formula(paste("IntensityRating ~ aware_pc + aware_pm", f_rand)), data = clips, control = ctl)
m_body  <- lmer(as.formula(paste("IntensityRating ~ body_pc + body_pm", f_rand)), data = clips, control = ctl)
m_both  <- lmer(as.formula(paste("IntensityRating ~ aware_pc + body_pc + aware_pm + body_pm", f_rand)), data = clips, control = ctl)
m_int   <- lmer(as.formula(paste("IntensityRating ~ aware_pc * body_pc + aware_pm + body_pm", f_rand)), data = clips, control = ctl)
m_ar    <- lmer(as.formula(paste("IntensityRating ~ aware_pc + body_pc + arousal_pc + aware_pm + body_pm + arousal_pm", f_rand)),
                data = clips, control = ctl)
# Sensitivity: by-participant random slope for felt arousal (the arousal-intensity
# link varies between people; clip-level models without it overstate precision)
m_ar_s  <- lmer(as.formula(paste("IntensityRating ~ aware_pc + body_pc + arousal_pc + aware_pm + body_pm + arousal_pm",
                                 "+ dir_e * sal_e + (1 + arousal_pc | id) + (1 | FileName)")), data = clips, control = ctl)
p_ar_s  <- lmer(arousal ~ body_pc + aware_pc + body_pm + aware_pm + dir_e * sal_e + (1 + aware_pc | id), data = blocks, control = ctl)
p_aw_s  <- lmer(aware ~ body_pc + body_pm + dir_e * sal_e + (1 + body_pc | id), data = blocks, control = ctl)
m_aware3 <- lmer(as.formula(paste("IntensityRating ~ aware3 + body_pc + body_pm", f_rand)), data = clips, control = ctl)
m_acc   <- glmer(emoAccuracy ~ aware_pc + body_pc + aware_pm + body_pm + dir_e * sal_e + (1 | id) + (1 | FileName),
                 data = clips, family = binomial, control = glmerControl(optimizer = "bobyqa"))
# Paths at block level: does the body's change drive awareness and felt arousal?
p_aware <- lmer(aware ~ body_pc + body_pm + dir_e * sal_e + (1 | id), data = blocks, control = ctl)
p_ar    <- lmer(arousal ~ body_pc + aware_pc + body_pm + aware_pm + dir_e * sal_e + (1 | id), data = blocks, control = ctl)

# Indirect effects, Monte Carlo (20,000 draws)
set.seed(2026)
mc <- function(a, b) { d <- rnorm(2e4, a["b"], a["se"]) * rnorm(2e4, b["b"], b["se"])
  c(est = unname(a["b"] * b["b"]), lo = unname(quantile(d, .025)), hi = unname(quantile(d, .975))) }
ind_aware <- mc(fx(p_aware, "body_pc"), fx(m_both, "aware_pc"))
ind_ar    <- mc(fx(p_ar, "body_pc"), fx(m_ar, "arousal_pc"))
# Serial chain body -> awareness -> felt arousal -> intensity
mc3 <- function(a, b, c) { d <- rnorm(2e4, a["b"], a["se"]) * rnorm(2e4, b["b"], b["se"]) * rnorm(2e4, c["b"], c["se"])
  c(est = unname(a["b"] * b["b"] * c["b"]), lo = unname(quantile(d, .025)), hi = unname(quantile(d, .975))) }
ind_chain <- mc3(fx(p_aware, "body_pc"), fx(p_ar, "aware_pc"), fx(m_ar, "arousal_pc"))
body_sd <- sd(blocks$body_pc)
ind_chain_s <- mc3(fx(p_aw_s, "body_pc"), fx(p_ar_s, "aware_pc"), fx(m_ar_s, "arousal_pc"))

sink(file.path(outPath, "models.txt"))
cat("Body vs awareness -", format(Sys.time()), "\n")
cat(sprintf("Participants %d, blocks %d, clips %d\n\n", length(unique(clips$id)), nrow(blocks), nrow(clips)))
for (nm in c("m_aware", "m_body", "m_both", "m_int", "m_ar", "m_ar_s", "m_aware3", "m_acc", "p_aware", "p_ar", "p_aw_s", "p_ar_s")) {
  cat("==", nm, "==\n"); print(summary(get(nm))$coefficients); cat("\n") }
cat("Indirect body -> awareness -> intensity:", round(ind_aware, 5), "\n")
cat("Indirect body -> felt arousal -> intensity:", round(ind_ar, 5), "\n")
sink()

# Report ---------
v <- list(aw = fx(m_aware, "aware_pc"), bo = fx(m_body, "body_pc"),
          aw2 = fx(m_both, "aware_pc"), bo2 = fx(m_both, "body_pc"),
          int = fx(m_int, "aware_pc:body_pc"),
          aw3 = fx(m_ar, "aware_pc"), bo3 = fx(m_ar, "body_pc"), ar3 = fx(m_ar, "arousal_pc"),
          awb = fx(m_aware3, "aware3TRUE"), acc = fx(m_acc, "aware_pc"),
          pa = fx(p_aware, "body_pc"), par_b = fx(p_ar, "body_pc"), par_a = fx(p_ar, "aware_pc"))
ci3 <- function(x) sprintf("%.4f [%.4f, %.4f]", x["est"], x["lo"], x["hi"])
rep <- c("# Body vs awareness: what drives perceived intensity?", "",
  sprintf("Run %s. Script `Analysis/body_vs_awareness.R`; full output in `models.txt`. Exploratory; p values uncorrected.", format(Sys.Date())), "",
  sprintf("%d participants (combined-task sample, belt follows pacer), %d blocks, %d clips. Clip-level models with random intercepts for participant and clip; predictors split within / between person; all control direction x salience.",
          length(unique(clips$id)), nrow(blocks), nrow(clips)), "",
  "Predictors (within person): **awareness** = share of the block's 2 changes detected; **body** = log change in breath-cycle duration actually made in the prescribed direction (positive = followed the pacer); **arousal** = felt arousal.", "",
  "| Model | Awareness | Body | Felt arousal |", "|---|---|---|---|",
  sprintf("| Awareness alone | %s | | |", txt(v$aw)),
  sprintf("| Body alone | | %s | |", txt(v$bo)),
  sprintf("| Both | %s | %s | |", txt(v$aw2), txt(v$bo2)),
  sprintf("| Both + felt arousal | %s | %s | %s |", txt(v$aw3), txt(v$bo3), txt(v$ar3)), "",
  sprintf("- Awareness x body interaction: %s.", txt(v$int)),
  sprintf("- Thesis awareness definition (>= 2/3 of all 3 trials), with body: %s.", txt(v$awb)),
  sprintf("- Recognition accuracy (logit) on awareness, with body: %s.", txt(v$acc)),
  "", "Paths (block level, within person):", "",
  sprintf("- Body -> awareness: %s (detection rate per log-unit).", txt(v$pa)),
  sprintf("- Body -> felt arousal, holding awareness: %s; awareness -> felt arousal: %s.", txt(v$par_b), txt(v$par_a)),
  sprintf("- Indirect body -> awareness -> intensity: %s; body -> felt arousal -> intensity: %s; serial body -> awareness -> felt arousal -> intensity: %s (Monte Carlo 95%% CI).",
          ci3(ind_aware), ci3(ind_ar), ci3(ind_chain)),
  "", "Random-slope sensitivity (each path with a by-participant slope for its predictor):", "",
  sprintf("- Body -> awareness: %s; awareness -> felt arousal: %s; felt arousal -> intensity: %s.",
          txt(fx(p_aw_s, "body_pc")), txt(fx(p_ar_s, "aware_pc")), txt(fx(m_ar_s, "arousal_pc"))),
  sprintf("- Serial body -> awareness -> felt arousal -> intensity with slopes: %s.", ci3(ind_chain_s)),
  "", sprintf("Scale: the within-person SD of the body measure is %.3f log-units (about %.0f%% in cycle duration). Per SD, the body effect on intensity in the 'Both' model is %.4f [%.4f, %.4f] scale points.",
          body_sd, 100 * (exp(body_sd) - 1), v$bo2["b"] * body_sd, v$bo2["lo"] * body_sd, v$bo2["hi"] * body_sd))
writeLines(rep, file.path(outPath, "report.md"))

# Key estimates for equivalence testing (Analysis/equivalence_tests.R)
est_row <- function(name, m, term, scale = 1, unit = "") {
  s <- summary(m)$coefficients
  data.frame(estimate = name, b = s[term, 1] * scale, se = s[term, 2] * scale, df = s[term, "df"],
             p = s[term, ncol(s)], unit = unit)
}
write.csv(rbind(
  est_row("body_change_per_sd_on_intensity", m_both, "body_pc", body_sd, "intensity scale points per within-person SD of breathing change"),
  est_row("awareness_on_intensity_clip", m_both, "aware_pc", 1, "intensity scale points, 0 to 2 of 2 changes detected"),
  est_row("path_body_to_awareness_slope", p_aw_s, "body_pc", 1, "detection rate per log-unit of breathing change"),
  est_row("path_awareness_to_arousal_slope", p_ar_s, "aware_pc", 1, "felt-arousal points, none to all changes detected"),
  est_row("path_body_to_arousal_slope", p_ar_s, "body_pc", 1, "felt-arousal points per log-unit, holding awareness"),
  est_row("path_arousal_to_intensity_slope", m_ar_s, "arousal_pc", 1, "intensity points per felt-arousal point"),
  est_row("path_awareness_to_intensity_slope", m_ar_s, "aware_pc", 1, "intensity points, holding arousal"),
  est_row("path_body_to_intensity_slope", m_ar_s, "body_pc", 1, "intensity points per log-unit, holding awareness and arousal")),
  file.path(outPath, "estimates.csv"), row.names = FALSE)

# Figure: intensity by awareness, and by bodily change (within-person bins) ---------
fig_d <- clips |>
  dplyr::group_by(id) |>
  dplyr::mutate(int_pc = IntensityRating - mean(IntensityRating, na.rm = TRUE),
                body_bin = dplyr::ntile(body_pc, 3)) |> dplyr::ungroup()
f1 <- fig_d |> dplyr::group_by(id, aware) |> dplyr::summarise(y = mean(int_pc, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(factor(aware, labels = c("0 of 2", "1 of 2", "2 of 2")), y)) +
  geom_hline(yintercept = 0, linetype = 2) + stat_summary(fun.data = mean_cl_normal, colour = "#2E4A7A") +
  labs(x = "Changes detected in the block", y = "Clip intensity, person-centred") + theme_minimal(base_size = 12)
f2 <- fig_d |> dplyr::group_by(id, body_bin) |> dplyr::summarise(y = mean(int_pc, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(factor(body_bin, labels = c("Least", "Middle", "Most")), y)) +
  geom_hline(yintercept = 0, linetype = 2) + stat_summary(fun.data = mean_cl_normal, colour = "#2E4A7A") +
  labs(x = "Breathing change made (within-person tertile)", y = NULL) + theme_minimal(base_size = 12)
ggsave(file.path(outPath, "fig_body_vs_awareness.png"), f1 + f2, width = 10, height = 4, dpi = 300)
message("Done: ", outPath)
