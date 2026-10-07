# cardiac_analysis.R
# Cardiac measures against the study's questions:
#   E1A (prereg, exploratory)  resting HRV ~ baseline emotion-recognition accuracy
#   E1B (prereg, exploratory)  HRV higher in the combined task than in baseline GERT
#   HR response to paced breathing change, and to the change actually made
#   Felt arousal ~ the heart's response (does felt arousal track physiology?)
#   Burnout ~ resting HR/HRV and HR response (blunted physiology or perception?)
#   Heartbeat-counting accuracy: convergent validity
#   GERT clips: does HR during a clip track its rated intensity?
#
# Input:  Results/physio/cardiac_*.csv (05d), breath_trials.csv, breath_participants.csv
#         (05c), and the behavioural CSVs in Results/.
# Output: Results/cardiac_analysis/ (models.txt, report.md, figures)
# Exploratory; p values uncorrected.
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
resultsPath  <- file.path(mainPath, "Repo", "Results")
physioQCPath <- file.path(resultsPath, "physio")
outPath      <- file.path(resultsPath, "cardiac_analysis")
if (!dir.exists(outPath)) dir.create(outPath, recursive = TRUE)

## Constants ---------
ECG_MIN      <- 0.6   # channel-choice ECG score (05b)
MAX_ART_PCT  <- 10    # % flagged RR allowed in an HRV window
SYNC_MIN     <- 0.4   # belt follows pacer (05c)
rd <- function(f) readr::read_csv(file.path(physioQCPath, f), col_types = readr::cols(id = "c", .default = "?"))

# Data ---------
cp   <- rd("cardiac_participants.csv")
hbd  <- rd("cardiac_hbd.csv")
ctr  <- rd("cardiac_bcat_trials.csv")
clip <- rd("cardiac_gert_clips.csv")
btr  <- rd("breath_trials.csv")
bpt  <- rd("breath_participants.csv")

aliases <- tryCatch(rd("physio_id_aliases.csv"), error = function(e) NULL)
fix_id <- function(x) {
  x <- sub("\\.0$", "", as.character(x))
  if (!is.null(aliases)) x[x %in% aliases$alias_id] <- aliases$assigned_id[match(x[x %in% aliases$alias_id], aliases$alias_id)]
  x
}
quest <- readr::read_csv(file.path(resultsPath, "questionnaireFile.csv")) |>
  dplyr::mutate(id = fix_id(id)) |>
  dplyr::distinct(id, .keep_all = TRUE)
gertb <- readr::read_csv(file.path(resultsPath, "GERT_baseline_data.csv")) |>
  dplyr::mutate(id = fix_id(id)) |>
  dplyr::group_by(id) |>
  dplyr::summarise(gert_base_acc = mean(emoAccuracy, na.rm = TRUE),
                   gert_base_int = mean(IntensityRating, na.rm = TRUE), .groups = "drop")
bcatb <- readr::read_csv(file.path(resultsPath, "BCAT_baseline_data.csv")) |>
  dplyr::mutate(id = fix_id(id)) |>
  dplyr::group_by(id) |>
  dplyr::summarise(base_thresh = mean(c(ACCthresh[1], DECthresh[1])), .groups = "drop")
combG <- readr::read_csv(file.path(resultsPath, "Combined_fullGERT_data.csv")) |>
  dplyr::mutate(id = fix_id(id))
combB <- readr::read_csv(file.path(resultsPath, "Combined_data.csv")) |>
  dplyr::mutate(id = fix_id(id)) |>
  dplyr::select(id, Block, Salience, DirectionLabel, Arousal, bcatAccuracy)

# Exclusion sets shared with every other analysis (R/06_exclusions.R):
# participant-level questions use the questionnaire sample (E1A further needs
# the baseline GERT); trial- and clip-level combined-task models use the
# combined-task sample.
excl <- readr::read_csv(file.path(resultsPath, "exclusions.csv"), col_types = readr::cols(id = "c", .default = "?"))
cp <- cp |> dplyr::filter(id %in% excl$id[excl$set_questionnaire])
in_combined <- excl$id[excl$set_combined]
ecg_ok <- cp |> dplyr::filter(ecg_quality >= ECG_MIN) |> dplyr::pull(id)

## Participant table ---------
pt <- cp |>
  dplyr::filter(id %in% ecg_ok) |>
  dplyr::mutate(
    rest_ok = !is.na(rest_rmssd_ms) & rest_pct_artifact <= MAX_ART_PCT,
    ln_rest_rmssd = ifelse(rest_ok, log(rest_rmssd_ms), NA),
    rest_hf = ifelse(rest_ok, rest_hf_lnms2, NA),
    rest_hr = ifelse(rest_ok, rest_hr_bpm, NA),
    ln_gb_rmssd = ifelse(gert_baseline_pct_artifact <= MAX_ART_PCT, log(gert_baseline_rmssd_ms), NA),
    ln_cb_rmssd = ifelse(combined_pct_artifact <= MAX_ART_PCT, log(combined_rmssd_ms), NA)) |>
  dplyr::left_join(quest |> dplyr::select(id, BATtotal, MAIAtotal, BIPStotal, PHQ4total, Age, Gender), by = "id") |>
  dplyr::left_join(gertb, by = "id") |>
  dplyr::left_join(bcatb, by = "id")

## Trial table: HR change on paced trials, with the breathing actually done ---------
# A restarted task leaves two matched files for one participant; keep the one
# with the most trials (the completed run).
main_file <- function(d) d |>
  dplyr::add_count(id, task, task_file, name = "n_in_file") |>
  dplyr::group_by(id, task) |>
  dplyr::filter(task_file == task_file[which.max(n_in_file)]) |>
  dplyr::ungroup() |> dplyr::select(-n_in_file)
tr <- ctr |>
  main_file() |>
  dplyr::filter(id %in% ecg_ok, pct_artifact <= MAX_ART_PCT, is.finite(hr_0_8), is.finite(hr_8_13),
                !is.na(high_salience)) |>
  dplyr::left_join(btr |> dplyr::select(id, task, task_file, trial, obs_ratio, exp_ratio),
                   by = c("id", "task", "task_file", "trial")) |>
  dplyr::mutate(
    d_hr   = hr_8_13 - hr_0_8,          # time-locked: same latency in every condition
    dir_e  = dplyr::case_when(direction == -1 ~ 0.5, direction == 1 ~ -0.5),
    sal_e  = ifelse(high_salience, 0.5, -0.5),
    spd_exp = -log(exp_ratio), spd_obs = -log(obs_ratio),
    belt_ok = id %in% (bpt |> dplyr::filter(sync >= SYNC_MIN) |> dplyr::pull(id)) &
              is.finite(spd_obs) & abs(spd_obs) <= log(2)) |>
  dplyr::group_by(id, task) |>
  dplyr::mutate(d_hr_pm = mean(d_hr), d_hr_pc = d_hr - d_hr_pm,
                spd_obs_pm = mean(spd_obs[belt_ok]), spd_obs_pc = spd_obs - spd_obs_pm) |>
  dplyr::ungroup()
comb_tr <- tr |> dplyr::filter(task == "combined", id %in% in_combined)
tr <- tr |> dplyr::filter(task == "combined" | id %in% excl$id[excl$set_bcat_baseline])

## Clip table: combined-task clips with ratings and block condition ---------
cl <- clip |>
  dplyr::filter(task == "combined", id %in% ecg_ok, id %in% in_combined, pct_artifact <= MAX_ART_PCT,
                is.finite(hr_clip), is.finite(hr_pre)) |>
  dplyr::inner_join(combG |> dplyr::select(id, FileName, IntensityRating, emoAccuracy, Block),
                    by = c("id", "FileName")) |>
  dplyr::left_join(combB, by = c("id", "Block")) |>
  dplyr::mutate(d_hr_clip = hr_clip - hr_pre,
                dir_e = ifelse(DirectionLabel == "Acc", 0.5, -0.5),
                sal_e = ifelse(Salience == "High", 0.5, -0.5)) |>
  dplyr::group_by(id) |>
  dplyr::mutate(hr_clip_pc = hr_clip - mean(hr_clip), hr_clip_pm = mean(hr_clip),
                d_hr_clip_pc = d_hr_clip - mean(d_hr_clip)) |>
  dplyr::ungroup()

# Models ---------
ct <- function(x, y, ...) { t <- cor.test(x, y, ...); c(r = unname(t$estimate), lo = t$conf.int[1],
                                                         hi = t$conf.int[2], p = t$p.value, n = sum(is.finite(x) & is.finite(y))) }
fmt_ct <- function(v) sprintf("r = %.3f [%.3f, %.3f], p = %s, n = %d", v["r"], v["lo"], v["hi"],
                              if (v["p"] < .001) formatC(v["p"], format = "e", digits = 1) else sprintf("%.3f", v["p"]), v["n"])
fx <- function(m, term) {
  s <- summary(m)$coefficients; p <- s[term, ncol(s)]
  ci <- tryCatch(suppressMessages(confint(m, parm = term, method = "Wald")), error = function(e) c(NA, NA))
  sprintf("b = %.3f [%.3f, %.3f], p = %s", s[term, 1], ci[1], ci[2],
          if (p < .001) formatC(p, format = "e", digits = 1) else sprintf("%.3f", p))
}

sink(file.path(outPath, "models.txt"))
cat("Cardiac analysis -", format(Sys.time()), "\n\n")
cat(sprintf("Participants with ECG: %d; ECG score >= %.1f: %d; usable rest HRV: %d\n\n",
            nrow(cp), ECG_MIN, length(ecg_ok), sum(pt$rest_ok)))

e1a <- ct(pt$ln_rest_rmssd, pt$gert_base_acc); e1a_hf <- ct(pt$rest_hf, pt$gert_base_acc)
e1a_lm <- lm(gert_base_acc ~ ln_rest_rmssd + rest_hr, data = pt)
cat("== E1A: resting lnRMSSD ~ baseline GERT accuracy ==\n", fmt_ct(e1a), "\n lnHF: ", fmt_ct(e1a_hf), "\n")
print(summary(e1a_lm))

e1b <- t.test(pt$ln_cb_rmssd, pt$ln_gb_rmssd, paired = TRUE)
cat("\n== E1B: lnRMSSD combined task vs baseline GERT (paired) ==\n"); print(e1b)

m_hr  <- lmer(d_hr ~ dir_e * sal_e + (1 | id), data = dplyr::filter(comb_tr, direction != 0))
cat("\n== HR1: d_hr ~ dir * sal + (1 | id), combined change trials ==\n"); print(summary(m_hr))
m_hr0 <- lmer(d_hr ~ 1 + (1 | id), data = dplyr::filter(comb_tr, direction == 0))
cat("\n== HR1b: no-change trials, d_hr ~ 1 + (1 | id) ==\n"); print(summary(m_hr0))
m_hrb <- lmer(d_hr ~ dir_e + (1 | id), data = dplyr::filter(tr, task == "bcat_baseline", direction != 0))
cat("\n== HR1c: baseline task, d_hr ~ dir + (1 | id) ==\n"); print(summary(m_hrb))
m_hr2 <- lmer(d_hr ~ spd_exp + spd_obs_pc + spd_obs_pm + (1 | id), data = dplyr::filter(comb_tr, belt_ok))
cat("\n== HR2: d_hr ~ prescribed speed change + actual (within/between) + (1 | id), combined ==\n"); print(summary(m_hr2))

m_ar <- lmer(arousal ~ spd_exp + d_hr_pc + d_hr_pm + (1 | id), data = comb_tr)
cat("\n== AR1: felt arousal ~ prescribed speed change + HR change (within/between) + (1 | id) ==\n"); print(summary(m_ar))

bat_hr  <- ct(pt$BATtotal, pt$rest_hr); bat_rm <- ct(pt$BATtotal, pt$ln_rest_rmssd)
resp_pp <- comb_tr |> dplyr::filter(direction != 0) |>
  dplyr::group_by(id) |>
  dplyr::summarise(hr_resp = mean(d_hr[dir_e > 0]) - mean(d_hr[dir_e < 0]),
                   felt_arousal = mean(arousal, na.rm = TRUE), mean_hr = mean(hr_trial, na.rm = TRUE),
                   .groups = "drop") |>
  dplyr::left_join(pt |> dplyr::select(id, BATtotal, rest_hr), by = "id")
bat_resp <- ct(resp_pp$BATtotal, resp_pp$hr_resp)
bat_felt <- lm(scale(felt_arousal) ~ scale(BATtotal), data = resp_pp)
bat_felt_hr <- lm(scale(felt_arousal) ~ scale(BATtotal) + scale(mean_hr) + scale(hr_resp), data = resp_pp)
cat("\n== Burnout ==\n BAT ~ rest HR: ", fmt_ct(bat_hr), "\n BAT ~ rest lnRMSSD: ", fmt_ct(bat_rm),
    "\n BAT ~ HR response (accel - decel, combined): ", fmt_ct(bat_resp), "\n")
print(summary(bat_felt)); print(summary(bat_felt_hr))

hbd_pt <- pt |> dplyr::filter(!is.na(hbd_accuracy))
h_thr <- ct(hbd_pt$hbd_accuracy, hbd_pt$base_thresh); h_maia <- ct(hbd_pt$hbd_accuracy, hbd_pt$MAIAtotal)
h_gert <- ct(hbd_pt$hbd_accuracy, hbd_pt$gert_base_acc); h_bat <- ct(hbd_pt$hbd_accuracy, hbd_pt$BATtotal)
cat("\n== Heartbeat counting (Schandry) ==\n"); print(summary(hbd_pt$hbd_accuracy))
cat(" ~ BCAT threshold: ", fmt_ct(h_thr), "\n ~ MAIA: ", fmt_ct(h_maia),
    "\n ~ baseline GERT accuracy: ", fmt_ct(h_gert), "\n ~ BAT: ", fmt_ct(h_bat), "\n")

m_int <- lmer(IntensityRating ~ hr_clip_pc + hr_clip_pm + (1 | id) + (1 | FileName), data = cl)
cat("\n== CL1: clip intensity ~ HR during clip (within/between) + (1 | id) + (1 | clip) ==\n"); print(summary(m_int))
m_int2 <- lmer(IntensityRating ~ d_hr_clip_pc + (1 | id) + (1 | FileName), data = cl)
cat("\n== CL2: clip intensity ~ HR change from pre-clip (within) ==\n"); print(summary(m_int2))
m_blk <- lmer(hr_clip ~ dir_e * sal_e + (1 | id) + (1 | FileName), data = cl)
cat("\n== CL3: HR during clips ~ block direction * salience ==\n"); print(summary(m_blk))
sink()

# Report ---------
rep <- c(
  "# Cardiac analysis", "",
  sprintf("Run %s. Script `Analysis/cardiac_analysis.R`; full output in `models.txt`. Exploratory; p values uncorrected.", format(Sys.Date())), "",
  "## Sample", "",
  sprintf("- %d questionnaire-sample participants with ECG; %d with a usable lead (score >= %.1f); %d with usable resting HRV (<= %d%% flagged RR); %d with all three heartbeat-counting intervals valid.",
          nrow(cp), length(ecg_ok), ECG_MIN, sum(pt$rest_ok), MAX_ART_PCT, nrow(hbd_pt)),
  sprintf("- Pre-task baseline (the up-to-2-min window before the BCAT script; protocol rest, not verifiable from the triggers), medians: HR %.1f bpm, RMSSD %.1f ms, window %.0f s; breathing %.1f /min.",
          stats::median(pt$rest_hr, na.rm = TRUE), stats::median(exp(pt$ln_rest_rmssd), na.rm = TRUE),
          stats::median(pt$rest_window_s, na.rm = TRUE), stats::median(pt$rest_resp_rate_bpm, na.rm = TRUE)),
  "", "## Preregistered exploratory hypotheses", "",
  sprintf("- **E1A** resting HRV and baseline emotion-recognition accuracy: lnRMSSD %s; lnHF %s.", fmt_ct(e1a), fmt_ct(e1a_hf)),
  sprintf("- **E1B** lnRMSSD, combined task minus baseline GERT: mean difference %.3f [%.3f, %.3f], t(%d) = %.2f, p = %s. Note the combined task includes paced breathing at ~15/min, which itself shapes RSA.",
          unname(e1b$estimate), e1b$conf.int[1], e1b$conf.int[2], as.integer(e1b$parameter), unname(e1b$statistic),
          if (e1b$p.value < .001) formatC(e1b$p.value, format = "e", digits = 1) else sprintf("%.3f", e1b$p.value)),
  "- **E1C** (trial-type effect mediated by HRV change) is not estimable as specified: a 4-breath trial (~16 s) is too short for HRV. HR change per trial is used below instead.",
  "", "## Breathing and the heart", "",
  sprintf("- **HR response to the pacer.** HR change, 8-13 s minus 0-8 s after onset (the same latencies in every condition; cycle-defined windows confound condition with latency because HR falls through every trial and trial lengths differ), acceleration minus deceleration: %s bpm; salience %s; interaction %s. No-change trials (mean change): %s bpm. Baseline task, acceleration minus deceleration: %s.",
          fx(m_hr, "dir_e"), fx(m_hr, "sal_e"), fx(m_hr, "dir_e:sal_e"), fx(m_hr0, "(Intercept)"), fx(m_hrb, "dir_e")),
  sprintf("- **The body's own change.** HR change on the prescribed speed change (positive = faster): %s; on the participant's actual extra speeding, within person: %s. Negative b = faster breathing, smaller HR rise.",
          fx(m_hr2, "spd_exp"), fx(m_hr2, "spd_obs_pc")),
  sprintf("- **Felt arousal and the heart.** Felt arousal on the prescribed speed change: %s; on the trial's HR change, within person: %s.",
          fx(m_ar, "spd_exp"), fx(m_ar, "d_hr_pc")),
  "", "## Burnout", "",
  sprintf("- BAT and resting HR: %s; resting lnRMSSD: %s; HR response to acceleration: %s.", fmt_ct(bat_hr), fmt_ct(bat_rm), fmt_ct(bat_resp)),
  sprintf("- Felt arousal on BAT (standardised): %.3f, p = %.3f; with mean task HR and HR response added: %.3f, p = %.3f.",
          coef(bat_felt)[2], summary(bat_felt)$coefficients[2, 4], coef(bat_felt_hr)[2], summary(bat_felt_hr)$coefficients[2, 4]),
  "", "## Heartbeat counting", "",
  sprintf("- Schandry accuracy median %.2f (IQR %.2f-%.2f).", stats::median(hbd_pt$hbd_accuracy),
          stats::quantile(hbd_pt$hbd_accuracy, .25), stats::quantile(hbd_pt$hbd_accuracy, .75)),
  sprintf("- With BCAT threshold %s; MAIA %s; baseline GERT accuracy %s; BAT %s.", fmt_ct(h_thr), fmt_ct(h_maia), fmt_ct(h_gert), fmt_ct(h_bat)),
  "", "## Emotion clips (combined task)", "",
  sprintf("- Rated intensity and HR during the clip, within person: %s; HR change from the 2 s before the clip: %s.",
          fx(m_int, "hr_clip_pc"), fx(m_int2, "d_hr_clip_pc")),
  sprintf("- HR during clips by the preceding block's breathing: direction %s; salience %s.", fx(m_blk, "dir_e"), fx(m_blk, "sal_e"))
)
writeLines(rep, file.path(outPath, "report.md"))

# Key estimates for equivalence testing (Analysis/equivalence_tests.R)
est_row <- function(name, m, term, unit) {
  s <- summary(m)$coefficients
  data.frame(estimate = name, b = s[term, 1], se = s[term, 2], df = s[term, "df"], unit = unit)
}
write.csv(rbind(
  est_row("hr_change_accel_minus_decel", m_hr, "dir_e", "bpm, 8-13 s minus 0-8 s after onset"),
  est_row("hr_change_high_minus_low_salience", m_hr, "sal_e", "bpm, 8-13 s minus 0-8 s after onset")),
  file.path(outPath, "estimates.csv"), row.names = FALSE)

# Figure ---------
f1 <- comb_tr |> dplyr::filter(direction != 0) |>
  dplyr::mutate(cond = paste(ifelse(dir_e > 0, "Accelerate", "Decelerate"), ifelse(sal_e > 0, "high", "low"))) |>
  dplyr::group_by(id, cond) |> dplyr::summarise(d_hr = mean(d_hr), .groups = "drop") |>
  ggplot(aes(cond, d_hr)) + geom_hline(yintercept = 0, linetype = 2) +
  geom_violin(fill = "#9FB7D9", colour = NA) + stat_summary(fun.data = mean_cl_normal, colour = "#2E4A7A") +
  labs(x = NULL, y = "HR change, 8-13 s minus 0-8 s (bpm)") + theme_minimal(base_size = 12)
f2 <- ggplot(hbd_pt, aes(hbd_accuracy)) + geom_histogram(binwidth = 0.05, boundary = 0, fill = "#2E4A7A") +
  labs(x = "Heartbeat-counting accuracy (Schandry)", y = "Participants") + theme_minimal(base_size = 12)
ggsave(file.path(outPath, "fig_cardiac.png"), f1 + f2, width = 11, height = 4, dpi = 300)
message("Done: ", outPath)
