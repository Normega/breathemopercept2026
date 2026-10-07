# results_numbers.R
# Descriptives and the preregistered manipulation checks (R1, R2A, R2B) and
# exploratory hypotheses (E2A-C, E3) on the cleaned sample, for the Results
# draft. Run after main.R's prep, exclusion and merge steps (01-04, 06, 07).
# Output: Results/results_numbers/numbers.txt
# ---------------------------------------------------------------

outPath <- file.path(resultsPath, "results_numbers")
if (!dir.exists(outPath)) dir.create(outPath, recursive = TRUE)

fx <- function(m, term) {
  s <- summary(m)$coefficients; p <- s[term, ncol(s)]
  ci <- suppressMessages(confint(m, parm = term, method = "Wald"))
  sprintf("b = %.3f, 95%% CI [%.3f, %.3f], p = %s", s[term, 1], ci[1], ci[2],
          if (p < .001) formatC(p, format = "e", digits = 1) else sprintf("%.3f", p))
}
ctxt <- function(t) sprintf("r(%d) = %.3f, 95%% CI [%.3f, %.3f], p = %s", as.integer(t$parameter),
                            t$estimate, t$conf.int[1], t$conf.int[2],
                            if (t$p.value < .001) formatC(t$p.value, format = "e", digits = 1) else sprintf("%.3f", t$p.value))
msd <- function(x) sprintf("M = %.2f, SD = %.2f, range %.2f-%.2f, n = %d", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE),
                           min(x, na.rm = TRUE), max(x, na.rm = TRUE), sum(!is.na(x)))

sink(file.path(outPath, "numbers.txt"))
cat("Results numbers -", format(Sys.time()), "\n\n")

## Descriptives ---------
for (nm in c("H1 sample (questionnaire + GERT baseline)", "combined-task sample")) {
  ids <- if (grepl("H1", nm)) exclusions$id[exclusions$set_gert_baseline] else exclusions$id[exclusions$set_combined]
  d <- pData_clean[pData_clean$id %in% ids, ]
  cat("==", nm, ": N =", length(ids), "\n")
  cat("  Age:", msd(d$Age), "\n  Gender:\n"); print(table(d$Gender, useNA = "ifany"))
  cat("  BAT total:", msd(d$BATtotal), "\n  MAIA total:", msd(d$MAIAtotal), "\n")
}
b1 <- bcatBase_clean |> dplyr::group_by(id) |>
  dplyr::summarise(acc = ACCthresh[1], dec = DECthresh[1], .groups = "drop")
cat("\nBCAT thresholds (proportion change): accelerate", msd(b1$acc), "\n  decelerate", msd(b1$dec), "\n")
cat("  paired accel vs decel: "); tt <- t.test(b1$dec, b1$acc, paired = TRUE)
cat(sprintf("t(%d) = %.2f, p = %s, mean diff %.3f\n", as.integer(tt$parameter), tt$statistic,
            formatC(tt$p.value, format = "g", digits = 3), tt$estimate))
g1 <- gertBase_clean |> dplyr::group_by(id) |>
  dplyr::summarise(acc = mean(emoAccuracy, na.rm = TRUE), int = mean(IntensityRating, na.rm = TRUE), .groups = "drop")
cat("GERT baseline accuracy:", msd(g1$acc), "\n  intensity:", msd(g1$int), "\n")

## Combined-task trial data, effect coded ---------
bt <- combBCAT_clean |>
  dplyr::mutate(change = Direction != "NoChange",
                dir_e = dplyr::case_when(DirectionLabel == "Acc" ~ 0.5, DirectionLabel == "Dec" ~ -0.5),
                sal_e = ifelse(Salience == "High", 0.5, -0.5)) |>
  dplyr::group_by(id) |>
  dplyr::mutate(acc_pc = Accuracy - mean(Accuracy[change], na.rm = TRUE)) |>
  dplyr::ungroup()
btc <- dplyr::filter(bt, change)
cat("\nCombined task: blocks", nrow(combData_clean), "; change trials", nrow(btc), "; catch trials", sum(!bt$change), "\n")
cat("Detection: high salience", sprintf("%.1f%%", 100 * mean(btc$Accuracy[btc$sal_e > 0], na.rm = TRUE)),
    ", low salience", sprintf("%.1f%%", 100 * mean(btc$Accuracy[btc$sal_e < 0], na.rm = TRUE)),
    "; catch trials correct", sprintf("%.1f%%", 100 * mean(bt$Accuracy[!bt$change], na.rm = TRUE)), "\n")
cat("Aware blocks (>= 2/3 correct, thesis definition):", sprintf("%.1f%%", 100 * mean(combData_clean$bcatAccuracy >= 2/3)), "\n")

r1 <- lmer(Arousal ~ dir_e * sal_e + (1 | id), data = btc)
cat("\n== R1 felt arousal ~ direction (accel +0.5) x salience, change trials ==\n", fx(r1, "dir_e"),
    "\n  salience", fx(r1, "sal_e"), "\n  interaction", fx(r1, "dir_e:sal_e"), "\n")
r2a <- glmer(Accuracy ~ sal_e * dir_e + (1 | id), data = btc, family = binomial,
             control = glmerControl(optimizer = "bobyqa"))
cat("\n== R2A detection ~ salience x direction (logit) ==\n", fx(r2a, "sal_e"), sprintf("(OR = %.2f)", exp(fixef(r2a)["sal_e"])),
    "\n  direction", fx(r2a, "dir_e"), "\n  interaction", fx(r2a, "sal_e:dir_e"), "\n")
r2b <- lmer(Arousal ~ dir_e * acc_pc + sal_e + (1 | id), data = btc)
cat("\n== R2B arousal ~ direction x detection (within person) ==\n", fx(r2b, "dir_e:acc_pc"),
    "\n  detection main", fx(r2b, "acc_pc"), "\n")
cat("  direction effect when detected vs missed: ")
for (a in c(1, 0)) { m <- lmer(Arousal ~ dir_e + sal_e + (1 | id), data = dplyr::filter(btc, Accuracy == a))
  cat(ifelse(a == 1, "detected ", "missed "), fx(m, "dir_e"), "; ") }
cat("\n")

## Block level: felt arousal and perceived intensity (E2C) ---------
blk <- combData_clean |>
  dplyr::mutate(dir_e = ifelse(DirectionLabel == "Acc", 0.5, -0.5), sal_e = ifelse(Salience == "High", 0.5, -0.5),
                aware = bcatAccuracy >= 2/3) |>
  dplyr::group_by(id) |>
  dplyr::mutate(ar_pm = mean(Arousal, na.rm = TRUE), ar_pc = Arousal - ar_pm) |> dplyr::ungroup()
e2c <- lmer(emoIntensity ~ ar_pc + ar_pm + dir_e * sal_e + (1 | id), data = blk)
cat("\n== E2C block intensity ~ felt arousal (within, between) + condition ==\n within ", fx(e2c, "ar_pc"),
    "\n between ", fx(e2c, "ar_pm"), "\n")
e2c_acc <- lmer(emoAccuracy ~ ar_pc + ar_pm + dir_e * sal_e + (1 | id), data = blk)
cat(" accuracy within ", fx(e2c_acc, "ar_pc"), "\n")

## Salience and direction on perceived intensity, clip level ---------
gt <- combGERT_clean |>
  dplyr::left_join(blk |> dplyr::select(id, Block, dir_e, sal_e), by = c("id", "Block"))
s_int <- lmer(IntensityRating ~ sal_e * dir_e + (1 | id) + (1 | FileName), data = gt)
cat("\n== Clip intensity ~ salience x direction ==\n salience ", fx(s_int, "sal_e"), "\n direction ", fx(s_int, "dir_e"),
    "\n interaction ", fx(s_int, "sal_e:dir_e"), "\n")

## E2A / E2B: combined task vs baseline ---------
cg <- combGERT_clean |> dplyr::group_by(id) |>
  dplyr::summarise(acc_c = mean(emoAccuracy, na.rm = TRUE), int_c = mean(IntensityRating, na.rm = TRUE), .groups = "drop") |>
  dplyr::inner_join(g1, by = "id")
ta <- t.test(cg$acc_c, cg$acc, paired = TRUE); ti <- t.test(cg$int_c, cg$int, paired = TRUE)
cat(sprintf("\n== E2A accuracy, combined minus baseline: %.3f [%.3f, %.3f], t(%d) = %.2f, p = %s (combined M = %.3f, baseline M = %.3f)\n",
            ta$estimate, ta$conf.int[1], ta$conf.int[2], as.integer(ta$parameter), ta$statistic,
            formatC(ta$p.value, format = "g", digits = 3), mean(cg$acc_c), mean(cg$acc)))
cat(sprintf("== E2B intensity, combined minus baseline: %.3f [%.3f, %.3f], t(%d) = %.2f, p = %s (combined M = %.2f, baseline M = %.2f)\n",
            ti$estimate, ti$conf.int[1], ti$conf.int[2], as.integer(ti$parameter), ti$statistic,
            formatC(ti$p.value, format = "g", digits = 3), mean(cg$int_c), mean(cg$int)))

## Burnout and felt arousal (E3-E5 area) ---------
tr <- traitData
cat("\n== Burnout and felt arousal ==\n combined task: ", ctxt(cor.test(tr$BATtotal, tr$combinedArousal)),
    "\n baseline BCAT: ", ctxt(cor.test(bothBaseQ_data$BATtotal, bothBaseQ_data$baseArousal)), "\n")
cat(" burnout ~ combined intensity: ", ctxt(cor.test(tr$BATtotal, tr$combinedIntensity)), "\n")
sink()
message("Numbers written to ", outPath)
