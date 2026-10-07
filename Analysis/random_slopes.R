# random_slopes.R
# The manuscript's primary specification for within-person effects includes
# by-participant random slopes for the focal within-person predictors (Barr et
# al., 2013); random-intercept models are the sensitivity analysis. This script
# refits every within-person model reported in the Results both ways, so the
# text can lead with the slope model and report the other alongside.
#
# Each analysis script's own data preparation is reused: the code above its
# model section is evaluated in a separate environment, so this script uses
# exactly the data those scripts use. If a correlated slope model fails to
# converge or is singular, it is refitted with uncorrelated random effects (||).
#
# Output: Results/random_slopes/ (slopes.csv, equivalence.csv, report.md)
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
analysisPath <- file.path(mainPath, "Repo", "Analysis")
resultsPath  <- file.path(mainPath, "Repo", "Results")
outPath <- file.path(resultsPath, "random_slopes")
if (!dir.exists(outPath)) dir.create(outPath, recursive = TRUE)
rd <- function(...) readr::read_csv(file.path(resultsPath, ...), col_types = readr::cols(id = "c", .default = "?"))

# Evaluate a script's data preparation (everything above `marker`) in its own environment
prep_env <- function(script, marker) {
  src <- readLines(file.path(analysisPath, script))
  stop_at <- grep(marker, src)[1]
  env <- new.env(parent = globalenv())
  assign("mainPath", mainPath, envir = env)
  eval(parse(text = src[1:(stop_at - 1)]), envir = env)
  env
}

# Fitting ---------
ctl_l <- lmerControl(optimizer = "bobyqa", calc.derivs = FALSE)
ctl_g <- glmerControl(optimizer = "bobyqa", calc.derivs = FALSE)
fit <- function(f, data, logit) {
  w <- character(0)
  m <- withCallingHandlers(
    if (logit) glmer(f, data = data, family = binomial, control = ctl_g) else lmer(f, data = data, control = ctl_l),
    warning = function(cw) { w <<- c(w, conditionMessage(cw)); invokeRestart("muffleWarning") })
  attr(m, "fit_warn") <- any(grepl("converge|singular", w, ignore.case = TRUE)) || isSingular(m)
  m
}
coef_row <- function(m, term) {
  s <- summary(m)$coefficients
  b <- s[term, 1]; se <- s[term, 2]
  df <- if ("df" %in% colnames(s)) s[term, "df"] else Inf
  q <- if (is.finite(df)) qt(0.975, df) else qnorm(0.975)
  c(b = b, se = se, df = df, lo = b - q * se, hi = b + q * se, p = s[term, ncol(s)])
}
# Fit `fixed` with random intercepts and with `slopes` (by-participant random
# slopes); `extra` adds other random effects (e.g. clip intercepts).
rows <- list(); models <- list()
both <- function(label, fixed, slopes, terms, data, extra = "", logit = FALSE) {
  f_int <- as.formula(paste(fixed, "+ (1 | id)", extra))
  f_sl  <- as.formula(paste(fixed, "+ (1 +", slopes, "| id)", extra))
  m_int <- fit(f_int, data, logit)
  m_sl  <- fit(f_sl, data, logit)
  re <- "correlated"
  if (isTRUE(attr(m_sl, "fit_warn"))) {
    m_sl <- fit(as.formula(paste(fixed, "+ (1 +", slopes, "|| id)", extra)), data, logit); re <- "uncorrelated"
  }
  models[[label]] <<- list(int = m_int, slope = m_sl)
  for (t in terms) {
    a <- coef_row(m_int, t); s <- coef_row(m_sl, t)
    rows[[length(rows) + 1]] <<- data.frame(
      model = label, term = t, slopes = slopes, re = re, singular = isSingular(m_sl),
      b_int = a["b"], p_int = a["p"],
      b = s["b"], se = s["se"], df = s["df"], lo = s["lo"], hi = s["hi"], p = s["p"],
      stringsAsFactors = FALSE, row.names = NULL)
  }
  message("  fitted: ", label, " (", re, ")")
}

# A. Behavioural combined-task data (cleaned CSVs) ---------
excl <- rd("exclusions.csv"); comb_ids <- excl$id[excl$set_combined]
bt <- rd("Combined_fullBCAT_data.csv") |>
  dplyr::filter(id %in% comb_ids) |>
  dplyr::mutate(change = Direction != "NoChange",
                dir_e = dplyr::case_when(DirectionLabel == "Acc" ~ 0.5, DirectionLabel == "Dec" ~ -0.5),
                sal_e = ifelse(Salience == "High", 0.5, -0.5)) |>
  dplyr::group_by(id) |>
  dplyr::mutate(acc_pc = Accuracy - mean(Accuracy[change], na.rm = TRUE)) |>
  dplyr::ungroup()
btc <- dplyr::filter(bt, change, !is.na(dir_e), !is.na(Accuracy), !is.na(Arousal))
blk <- rd("Combined_data.csv") |>
  dplyr::filter(id %in% comb_ids) |>
  dplyr::mutate(dir_e = ifelse(DirectionLabel == "Acc", 0.5, -0.5), sal_e = ifelse(Salience == "High", 0.5, -0.5),
                aware = bcatAccuracy >= 2/3, aware_e = ifelse(aware, 0.5, -0.5),
                accel_aware = as.numeric(DirectionLabel == "Acc" & aware)) |>
  dplyr::group_by(id) |>
  dplyr::mutate(ar_pm = mean(Arousal, na.rm = TRUE), ar_pc = Arousal - ar_pm) |>
  dplyr::ungroup()
gt <- rd("Combined_fullGERT_data.csv") |>
  dplyr::filter(id %in% comb_ids) |>
  dplyr::inner_join(blk |> dplyr::select(id, Block, dir_e, sal_e, aware_e, accel_aware, ar_pc, ar_pm), by = c("id", "Block"))

message("A. Manipulation checks, H3, E2C, salience")
both("R1 felt arousal ~ direction x salience", "Arousal ~ dir_e * sal_e", "dir_e + sal_e",
     c("dir_e", "sal_e", "dir_e:sal_e"), btc)
both("R2A detection ~ salience x direction (logit)", "Accuracy ~ sal_e * dir_e", "sal_e + dir_e",
     c("sal_e", "dir_e", "sal_e:dir_e"), btc, logit = TRUE)
both("R2B felt arousal ~ direction x detection", "Arousal ~ dir_e * acc_pc + sal_e", "dir_e + acc_pc",
     c("dir_e:acc_pc", "acc_pc"), btc)
both("H3A block accuracy ~ accelerate-and-aware", "emoAccuracy ~ accel_aware", "accel_aware", "accel_aware", blk)
both("H3B block intensity ~ accelerate-and-aware", "emoIntensity ~ accel_aware", "accel_aware", "accel_aware", blk)
both("H3B clip intensity ~ accelerate-and-aware", "IntensityRating ~ accel_aware", "accel_aware", "accel_aware",
     gt, extra = "+ (1 | FileName)")
both("Awareness x direction, clip intensity (effect coded)", "IntensityRating ~ aware_e * dir_e", "aware_e + dir_e",
     c("aware_e", "dir_e", "aware_e:dir_e"), gt, extra = "+ (1 | FileName)")
both("E2C block intensity ~ felt arousal", "emoIntensity ~ ar_pc + ar_pm + dir_e * sal_e", "ar_pc",
     c("ar_pc", "ar_pm"), blk)
both("E2C clip intensity ~ felt arousal", "IntensityRating ~ ar_pc + ar_pm + dir_e * sal_e", "ar_pc",
     c("ar_pc", "ar_pm"), gt, extra = "+ (1 | FileName)")
both("E2C block accuracy ~ felt arousal", "emoAccuracy ~ ar_pc + ar_pm + dir_e * sal_e", "ar_pc", "ar_pc", blk)
both("Clip intensity ~ salience x direction", "IntensityRating ~ sal_e * dir_e", "sal_e + dir_e",
     c("sal_e", "dir_e", "sal_e:dir_e"), gt, extra = "+ (1 | FileName)")
both("Clip accuracy ~ salience x direction (linear probability)", "emoAccuracy ~ sal_e * dir_e", "sal_e + dir_e",
     c("sal_e", "dir_e"), gt, extra = "+ (1 | FileName)")

# B. Breathing (breath_manipulation_check.R data) ---------
message("B. Breathing")
eb <- prep_env("breath_manipulation_check.R", "^sink\\(")
both("Breathing change ~ direction x salience", "spd_obs ~ dir_e * sal_e", "dir_e + sal_e",
     c("dir_e", "sal_e", "dir_e:sal_e"), dplyr::filter(eb$comb, change))
both("Ramp vs step: observed ~ prescribed x salience", "lr_obs ~ lr_exp * sal_e", "lr_exp",
     c("lr_exp", "lr_exp:sal_e"), eb$comb)
both("Detection ~ breathing change made (combined, logit)", "accuracy ~ aligned_pc + aligned_pm + dir_e * sal_e",
     "aligned_pc", "aligned_pc", dplyr::filter(eb$comb, change), logit = TRUE)
both("Detection ~ breathing change made (baseline, logit)", "accuracy ~ aligned_pc + aligned_pm + abs(lr_exp) + dir_e",
     "aligned_pc", "aligned_pc", dplyr::filter(eb$base, change), logit = TRUE)
both("Felt arousal ~ prescribed and own speeding", "arousal ~ spd_exp + sal_e + spd_obs_pc + spd_obs_pm",
     "spd_exp + spd_obs_pc", c("spd_exp", "spd_obs_pc"), eb$comb)

# C. Body vs awareness (body_vs_awareness.R data) ---------
message("C. Body vs awareness")
ev <- prep_env("body_vs_awareness.R", "^# Models ---------")
cl <- ev$clips; bl <- ev$blocks
both("Intensity ~ awareness", "IntensityRating ~ aware_pc + aware_pm + dir_e * sal_e", "aware_pc",
     "aware_pc", cl, extra = "+ (1 | FileName)")
both("Intensity ~ breathing change made", "IntensityRating ~ body_pc + body_pm + dir_e * sal_e", "body_pc",
     "body_pc", cl, extra = "+ (1 | FileName)")
both("Intensity ~ awareness + breathing change", "IntensityRating ~ aware_pc + body_pc + aware_pm + body_pm + dir_e * sal_e",
     "aware_pc + body_pc", c("aware_pc", "body_pc"), cl, extra = "+ (1 | FileName)")
both("Intensity ~ awareness + breathing change + felt arousal",
     "IntensityRating ~ aware_pc + body_pc + arousal_pc + aware_pm + body_pm + arousal_pm + dir_e * sal_e",
     "aware_pc + body_pc + arousal_pc", c("aware_pc", "body_pc", "arousal_pc", "arousal_pm"), cl, extra = "+ (1 | FileName)")
both("Chain: awareness ~ breathing change", "aware ~ body_pc + body_pm + dir_e * sal_e", "body_pc", "body_pc", bl)
both("Chain: felt arousal ~ awareness + breathing change", "arousal ~ body_pc + aware_pc + body_pm + aware_pm + dir_e * sal_e",
     "aware_pc + body_pc", c("aware_pc", "body_pc"), bl)
body_sd <- sd(bl$body_pc)

# D. Cardiac (cardiac_analysis.R data) ---------
message("D. Cardiac")
ec <- prep_env("cardiac_analysis.R", "^# Models ---------")
both("Trial HR change ~ direction x salience", "d_hr ~ dir_e * sal_e", "dir_e + sal_e",
     c("dir_e", "sal_e"), dplyr::filter(ec$comb_tr, direction != 0))
both("Felt arousal ~ trial HR change", "arousal ~ spd_exp + d_hr_pc + d_hr_pm", "spd_exp + d_hr_pc",
     "d_hr_pc", ec$comb_tr)
both("Clip intensity ~ clip HR", "IntensityRating ~ hr_clip_pc + hr_clip_pm", "hr_clip_pc",
     "hr_clip_pc", ec$cl, extra = "+ (1 | FileName)")

# Serial indirect effect with slopes (body -> awareness -> felt arousal -> intensity)
mc3 <- function(a, b, c, n = 2e4) {
  set.seed(20261007)
  d <- rnorm(n, a["b"], a["se"]) * rnorm(n, b["b"], b["se"]) * rnorm(n, c["b"], c["se"])
  c(est = unname(a["b"] * b["b"] * c["b"]), lo = unname(quantile(d, .025)), hi = unname(quantile(d, .975)))
}
cr <- function(lbl, t) coef_row(models[[lbl]]$slope, t)
serial <- mc3(cr("Chain: awareness ~ breathing change", "body_pc"),
              cr("Chain: felt arousal ~ awareness + breathing change", "aware_pc"),
              cr("Intensity ~ awareness + breathing change + felt arousal", "arousal_pc"))

# Equivalence (TOST) on the slope models ---------
tost <- function(test, v, bound, unit, scale = 1) {
  b <- v["b"] * scale; se <- v["se"] * scale; df <- if (is.finite(v["df"])) v["df"] else 1e6
  p <- max(pt((b + bound) / se, df, lower.tail = FALSE), pt((b - bound) / se, df))
  h <- qt(0.95, df) * se
  data.frame(test = test, estimate = unname(b), ci90_lo = unname(b - h), ci90_hi = unname(b + h), sesoi = bound,
             unit = unit, p_tost = unname(p), equivalent = unname(p < .05), stringsAsFactors = FALSE)
}
eq <- dplyr::bind_rows(
  tost("Salience (high - low) -> clip intensity", cr("Clip intensity ~ salience x direction", "sal_e"), 0.05, "scale points"),
  tost("Direction (accelerate - decelerate) -> clip intensity", cr("Clip intensity ~ salience x direction", "dir_e"), 0.05, "scale points"),
  tost("Salience x direction -> clip intensity", cr("Clip intensity ~ salience x direction", "sal_e:dir_e"), 0.05, "scale points"),
  tost("Breathing change made (per within-person SD) -> clip intensity",
       cr("Intensity ~ awareness + breathing change", "body_pc"), 0.05, "scale points", scale = body_sd),
  tost("Salience (high - low) -> recognition accuracy", cr("Clip accuracy ~ salience x direction (linear probability)", "sal_e"), 0.02, "proportion"),
  tost("Direction (accelerate - decelerate) -> recognition accuracy", cr("Clip accuracy ~ salience x direction (linear probability)", "dir_e"), 0.02, "proportion"),
  tost("Direction (accelerate - decelerate) -> HR change", cr("Trial HR change ~ direction x salience", "dir_e"), 1, "bpm"),
  tost("Salience (high - low) -> HR change", cr("Trial HR change ~ direction x salience", "sal_e"), 1, "bpm"))

# Output ---------
tab <- dplyr::bind_rows(rows)
write.csv(tab, file.path(outPath, "slopes.csv"), row.names = FALSE)
write.csv(eq, file.path(outPath, "equivalence.csv"), row.names = FALSE)
fp <- function(p) ifelse(p < .001, "< .001", sub("^0", "", sprintf("%.3f", p)))
rep <- c(
  "# Random-slope models (primary) and random-intercept models (sensitivity)", "",
  sprintf("Run %s. Script `Analysis/random_slopes.R`.", format(Sys.Date())),
  "Each row is one fixed effect. `b, 95% CI, p` come from the model with by-participant random slopes for the listed predictors; `b (int.)` and `p (int.)` from the same model with random intercepts only. Clip-level models also have random intercepts for clip. `re` says whether the slope model kept correlated random effects or needed uncorrelated ones (||).", "",
  "| Model | Term | Slopes | re | b [95% CI] | p | b (int.) | p (int.) |", "|---|---|---|---|---|---|---|---|",
  sprintf("| %s | %s | %s | %s%s | %.3f [%.3f, %.3f] | %s | %.3f | %s |", tab$model, tab$term, tab$slopes, tab$re,
          ifelse(tab$singular, ", singular", ""), tab$b, tab$lo, tab$hi, fp(tab$p), tab$b_int, fp(tab$p_int)), "",
  sprintf("Serial indirect effect, breathing change -> awareness -> felt arousal -> intensity, all paths with slopes: %.4f, Monte Carlo 95%% CI [%.4f, %.4f].",
          serial["est"], serial["lo"], serial["hi"]),
  sprintf("Within-person SD of breathing change (blocks): %.3f log-units.", body_sd), "",
  "## Equivalence tests on the slope models", "",
  "| Effect | Estimate | 90% CI | SESOI | Equivalent |", "|---|---|---|---|---|",
  sprintf("| %s | %.3f | [%.3f, %.3f] | +-%s %s | %s |", eq$test, eq$estimate, eq$ci90_lo, eq$ci90_hi, eq$sesoi, eq$unit,
          ifelse(eq$equivalent, "yes", "no")))
writeLines(rep, file.path(outPath, "report.md"))
message("Done: ", outPath)
