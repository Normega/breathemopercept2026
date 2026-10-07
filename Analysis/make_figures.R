# make_figures.R
# Manuscript figures 1-6, from the pipeline's outputs. Each figure is written
# as vector PDF and 600-dpi PNG to Results/figures/, with captions in
# Results/figures/captions.md.
#
#   Fig 1  Design: session and block structure; prescribed vs observed breath cycles
#   Fig 2  Breathing manipulation: belt-pacer synchrony; detection follows the body;
#          felt arousal by direction and detection (R2B)
#   Fig 3  H2B: threshold and recognition, baseline and independent clips; robustness
#   Fig 4  Awareness, felt arousal and perceived intensity; the chain
#   Fig 5  Felt vs cardiac arousal: heart rate over a trial; felt arousal; burnout
#   Fig 6  Equivalence tests for the key null results
#
# Run after the pipeline and the analyses whose estimates it reads
# (h2b_robustness.R, body_vs_awareness.R, equivalence_tests.R).
# Palette: categorical slots 1-2 of the reference data-viz palette (validated
# for colour-vision deficiency): accelerate = orange, decelerate = blue; no
# change and neutral marks = grey. Salience is coded by marker fill and line
# type, never by colour alone.
# ---------------------------------------------------------------

# Set Up ---------
## Load libraries ---------
packages <- c("tidyverse", "patchwork", "ragg", "scales")
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
figPath      <- file.path(resultsPath, "figures")
if (!dir.exists(figPath)) dir.create(figPath, recursive = TRUE)
source(file.path(analysisPath, "R", "physio_functions.R"))   # pacer_durations()
rd <- function(...) readr::read_csv(file.path(resultsPath, ...), col_types = readr::cols(id = "c", .default = "?"))

## Look ---------
COL_ACC <- "#eb6834"; COL_DEC <- "#2a78d6"; COL_NONE <- "#8a8984"
INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e4e3df"
dir_cols <- c(Accelerate = COL_ACC, Decelerate = COL_DEC, `No change` = COL_NONE)
theme_fig <- function(base = 8) {
  theme_minimal(base_size = base, base_family = "Arial") +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = GRID, linewidth = 0.3),
          axis.line = element_line(colour = INK2, linewidth = 0.3),
          axis.ticks = element_line(colour = INK2, linewidth = 0.3),
          axis.text = element_text(colour = INK2), axis.title = element_text(colour = INK),
          plot.title = element_text(size = base, face = "bold", colour = INK),
          legend.position = "bottom", legend.title = element_blank(),
          legend.key.size = unit(8, "pt"), plot.tag = element_text(size = base + 2, face = "bold"),
          strip.text = element_text(colour = INK, face = "bold"))
}
save_fig <- function(p, name, width = 7, height = 4.5) {
  ggsave(file.path(figPath, paste0(name, ".pdf")), p, width = width, height = height, device = cairo_pdf)
  ggsave(file.path(figPath, paste0(name, ".png")), p, width = width, height = height, dpi = 600, device = ragg::agg_png)
}
# mean and 95% CI across participants
mci <- function(d, value, ...) d |> dplyr::group_by(...) |>
  dplyr::summarise(m = mean({{ value }}, na.rm = TRUE), se = sd({{ value }}, na.rm = TRUE) / sqrt(sum(!is.na({{ value }}))),
                   n = sum(!is.na({{ value }})), .groups = "drop") |>
  dplyr::mutate(lo = m - qt(.975, pmax(n - 1, 1)) * se, hi = m + qt(.975, pmax(n - 1, 1)) * se)
fp <- function(p) if (p < .001) "p < .001" else sprintf("p = %s", sub("^0", "", sprintf("%.3f", p)))
rlab <- function(x, y) { t <- cor.test(x, y)
  sprintf("r = %s [%s, %s], %s, n = %d", sub("^(-?)0", "\\1", sprintf("%.2f", t$estimate)),
          sub("^(-?)0", "\\1", sprintf("%.2f", t$conf.int[1])), sub("^(-?)0", "\\1", sprintf("%.2f", t$conf.int[2])),
          fp(t$p.value), sum(is.finite(x) & is.finite(y))) }

# Data ---------
excl <- rd("exclusions.csv")
ids_comb <- excl$id[excl$set_combined]
bpart <- rd("physio", "breath_participants.csv") |> dplyr::filter(id %in% ids_comb)
ids_belt <- bpart$id[!is.na(bpart$sync) & bpart$sync >= 0.4]
main_file <- function(d) d |> dplyr::add_count(id, task, task_file, name = "nf") |>
  dplyr::group_by(id, task) |> dplyr::filter(task_file == task_file[which.max(nf)]) |> dplyr::ungroup()
btr <- rd("physio", "breath_trials.csv") |> dplyr::filter(id %in% ids_belt) |> main_file()
cbcat <- rd("Combined_fullBCAT_data.csv") |> dplyr::filter(id %in% ids_comb)
cgert <- rd("Combined_fullGERT_data.csv") |> dplyr::filter(id %in% ids_comb)
cblk  <- rd("Combined_data.csv") |> dplyr::filter(id %in% ids_comb)
quest <- rd("questionnaireFile.csv") |> dplyr::mutate(id = sub("\\.0$", "", id))
captions <- character(0)

# Figure 1: design ---------
box <- function(x, y, w, h, label, fill = "white", size = 2.6, face = "plain")
  list(annotate("rect", xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2,
                fill = fill, colour = INK2, linewidth = 0.3),
       annotate("text", x = x, y = y, label = label, size = size, colour = INK, family = "Arial", fontface = face, lineheight = 0.9))
arrow_seg <- function(x0, x1, y) annotate("segment", x = x0, xend = x1, y = y, yend = y, colour = INK2, linewidth = 0.3,
                                          arrow = arrow(length = unit(3, "pt"), type = "closed"))
steps <- c("Rest\n(about 2 min)", "BCAT baseline\n20 trials", "Heartbeat\ncounting", "GERT baseline\n23 clips",
           "Questionnaires", "Combined task\n12 blocks")
xs <- seq(0.85, 9.15, length.out = length(steps))
f1a <- ggplot() + coord_cartesian(xlim = c(0, 10), ylim = c(0, 4.2), expand = FALSE) + theme_void(base_family = "Arial") +
  lapply(seq_along(steps), function(i) box(xs[i], 3.5, 1.4, 0.8, steps[i], fill = if (i == 6) "#f0efec" else "white")) +
  lapply(1:5, function(i) arrow_seg(xs[i] + 0.72, xs[i + 1] - 0.72, 3.5)) +
  annotate("text", x = 0.15, y = 2.05, label = "Each of the 12 combined-task blocks", hjust = 0, size = 2.6, fontface = "bold", colour = INK) +
  box(1.6, 1.0, 1.5, 0.9, "BCAT trial\n(change)") + box(3.3, 1.0, 1.5, 0.9, "BCAT trial\n(change)") +
  box(5.0, 1.0, 1.5, 0.9, "BCAT trial\n(no change)") + box(8.0, 1.0, 2.6, 0.9, "5 GERT clips\nlabel + intensity (1-7)") +
  arrow_seg(2.35, 2.55, 1.0) + arrow_seg(4.05, 4.25, 1.0) + arrow_seg(5.75, 6.7, 1.0) +
  annotate("text", x = 3.3, y = 0.25, size = 2.3, colour = INK2, family = "Arial",
           label = "order random; after each: change? (faster / slower / none), confidence, felt arousal") +
  theme(plot.tag = element_text(size = 10, face = "bold", family = "Arial"))
dur <- btr |> dplyr::filter(task == "combined", !is.na(high_salience)) |>
  dplyr::mutate(cond_dir = dplyr::case_when(direction == -1 ~ "Accelerate", direction == 1 ~ "Decelerate", TRUE ~ "No change"),
                salience = ifelse(high_salience, "High salience: one step", "Low salience: gradual ramp"))
presc <- dur |> dplyr::rowwise() |>
  # cycle 4 is timed peak to peak (breath 3 to 4), so its prescribed value is the pacer over that span
  dplyr::mutate(p = list({ q <- pacer_durations(level, direction, isTRUE(high_salience)); q[4] <- (q[3] + q[4]) / 2; q })) |>
  dplyr::ungroup() |>
  tidyr::unnest_longer(p, indices_to = "cycle") |>
  dplyr::group_by(id, salience, cond_dir, cycle) |> dplyr::summarise(v = mean(p), .groups = "drop") |>
  mci(v, salience, cond_dir, cycle)
obs <- dur |> dplyr::select(id, salience, cond_dir, obs_d1, obs_d2, obs_d3, obs_d4) |>
  tidyr::pivot_longer(obs_d1:obs_d4, names_to = "cycle", values_to = "d") |>
  dplyr::mutate(cycle = as.integer(sub("obs_d", "", cycle))) |>
  dplyr::filter(is.finite(d), d > 1, d < 12) |>
  dplyr::group_by(id, salience, cond_dir, cycle) |> dplyr::summarise(v = mean(d), .groups = "drop") |>
  mci(v, salience, cond_dir, cycle)
f1b <- ggplot() +
  geom_line(data = presc, aes(cycle, m, colour = cond_dir), linewidth = 0.6, linetype = "22") +
  geom_errorbar(data = obs, aes(cycle + 0.06 * (as.integer(factor(cond_dir)) - 2), ymin = lo, ymax = hi, colour = cond_dir),
                width = 0, linewidth = 0.4) +
  geom_point(data = obs, aes(cycle + 0.06 * (as.integer(factor(cond_dir)) - 2), m, colour = cond_dir), size = 1.6) +
  facet_wrap(~ salience) + scale_colour_manual(values = dir_cols) +
  scale_x_continuous(breaks = 1:4, labels = c("1", "2", "3", "4*")) + coord_cartesian(ylim = c(2.4, 6.6)) +
  labs(x = "Pacer cycle within the trial", y = "Breath cycle (s)") + theme_fig()
p1 <- (f1a / f1b) + plot_layout(heights = c(1, 1.15)) + plot_annotation(tag_levels = "A")
save_fig(p1, "Fig1_design", height = 5)
captions <- c(captions, sprintf(paste0(
  "**Figure 1.** Study design. (A) Session order and the structure of one combined-task block. (B) Breath cycle ",
  "duration by pacer cycle, combined task (n = %d participants whose belt followed the pacer). Dashed lines: the ",
  "pacer's prescribed cycles (mean of each participant's schedule); points: observed cycles, mean and 95%% CI across ",
  "participants. High-salience changes step after cycle 2; low-salience changes ramp across cycles 2-4 to the same total. ",
  "Cycles 1-3 are timed trough to trough. *Cycle 4 is timed peak to peak (breath 3 to breath 4), against the pacer over ",
  "the same span, because its closing trough falls after the pacer stops."),
  length(unique(dur$id))))

# Figure 2: breathing manipulation ---------
f2a <- ggplot(bpart |> dplyr::filter(!is.na(sync)), aes(sync)) +
  geom_histogram(binwidth = 0.05, boundary = 0, fill = INK2, colour = "white", linewidth = 0.2) +
  geom_vline(xintercept = 0.4, linetype = 2, colour = INK, linewidth = 0.3) +
  annotate("text", x = 0.38, y = 45, hjust = 1, size = 2.5, colour = INK, family = "Arial",
           label = sprintf("included (>= .4)\nn = %d", length(ids_belt)), lineheight = 0.9) +
  labs(x = "Belt-pacer synchrony (median r over trials)", y = "Participants") + theme_fig()
det <- btr |> dplyr::filter(task == "combined", direction != 0, !is.na(high_salience), n_matched >= 2, is.finite(obs_ratio),
                            abs(log(obs_ratio)) <= log(2)) |>
  dplyr::mutate(followed = log(obs_ratio) * sign(log(exp_ratio)),
                salience = ifelse(high_salience, "High salience", "Low salience")) |>
  dplyr::group_by(id) |> dplyr::mutate(tert = dplyr::ntile(followed, 3)) |> dplyr::ungroup() |>
  dplyr::group_by(id, salience, tert) |> dplyr::summarise(v = 100 * mean(accuracy, na.rm = TRUE), .groups = "drop") |>
  mci(v, salience, tert)
f2b <- ggplot(det, aes(tert, m, linetype = salience, shape = salience)) +
  geom_line(colour = INK2, linewidth = 0.5, position = position_dodge(0.15)) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, colour = INK2, linewidth = 0.4, position = position_dodge(0.15)) +
  geom_point(size = 1.8, colour = INK, fill = "white", position = position_dodge(0.15)) +
  scale_shape_manual(values = c(`High salience` = 16, `Low salience` = 21)) +
  scale_linetype_manual(values = c(`High salience` = "solid", `Low salience` = "22")) +
  scale_x_continuous(breaks = 1:3, labels = c("Least", "Middle", "Most")) +
  labs(x = "Breathing change made in the\nprescribed direction (within-person tertile)", y = "Changes detected (%)") + theme_fig()
ar <- cbcat |> dplyr::filter(Direction != "NoChange", !is.na(Accuracy), !is.na(DirectionLabel)) |>
  dplyr::mutate(cond_dir = ifelse(DirectionLabel == "Acc", "Accelerate", "Decelerate"),
                detected = factor(ifelse(Accuracy == 1, "Detected", "Missed"), levels = c("Missed", "Detected"))) |>
  dplyr::group_by(id, cond_dir, detected) |> dplyr::summarise(v = mean(Arousal, na.rm = TRUE), .groups = "drop") |>
  mci(v, cond_dir, detected)
f2c <- ggplot(ar, aes(detected, m, colour = cond_dir, group = cond_dir)) +
  geom_line(linewidth = 0.5, position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 0.4, position = position_dodge(0.3)) +
  geom_point(size = 1.8, position = position_dodge(0.3)) +
  scale_colour_manual(values = dir_cols) + labs(x = "Change on that trial", y = "Felt arousal (1-6)") + theme_fig()
p2 <- (f2a | f2b | f2c) + plot_annotation(tag_levels = "A")
save_fig(p2, "Fig2_breathing_manipulation", height = 2.8)
captions <- c(captions, sprintf(paste0(
  "**Figure 2.** The breathing manipulation was enacted, detected and felt. (A) Belt-pacer synchrony for the %d ",
  "combined-task participants with a belt recording; belts either did not follow the pacer or followed it well, and ",
  "breathing analyses use those at or above .4. (B) Detection of change trials by how much the participant's breathing ",
  "actually changed in the prescribed direction (within-person tertiles), by salience. (C) Felt arousal after accelerating ",
  "and decelerating changes, by whether the change was detected (preregistered R2B; all %d combined-task participants). ",
  "Points: means of participant means; bars: 95%% CIs."), nrow(dplyr::filter(bpart, !is.na(sync))), length(ids_comb)))

# Figure 3: H2B ---------
th <- rd("BCAT_baseline_data.csv") |> dplyr::filter(id %in% excl$id[excl$set_bcat_baseline]) |>
  dplyr::group_by(id) |> dplyr::summarise(thresh = (ACCthresh[1] + DECthresh[1]) / 2,
                                          ceiling = ACCthresh[1] >= 1 | DECthresh[1] >= 1, .groups = "drop")
ga <- rd("GERT_baseline_data.csv") |> dplyr::group_by(id) |> dplyr::summarise(base = 100 * mean(emoAccuracy, na.rm = TRUE), .groups = "drop")
gc <- rd("Combined_fullGERT_data.csv") |> dplyr::group_by(id) |> dplyr::summarise(comb = 100 * mean(emoAccuracy, na.rm = TRUE), .groups = "drop")
h <- th |> dplyr::left_join(ga, by = "id") |> dplyr::left_join(gc, by = "id")
scatter_h2b <- function(y, ylab, title) {
  d <- h |> dplyr::filter(is.finite(.data[[y]]))
  ggplot(d, aes(thresh, .data[[y]])) +
    geom_point(aes(shape = ceiling), colour = INK2, alpha = 0.5, size = 1.1, stroke = 0.4) +
    geom_smooth(method = "lm", formula = y ~ x, colour = INK, fill = INK2, alpha = 0.15, linewidth = 0.6) +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 4), guide = "none") +
    labs(x = "Threshold (higher = less sensitive)", y = ylab, title = title, subtitle = rlab(d$thresh, d[[y]])) +
    theme_fig() + theme(plot.subtitle = element_text(size = 7, colour = INK2))
}
f3a <- scatter_h2b("base", "Recognition accuracy (%)", "Baseline GERT (22 clips)")
f3b <- scatter_h2b("comb", "Recognition accuracy (%)", "Combined task (60 different clips)")
he <- read.csv(file.path(resultsPath, "h2b_robustness", "estimates.csv"), stringsAsFactors = FALSE)
he$check <- factor(he$check, levels = rev(he$check))
f3c <- ggplot(he, aes(estimate, check)) +
  geom_vline(xintercept = 0, colour = INK2, linewidth = 0.3) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, colour = INK2, linewidth = 0.5) +
  geom_point(colour = INK, size = 1.6) +
  labs(x = "Standardised threshold effect on recognition (r or \u03b2, 95% CI)", y = NULL, title = "Robustness") +
  theme_fig() + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = GRID, linewidth = 0.3))
p3 <- ((f3a | f3b) / free(f3c)) + plot_layout(heights = c(1, 0.9)) + plot_annotation(tag_levels = "A")
save_fig(p3, "Fig3_H2B", height = 5.2)
captions <- c(captions, paste0(
  "**Figure 3.** Breath-change sensitivity and emotion recognition (preregistered H2B). (A) Mean breath-change threshold ",
  "(acceleration and deceleration) against baseline GERT accuracy; crosses mark thresholds at the task's ceiling. (B) The ",
  "same thresholds against accuracy on the combined task's 60 clips, none shared with baseline. Lines: least-squares fit ",
  "with 95% CI. (C) The threshold effect across robustness checks (correlations or standardised regression weights)."))

# Figure 4: awareness, felt arousal, intensity ---------
bdet <- cbcat |> dplyr::filter(Direction != "NoChange") |> dplyr::group_by(id, Block) |>
  dplyr::summarise(n_det = sum(Accuracy == 1, na.rm = TRUE), n_ch = dplyr::n(), .groups = "drop") |>
  dplyr::filter(n_ch == 2)
clip_c <- cgert |> dplyr::group_by(id) |> dplyr::mutate(int_pc = IntensityRating - mean(IntensityRating, na.rm = TRUE)) |>
  dplyr::ungroup() |> dplyr::inner_join(bdet, by = c("id", "Block"))
aw <- clip_c |> dplyr::group_by(id, n_det) |> dplyr::summarise(v = mean(int_pc, na.rm = TRUE), .groups = "drop") |> mci(v, n_det)
f4a <- ggplot(aw, aes(n_det, m)) + geom_hline(yintercept = 0, colour = INK2, linewidth = 0.3, linetype = 2) +
  geom_line(colour = INK2, linewidth = 0.5) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, colour = INK2, linewidth = 0.4) + geom_point(colour = INK, size = 1.8) +
  scale_x_continuous(breaks = 0:2, labels = c("0 of 2", "1 of 2", "2 of 2")) +
  labs(x = "Breathing changes detected in the block", y = "Clip intensity, person-centred") + theme_fig()
pm <- cblk |> dplyr::group_by(id) |> dplyr::summarise(arousal = mean(Arousal, na.rm = TRUE), .groups = "drop") |>
  dplyr::inner_join(cgert |> dplyr::group_by(id) |> dplyr::summarise(intensity = mean(IntensityRating, na.rm = TRUE), .groups = "drop"), by = "id")
f4b <- ggplot(pm, aes(arousal, intensity)) +
  geom_point(colour = INK2, alpha = 0.5, size = 1.1) +
  geom_smooth(method = "lm", formula = y ~ x, colour = INK, fill = INK2, alpha = 0.15, linewidth = 0.6) +
  labs(x = "Felt arousal, participant mean (1-6)", y = "Clip intensity, participant mean (1-7)",
       subtitle = rlab(pm$arousal, pm$intensity)) + theme_fig() + theme(plot.subtitle = element_text(size = 7, colour = INK2))
# Paths from the random-slope models (Analysis/random_slopes.R), the primary specification
rs <- read.csv(file.path(resultsPath, "random_slopes", "slopes.csv"), stringsAsFactors = FALSE)
path <- function(model, term) rs[rs$model == model & rs$term == term, ][1, ]
paths <- list(path("Chain: awareness ~ breathing change", "body_pc"),
              path("Chain: felt arousal ~ awareness + breathing change", "aware_pc"),
              path("Intensity ~ awareness + breathing change + felt arousal", "arousal_pc"))
direct <- path("Intensity ~ awareness + breathing change + felt arousal", "body_pc")
pe <- function(r) sprintf(if (abs(r$b) < 0.01) "b = %.3f, %s" else "b = %.2f, %s", r$b, fp(r$p))
sig <- vapply(paths, function(r) r$p < .05, logical(1))
nodes <- data.frame(x = c(1, 3.4, 5.8, 8.2), lab = c("Breathing change\nmade", "Change\nnoticed", "Felt\narousal", "Perceived\nintensity"))
f4c <- ggplot() + coord_cartesian(xlim = c(0.2, 9), ylim = c(-1.5, 1.3), expand = FALSE) + theme_void(base_family = "Arial") +
  theme(plot.tag = element_text(size = 10, face = "bold", family = "Arial")) +
  lapply(seq_len(nrow(nodes)), function(i) box(nodes$x[i], 0.45, 1.5, 0.75, nodes$lab[i], fill = if (i == 4) "#f0efec" else "white")) +
  lapply(1:3, function(i) annotate("segment", x = nodes$x[i] + 0.76, xend = nodes$x[i + 1] - 0.76, y = 0.45, yend = 0.45,
                                    colour = if (sig[i]) INK else INK2, linewidth = if (sig[i]) 0.6 else 0.4,
                                    linetype = if (sig[i]) "solid" else "22", arrow = arrow(length = unit(4, "pt"), type = "closed"))) +
  annotate("text", x = (nodes$x[1:3] + nodes$x[2:4]) / 2, y = 1.0, size = 2.4, colour = INK, family = "Arial",
           label = vapply(paths, pe, character(1))) +
  annotate("curve", x = nodes$x[1], xend = nodes$x[4], y = 0.05, yend = 0.05, curvature = 0.25, colour = INK2,
           linewidth = 0.3, linetype = "22", arrow = arrow(length = unit(3, "pt"), type = "closed")) +
  annotate("text", x = mean(nodes$x[c(1, 4)]), y = -1.3, size = 2.4, colour = INK2, family = "Arial",
           label = sprintf("direct: %s", pe(direct)))
p4 <- ((f4a | f4b) / f4c) + plot_layout(heights = c(1, 0.6)) + plot_annotation(tag_levels = "A")
save_fig(p4, "Fig4_awareness_arousal_intensity", height = 4.6)
captions <- c(captions, sprintf(paste0(
  "**Figure 4.** Awareness, felt arousal and perceived emotion intensity (combined task). (A) Intensity of the 5 clips ",
  "after a block, centred on each participant's mean, by how many of the block's 2 breathing changes were detected ",
  "(n = %d). (B) Participants who felt more aroused across the task rated clips as more intense (preregistered E2C, ",
  "between-person). (C) Within-person paths among participants whose belt followed the pacer (n = %d): solid arrows ",
  "p < .05, dashed arrows not. Models include by-participant random slopes for every within-person predictor, and the ",
  "intensity model random intercepts for clip. Bars: 95%% CIs."), length(unique(clip_c$id)), length(ids_belt)))

# Figure 5: felt vs cardiac arousal ---------
cp <- rd("physio", "cardiac_participants.csv")
ecg_ok <- cp$id[cp$ecg_quality >= 0.6 & cp$id %in% ids_comb]
ctr <- rd("physio", "cardiac_bcat_trials.csv") |> dplyr::filter(task == "combined", id %in% ecg_ok, pct_artifact <= 10) |>
  main_file()
hr_tc <- purrr::map_dfr(unique(ctr$id), function(pid) {
  rr <- readRDS(file.path(resultsPath, "physio", "rds", paste0(pid, "_cardiac.rds")))$rr
  x <- ctr[ctr$id == pid, ]
  purrr::map_dfr(seq_len(nrow(x)), function(k) {
    b <- vapply(0:12, function(s) { w <- rr[rr$t > x$t0[k] + s & rr$t <= x$t0[k] + s + 1 & rr$ok, ]
      if (nrow(w)) mean(60 / w$rr) else NA_real_ }, numeric(1))
    data.frame(id = pid, direction = x$direction[k], sec = 0:12 + 0.5, hr = b - mean(b[1:2], na.rm = TRUE))
  })
})
tc <- hr_tc |> dplyr::mutate(cond_dir = dplyr::case_when(direction == -1 ~ "Accelerate", direction == 1 ~ "Decelerate", TRUE ~ "No change")) |>
  dplyr::group_by(id, cond_dir, sec) |> dplyr::summarise(v = mean(hr, na.rm = TRUE), .groups = "drop") |> mci(v, cond_dir, sec)
f5a <- ggplot(tc, aes(sec, m, colour = cond_dir, fill = cond_dir)) +
  annotate("rect", xmin = 8, xmax = 13, ymin = -Inf, ymax = Inf, fill = "#f0efec") +
  geom_hline(yintercept = 0, colour = INK2, linewidth = 0.3, linetype = 2) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) + geom_line(linewidth = 0.6) +
  scale_colour_manual(values = dir_cols) + scale_fill_manual(values = dir_cols) +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  labs(x = "Seconds after trial onset", y = "Heart rate change from 0-2 s (bpm)", title = "Heart rate") + theme_fig()
fa <- cbcat |> dplyr::mutate(cond_dir = dplyr::case_when(Direction == "Faster" ~ "Accelerate", Direction == "Slower" ~ "Decelerate",
                                                         TRUE ~ "No change")) |>
  dplyr::group_by(id, cond_dir) |> dplyr::summarise(v = mean(Arousal, na.rm = TRUE), .groups = "drop") |> mci(v, cond_dir) |>
  dplyr::mutate(cond_dir = factor(cond_dir, levels = c("Decelerate", "No change", "Accelerate")))
f5b <- ggplot(fa, aes(cond_dir, m, colour = cond_dir)) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 0.5) + geom_point(size = 2) +
  scale_colour_manual(values = dir_cols, guide = "none") +
  labs(x = NULL, y = "Felt arousal (1-6)", title = "Felt arousal") + theme_fig()
pb <- cblk |> dplyr::group_by(id) |> dplyr::summarise(arousal = mean(Arousal, na.rm = TRUE), .groups = "drop") |>
  dplyr::inner_join(quest |> dplyr::select(id, BATtotal), by = "id")
rest <- cp |> dplyr::filter(id %in% excl$id[excl$set_questionnaire], ecg_quality >= 0.6, !is.na(rest_hr_bpm), rest_pct_artifact <= 10) |>
  dplyr::inner_join(quest |> dplyr::select(id, BATtotal), by = "id")
sc <- function(d, x, y, xl, yl, ttl) ggplot(d, aes(.data[[x]], .data[[y]])) +
  geom_point(colour = INK2, alpha = 0.5, size = 1) +
  geom_smooth(method = "lm", formula = y ~ x, colour = INK, fill = INK2, alpha = 0.15, linewidth = 0.6) +
  labs(x = xl, y = yl, title = ttl, subtitle = rlab(d[[x]], d[[y]])) +
  theme_fig() + theme(plot.subtitle = element_text(size = 7, colour = INK2))
f5c <- sc(pb, "BATtotal", "arousal", "Burnout (BAT-C)", "Felt arousal, task mean (1-6)", "Burnout and felt arousal")
f5d <- sc(rest, "BATtotal", "rest_hr_bpm", "Burnout (BAT-C)", "Pre-task heart rate (bpm)", "Burnout and heart rate")
p5 <- ((f5a | f5b) / (f5c | f5d)) + plot_layout(widths = c(1.6, 1)) + plot_annotation(tag_levels = "A")
save_fig(p5, "Fig5_felt_vs_cardiac", height = 5.2)
captions <- c(captions, sprintf(paste0(
  "**Figure 5.** Felt arousal followed the breathing change; heart rate did not. (A) Heart rate over the first 13 s of ",
  "paced trials, relative to the first 2 s, by direction of change (n = %d with a usable ECG lead). The saw-tooth is ",
  "respiratory sinus arrhythmia: heart rate rises with each 4-s paced inhalation and falls with the exhalation. Heart ",
  "rate fell over every trial, and the shaded window (8-13 s, after the change) did not differ by direction. (B) Felt arousal by ",
  "condition (n = %d). (C) Burnout and mean felt arousal. (D) Burnout and pre-task heart rate (equivalence ",
  "inconclusive; see Figure 6). Lines and ribbons: means and 95%% CIs across participants."),
  length(unique(hr_tc$id)), length(ids_comb)))

# Figure 6: equivalence ---------
# Within-person effects (intensity, accuracy, heart rate) from the random-slope models;
# correlations from equivalence_tests.R
eq_slope <- read.csv(file.path(resultsPath, "random_slopes", "equivalence.csv"), stringsAsFactors = FALSE)
eq <- rd("equivalence", "equivalence.csv") |> dplyr::distinct(test, .keep_all = TRUE) |>
  dplyr::rows_update(eq_slope |> dplyr::select(test, estimate, ci90_lo, ci90_hi, equivalent), by = "test", unmatched = "ignore") |>
  dplyr::mutate(panel = dplyr::case_when(unit == "r" ~ "Correlations (r)", unit == "bpm" ~ "Heart-rate change (bpm)",
                                         unit == "proportion" ~ "Recognition accuracy (proportion)",
                                         TRUE ~ "Clip intensity (scale points)"),
                verdict = ifelse(equivalent, "Equivalent to zero", "Inconclusive"),
                test = sub(" \\(n = \\d+\\)$", "", test))
eq$test <- factor(eq$test, levels = rev(unique(eq$test)))
f6 <- ggplot(eq, aes(estimate, test)) +
  geom_rect(aes(xmin = -sesoi, xmax = sesoi, ymin = -Inf, ymax = Inf), fill = "#f0efec", inherit.aes = FALSE,
            data = dplyr::distinct(eq, panel, sesoi)) +
  geom_vline(xintercept = 0, colour = INK2, linewidth = 0.3) +
  geom_errorbarh(aes(xmin = ci90_lo, xmax = ci90_hi, colour = verdict), height = 0, linewidth = 0.6) +
  geom_point(aes(colour = verdict, shape = verdict), size = 1.8) +
  scale_colour_manual(values = c(`Equivalent to zero` = INK, Inconclusive = INK2)) +
  scale_shape_manual(values = c(`Equivalent to zero` = 16, Inconclusive = 2)) +
  facet_wrap(~ panel, scales = "free", ncol = 1) + scale_x_continuous(n.breaks = 6) +
  labs(x = "Estimate with 90% CI; shaded band = smallest effect of interest", y = NULL) +
  theme_fig() + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = GRID, linewidth = 0.3))
save_fig(f6, "Fig6_equivalence", height = 6.2)
captions <- c(captions, paste0(
  "**Figure 6.** Equivalence tests for the key null results (two one-sided tests, alpha = .05). An effect is equivalent ",
  "to zero when its 90% CI lies inside the shaded band: |r| = .17 (the size of H2B), 0.05 intensity points, 2 accuracy ",
  "points, 1 bpm. Within-person effects come from models with by-participant random slopes. Open triangles mark ",
  "inconclusive results."))

writeLines(c("# Figure captions", "", sprintf("Generated %s by `Analysis/make_figures.R`. Files: `Fig*.pdf` (vector) and `Fig*.png` (600 dpi).", format(Sys.Date())), "",
             paste(captions, collapse = "\n\n")), file.path(figPath, "captions.md"))
message("Figures written to ", figPath)
