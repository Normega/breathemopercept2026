# functions.R
# Shared helper functions sourced by 00_packages.R.
# ---------------------------------------------------------------

# ── GERT emotion circle ────────────────────────────────────────
emotion_circle <- c("Anger", "Pride", "Joy", "Amusement", "Pleasure", "Relief",
                    "Interest", "Surprise", "Anxiety", "Fear", "Despair",
                    "Sadness", "Disgust", "Irritation")
n_emotions <- length(emotion_circle)  # 14

# Circular distance between two emotion labels (vectorised).
# Returns shortest path around the 14-emotion circle (0 to 7).
circular_distance <- function(response, correct, circle = emotion_circle) {
  pos_response <- match(response, circle)
  pos_correct  <- match(correct,  circle)

  if (any(is.na(pos_response)))
    warning("Unrecognised emotion in 'response': ",
            paste(unique(response[is.na(pos_response)]), collapse = ", "))
  if (any(is.na(pos_correct)))
    warning("Unrecognised emotion in 'correct': ",
            paste(unique(correct[is.na(pos_correct)]), collapse = ", "))

  raw_dist <- abs(pos_response - pos_correct)
  pmin(raw_dist, n_emotions - raw_dist)
}

# ── GERT confusion matrix ──────────────────────────────────────
# Returns list(data = df, plot = ggplot)
build_confusion_matrix <- function(data,
                                   response_col = "emoResponse",
                                   correct_col  = "vidCorrect",
                                   circle       = emotion_circle) {
  conf_df <- data |>
    dplyr::count(.data[[correct_col]], .data[[response_col]], name = "n") |>
    tidyr::complete(
      !!rlang::sym(correct_col)  := circle,
      !!rlang::sym(response_col) := circle,
      fill = list(n = 0)
    ) |>
    dplyr::group_by(.data[[correct_col]]) |>
    dplyr::mutate(prop = n / sum(n)) |>
    dplyr::ungroup() |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(c(correct_col, response_col)),
      ~factor(., levels = circle)
    ))

  p <- ggplot(conf_df, aes(x = .data[[response_col]],
                           y = .data[[correct_col]],
                           fill = prop)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_text(aes(label = ifelse(n > 0, n, "")), size = 3, colour = "white") +
    scale_fill_gradient(low = "#2c3e50", high = "#e74c3c",
                        labels = scales::percent_format(accuracy = 1),
                        name   = "Row %") +
    scale_x_discrete(position = "top") +
    labs(title = "GERT Emotion Recognition - Confusion Matrix",
         x = "Response", y = "Correct Emotion") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x  = element_text(angle = 45, hjust = 0),
          panel.grid   = element_blank())

  list(data = conf_df, plot = p)
}

# ── Scatter plot with r, R², p annotation ─────────────────────
plot_scatter <- function(df, x, y,
                         x_label     = x,
                         y_label     = y,
                         title       = NULL,
                         color       = "#2E4A7A",
                         alpha       = 0.65,
                         point_size  = 2.5,
                         point_style = c("point", "jitter", "none"),
                         jitter_width = 0.15) {

  point_style <- match.arg(point_style)
  if (!x %in% names(df)) stop(sprintf("Column '%s' not found.", x))
  if (!y %in% names(df)) stop(sprintf("Column '%s' not found.", y))

  x_vec    <- df[[x]]
  y_vec    <- df[[y]]
  complete <- stats::complete.cases(x_vec, y_vec)
  if (sum(!complete) > 0)
    message(sprintf("plot_scatter: %d rows excluded (missing values).", sum(!complete)))

  x_vec <- x_vec[complete]; y_vec <- y_vec[complete]
  if (length(x_vec) < 3) stop("Need at least 3 complete observations.")

  fit     <- stats::lm(y_vec ~ x_vec)
  r_val   <- stats::cor(x_vec, y_vec)
  r2      <- summary(fit)$r.squared
  p_val   <- summary(fit)$coefficients[2, 4]
  p_label <- if (p_val < .001) "p < .001" else sprintf("p = %.3f", p_val)

  stats_label <- sprintf(
    "italic(R) == '%.3f'~~ italic(R)^2 == '%.3f'~~ '%s'~~ italic(n) == '%d'",
    r_val, r2, p_label, length(x_vec)
  )

  plot_df <- data.frame(x = x_vec, y = y_vec)

  p <- ggplot(plot_df, aes(x = x, y = y)) +
    geom_smooth(method = "lm", formula = y ~ x,
                color = color, fill = scales::alpha(color, 0.10),
                linewidth = 0.9, se = TRUE)

  p <- switch(point_style,
    "point"  = p + geom_point(color = color, alpha = alpha,
                              size = point_size, shape = 16),
    "jitter" = p + geom_jitter(color = color, alpha = alpha,
                               size = point_size, shape = 16,
                               width = jitter_width, height = 0),
    "none"   = p
  )

  p + annotate("text", x = Inf, y = -Inf, label = stats_label,
               hjust = 1.05, vjust = -0.7, size = 3.4,
               color = "#444444", parse = TRUE, family = "sans") +
    labs(x = x_label, y = y_label, title = title) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.major = element_line(color = "#EBEBEB", linewidth = 0.4),
          panel.grid.minor = element_blank(),
          panel.border     = element_rect(color = "#CCCCCC", fill = NA, linewidth = 0.5),
          plot.background  = element_rect(fill = "white", color = NA),
          axis.title       = element_text(size = 12, color = "#333333"),
          axis.text        = element_text(size = 10, color = "#555555"),
          plot.margin      = margin(16, 20, 12, 12))
}

# ── Interaction plot: moderator split into tertiles ────────────
plot_interaction_tertiles <- function(model, pred, mod,
                                      pred_label    = NULL,
                                      mod_label     = NULL,
                                      outcome_label = NULL,
                                      n_pred        = 50,
                                      colors = c("#2C7BB6", "#878787", "#D7191C")) {
  mf          <- model.frame(model)
  outcome_var <- names(mf)[1]
  if (!pred %in% names(mf)) stop("'", pred, "' not found in model frame.")
  if (!mod  %in% names(mf)) stop("'", mod,  "' not found in model frame.")

  tbreaks <- quantile(mf[[mod]], probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
  t_vals  <- c(
    median(mf[[mod]][mf[[mod]] <= tbreaks[2]], na.rm = TRUE),
    median(mf[[mod]][mf[[mod]] >  tbreaks[2] & mf[[mod]] <= tbreaks[3]], na.rm = TRUE),
    median(mf[[mod]][mf[[mod]] >  tbreaks[3]], na.rm = TRUE)
  )
  t_labs <- c(sprintf("Low (<= %.2f)", tbreaks[2]), "Mid",
               sprintf("High (> %.2f)", tbreaks[3]))

  pred_range <- seq(min(mf[[pred]], na.rm = TRUE),
                    max(mf[[pred]], na.rm = TRUE), length.out = n_pred)

  pred_grid <- purrr::map_dfr(seq_along(t_vals), function(i) {
    g <- mf[rep(1, n_pred), ]
    g[[pred]] <- pred_range
    g[[mod]]  <- t_vals[i]
    g$tertile <- t_labs[i]
    g
  })

  pred_grid$fit     <- predict(model, newdata = pred_grid, re.form = NA)
  pred_grid$tertile <- factor(pred_grid$tertile, levels = t_labs)

  ggplot(pred_grid, aes(x = .data[[pred]], y = fit, colour = tertile)) +
    geom_line(linewidth = 1) +
    scale_colour_manual(values = colors,
                        name = if (!is.null(mod_label)) mod_label else mod) +
    labs(x = if (!is.null(pred_label))    pred_label    else pred,
         y = if (!is.null(outcome_label)) outcome_label else outcome_var) +
    theme_classic(base_size = 13) +
    theme(legend.position  = "right",
          panel.grid.major = element_line(colour = "grey92"))
}

# ── Task-file index ────────────────────────────────────────────
# One row per PsychoPy task file, with the participant ID it belongs to after
# documented corrections (id_corrections.csv) and the rules every prep step
# shares. A file is excluded as a test if its ID is a known test ID, is not a
# number, or the file predates data collection (pilot sessions in Oct 2025).
TASK_PATTERNS <- c(bcat_baseline = "_Intero2025_", gert_baseline = "_GERT_baseline_",
                   combined = "_CombinedTask_")

task_file_index <- function(dir = taskDataPath, corrections = idCorrectionsFile) {
  f <- list.files(dir, pattern = "[.]csv$")
  task <- rep(NA_character_, length(f))
  for (nm in names(TASK_PATTERNS)) task[grepl(TASK_PATTERNS[[nm]], f, fixed = TRUE)] <- nm
  f <- f[!is.na(task)]; task <- task[!is.na(task)]
  stamp <- sub("^.*_(\\d{4}-\\d{2}-\\d{2}_\\d{2}h\\d{2}\\.\\d{2}\\.\\d{3})\\.csv$", "\\1", f)
  idx <- data.frame(
    file = f, task = task,
    raw_id = sub("^([^_]+)_.*$", "\\1", f),
    date = as.Date(substr(stamp, 1, 10)),
    stamp = stamp,
    size = file.info(file.path(dir, f))$size,
    stringsAsFactors = FALSE)
  idx$id <- sub("\\.0$", "", idx$raw_id)
  idx$correction <- ""
  if (file.exists(corrections)) {
    cr <- utils::read.csv(corrections, stringsAsFactors = FALSE, colClasses = "character")
    cr <- cr[cr$source == "task_file", ]
    for (k in seq_len(nrow(cr))) {
      hit <- grepl(cr$file_regex[k], idx$file, perl = TRUE)
      if (sum(hit) != 1)
        warning("id_corrections.csv row ", k, " matches ", sum(hit), " files (expected 1)")
      idx$id[hit] <- cr$corrected_id[k]
      idx$correction[hit] <- paste("id corrected to", cr$corrected_id[k])
    }
  }
  idx$test <- idx$id %in% sub("\\.0$", "", TEST_IDS) | !grepl("^[0-9]+$", idx$id) |
    idx$date < DATA_START
  idx[order(idx$id, idx$task, idx$stamp), ]
}

# The file a prep step uses for each participant and task. The earliest file
# of at least `min_size` bytes (a complete run) wins; a participant with only
# smaller files keeps the largest, so later steps can count how far they got
# rather than having them vanish.
select_task_files <- function(idx, which_task, min_size) {
  x <- idx[idx$task == which_task & !idx$test, ]
  x <- x[order(x$id, -(x$size >= min_size), ifelse(x$size >= min_size, 0, -x$size), x$stamp), ]
  x <- x[!duplicated(x$id), ]
  x$complete_run <- x$size >= min_size
  x
}
