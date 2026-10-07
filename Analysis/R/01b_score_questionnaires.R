# R/01b_score_questionnaires.R
# Scores every questionnaire from item-level responses and writes
# questionnaireFile.csv plus reliability and descriptive tables. In analysis
# mode the items come from the de-identified dataset; 01 writes the lab copy.
# ---------------------------------------------------------------

message("01b | Questionnaire scoring...")

qData <- read.csv(questionnaireItemsFile, stringsAsFactors = FALSE, colClasses = c(id = "character"))

my.keys <- list(
  Extraversion         = c("-BFI10_1", "BFI10_6"),
  Agreeableness        = c("BFI10_2", "-BFI10_7"),
  Neuroticism          = c("-BFI10_4", "BFI10_9"),
  Openness             = c("-BFI10_5", "BFI10_10"),
  Conscientiousness    = c("-BFI10_3", "BFI10_8"),
  PosAffect            = c("SPANE_1", "SPANE_3", "SPANE_5", "SPANE_7", "SPANE_10", "SPANE_12"),
  NegAffect            = c("SPANE_2", "SPANE_4", "SPANE_6", "SPANE_8", "SPANE_9", "SPANE_11"),
  Depression           = c("PHQ4_3", "PHQ4_4"),
  Anxiety              = c("PHQ4_1", "PHQ4_2"),
  PHQ4total            = paste0("PHQ4_", 1:4),
  PushedStress         = c("BIPS_1", "BIPS_2", "BIPS_3"),
  ConflictAndImposition = c("BIPS_5", "BIPS_6", "BIPS_7"),
  LackOfControl        = c("BIPS_8", "-BIPS_9", "BIPS_10"),
  BIPStotal            = c("BIPS_1", "BIPS_2", "BIPS_3", "BIPS_5", "BIPS_6",
                           "BIPS_7", "BIPS_8", "-BIPS_9", "BIPS_10"),
  SWLS                 = "-SWLS",
  Noticing             = paste0("BriefMAIA_", 1:3),
  Notdistracting       = paste0("-BriefMAIA_", 4:6),
  NotWorrying          = paste0("-BriefMAIA_", 7:9),
  AttentionRegulation  = paste0("BriefMAIA_", 10:12),
  EmotionalAwareness   = paste0("BriefMAIA_", 13:15),
  SelfRegulation       = paste0("BriefMAIA_", 16:18),
  BodyListening        = paste0("BriefMAIA_", 19:21),
  Trusting             = paste0("BriefMAIA_", 22:24),
  MAIAtotal            = c(paste0("BriefMAIA_", 1:3), paste0("-BriefMAIA_", 4:9),
                           paste0("BriefMAIA_", 10:24)),
  Exhaustion           = paste0("BAT_", 1:8),
  MentalDistance       = paste0("BAT_", 9:13),
  ExhaustionImpairment = paste0("BAT_", 14:18),
  CognitiveImpairement = paste0("BAT_", 19:23),
  BATtotal             = paste0("BAT_", 1:23),
  BARQtotal            = paste0("BARQR_", 1:12)
)

my.scales <- scoreItems(my.keys, qData, totals = TRUE)
pData <- data.frame(my.scales$scores)
pData$id        <- qData$id
pData$Age       <- qData$Age
pData$YearStudy <- qData$YearStudy
pData$Gender    <- factor(qData$Gender)
pData$attention_pass <- qData$attention_pass
pData$n_complete_submissions <- qData$n_complete_submissions

# Reliability and descriptives describe the analysed sample: attention-check
# passes only.
qAll <- qData; pAll <- pData
qData <- qData[qData$attention_pass, ]
pData <- pData[pData$attention_pass, ]

# ── Scale reliability and descriptives ────────────────────────
# Three output files capturing item- and scale-level information
# that is not recoverable from the scored totals in questionnaireFile.csv.
#
# Helper: strip leading "-" from reverse-keyed item names
.strip_key <- function(x) sub("^-", "", x)

# Helper: extract a scored (direction-applied) item matrix for one scale
# scoreItems stores the rescored items in $scores only as totals, but the
# working item matrix (with reversals applied) is recoverable via scoreFast.
.scale_items <- function(keys, data) {
  item_names <- .strip_key(keys)
  mat        <- data[, item_names, drop = FALSE]
  # Apply reversals: for negatively keyed items, reverse within observed range
  for (k in keys) {
    col <- .strip_key(k)
    if (startsWith(k, "-")) {
      rng      <- range(mat[[col]], na.rm = TRUE)
      mat[[col]] <- rng[1] + rng[2] - mat[[col]]
    }
  }
  mat
}

# ── 1. Scale reliability table ─────────────────────────────────
# One row per scale/subscale: alpha, omega_total, mean inter-item r, n_items, n_valid
message("  Computing reliability indices...")

reliability_rows <- list()

for (scale_name in names(my.keys)) {
  keys      <- my.keys[[scale_name]]
  item_mat  <- .scale_items(keys, qData)
  n_items   <- ncol(item_mat)
  n_valid   <- sum(complete.cases(item_mat))
  
  # Cronbach's alpha via psych::alpha()
  alpha_out <- tryCatch(
    psych::alpha(item_mat, warnings = FALSE),
    error = function(e) NULL
  )
  alpha_val  <- if (!is.null(alpha_out)) round(alpha_out$total$raw_alpha, 3) else NA_real_
  mean_r_val <- if (!is.null(alpha_out)) round(alpha_out$total$average_r,  3) else NA_real_
  
  # McDonald's omega via psych::omega() -- suppress noisy output
  omega_val <- NA_real_
  if (n_items >= 3) {
    omega_out <- tryCatch(
      suppressMessages(suppressWarnings(
        psych::omega(item_mat, plot = FALSE, nfactors = 1)
      )),
      error = function(e) NULL
    )
    if (!is.null(omega_out))
      omega_val <- round(omega_out$omega.tot, 3)
  }
  
  reliability_rows[[scale_name]] <- data.frame(
    scale        = scale_name,
    n_items      = n_items,
    n_valid      = n_valid,
    alpha        = alpha_val,
    omega_total  = omega_val,
    mean_inter_r = mean_r_val,
    stringsAsFactors = FALSE
  )
}

reliability_df <- dplyr::bind_rows(reliability_rows)
write.csv(reliability_df,
          scaleReliabilityFile,
          row.names = FALSE)
message("  -> Written: scale_reliability.csv")

# ── 2. Item descriptives table ─────────────────────────────────
# One row per item (across all scales it appears in):
# mean, SD, n_missing, item-total correlation (from scoreItems$item.stats)
message("  Computing item descriptives...")

# scoreItems$item.stats has one row per unique item
item_stats_raw <- as.data.frame(my.scales$item.stats)
item_stats_raw$item <- rownames(item_stats_raw)

# Per-item mean, SD, n_missing from raw (un-reversed) data
all_items <- unique(.strip_key(unlist(my.keys)))
item_desc_rows <- lapply(all_items, function(itm) {
  if (!itm %in% names(qData)) return(NULL)
  x <- qData[[itm]]
  data.frame(
    item      = itm,
    mean      = round(mean(x, na.rm = TRUE), 3),
    sd        = round(sd(x,   na.rm = TRUE), 3),
    n_missing = sum(is.na(x)),
    stringsAsFactors = FALSE
  )
})
item_desc <- dplyr::bind_rows(item_desc_rows)

# Merge in item-total r from scoreItems (column "r" = corrected item-total r)
if ("r" %in% names(item_stats_raw)) {
  item_stats_sub <- item_stats_raw[, c("item", "r"), drop = FALSE]
  names(item_stats_sub)[2] <- "item_total_r"
  item_stats_sub$item_total_r <- round(item_stats_sub$item_total_r, 3)
  item_desc <- dplyr::left_join(item_desc, item_stats_sub, by = "item")
}

# Note which scales each item belongs to
item_scale_map <- lapply(names(my.keys), function(s) {
  data.frame(item  = .strip_key(my.keys[[s]]),
             scale = s,
             stringsAsFactors = FALSE)
})
item_scale_map <- dplyr::bind_rows(item_scale_map) |>
  dplyr::group_by(item) |>
  dplyr::summarise(scales = paste(scale, collapse = "; "), .groups = "drop")

item_desc <- dplyr::left_join(item_desc, item_scale_map, by = "item")

write.csv(item_desc,
          itemDescriptivesFile,
          row.names = FALSE)
message("  -> Written: item_descriptives.csv")

# ── 3. Scale descriptives table ────────────────────────────────
# One row per scale: mean, SD, min, max, n_valid, n_missing of the total score
message("  Computing scale descriptives...")

scale_desc_rows <- lapply(names(my.keys), function(s) {
  x <- pData[[s]]
  data.frame(
    scale     = s,
    mean      = round(mean(x, na.rm = TRUE), 3),
    sd        = round(sd(x,   na.rm = TRUE), 3),
    min       = round(min(x,  na.rm = TRUE), 3),
    max       = round(max(x,  na.rm = TRUE), 3),
    n_valid   = sum(!is.na(x)),
    n_missing = sum(is.na(x)),
    stringsAsFactors = FALSE
  )
})
scale_desc <- dplyr::bind_rows(scale_desc_rows)

write.csv(scale_desc,
          scaleDescriptivesFile,
          row.names = FALSE)
message("  -> Written: scale_descriptives.csv")

pData <- pAll; qData <- qAll
message(sprintf("  Participants written: %d (attention passes: %d)", nrow(pData), sum(pData$attention_pass)))
write.csv(pData, questionnaireFile, row.names = FALSE)
message("  -> Written: questionnaireFile.csv")