# R/08_hypotheses.R
# Confirmatory hypotheses H1-H4 with Benjamini-Hochberg correction.
# ---------------------------------------------------------------

message("08 | Confirmatory hypotheses...")

# Block-level awareness classification
combData_clean$Aware     <- combData_clean$bcatAccuracy >= AWARE_BLOCK_CUTOFF
combData_clean$AccelAware <- combData_clean$DirectionLabel == "Acc" & combData_clean$Aware

h3h4_data <- dplyr::left_join(
  combData_clean,
  dplyr::select(bothBaseQ_data, id, BATtotal),
  by = "id"
)

# ── H1: Burnout and baseline emotion processing ────────────────
H1a.test <- with(baseGERTwithQ_data, cor.test(basegertAccuracy,  BATtotal))
H1b.test <- with(baseGERTwithQ_data, cor.test(basegertIntensity, BATtotal))
H1e.test <- with(baseGERTwithQ_data, cor.test(basegertDistance,  BATtotal))

# ── H2: Interoceptive sensitivity and emotion processing ───────
H2a.test <- with(bothBaseQ_data, cor.test(BATtotal,          baseThresh))
H2b.test <- with(bothBaseQ_data, cor.test(basegertAccuracy,  baseThresh))
H2c.test <- with(bothBaseQ_data, cor.test(basegertIntensity, baseThresh))

# ── BH correction across H1A, H1B, H1E, H2A, H2B, H2C ────────
corr_tests <- list(H1a = H1a.test, H1b = H1b.test, H1e = H1e.test,
                   H2a = H2a.test, H2b = H2b.test, H2c = H2c.test)

p_raw  <- vapply(corr_tests, `[[`, numeric(1), "p.value")
p_BH   <- p.adjust(p_raw, method = "BH")

corr_results <- data.frame(
  hypothesis = names(corr_tests),
  r          = vapply(corr_tests, function(t) unname(t$estimate), numeric(1)),
  df         = vapply(corr_tests, function(t) unname(t$parameter), numeric(1)),
  p_raw      = p_raw,
  p_BH       = p_BH,
  sig_BH     = p_BH < BH_ALPHA
)
print(corr_results)

# ── H3: Respiration condition and emotion processing ───────────
H3A.model  <- lmer(emoAccuracy  ~ AccelAware + (1 | id), data = combData_clean)
H3B.model  <- lmer(emoIntensity ~ AccelAware + (1 | id), data = combData_clean)
H3E.model  <- lmer(emoDistance  ~ AccelAware + (1 | id), data = combData_clean)

H3A.expand <- lmer(emoAccuracy  ~ DirectionLabel * Aware + (1 | id), data = combData_clean)
H3B.expand <- lmer(emoIntensity ~ DirectionLabel * Aware + (1 | id), data = combData_clean)
H3E.expand <- lmer(emoDistance  ~ DirectionLabel * Aware + (1 | id), data = combData_clean)

tab_model(H3A.model, H3B.model, H3E.model,
          dv.labels = c("Accuracy", "Intensity", "Distance"))
tab_model(H3A.expand, H3B.expand, H3E.expand,
          dv.labels = c("Accuracy", "Intensity", "Distance"))

# ── H4: Burnout moderates respiration effects (high-salience) ──
hiSalData <- h3h4_data |> dplyr::filter(Salience == "High")

H4A.main <- lmer(emoAccuracy  ~ DirectionLabel + BATtotal + (1 | id), data = hiSalData)
H4A.int  <- lmer(emoAccuracy  ~ DirectionLabel * BATtotal + (1 | id), data = hiSalData)
H4B.main <- lmer(emoIntensity ~ DirectionLabel + BATtotal + (1 | id), data = hiSalData)
H4B.int  <- lmer(emoIntensity ~ DirectionLabel * BATtotal + (1 | id), data = hiSalData)
H4E.main <- lmer(emoDistance  ~ DirectionLabel + BATtotal + (1 | id), data = hiSalData)
H4E.int  <- lmer(emoDistance  ~ DirectionLabel * BATtotal + (1 | id), data = hiSalData)

tab_model(H4A.main, H4A.int, dv.labels = c("Accuracy (main)", "Accuracy (int.)"))
tab_model(H4B.main, H4B.int, dv.labels = c("Intensity (main)", "Intensity (int.)"))
tab_model(H4E.main, H4E.int, dv.labels = c("Distance (main)",  "Distance (int.)"))

# ── Manipulation check ─────────────────────────────────────────
combBCAT_clean$DirectionLabel <- factor(combBCAT_clean$DirectionLabel, levels = c("Acc", "Dec"))
combBCAT_clean$Salience       <- factor(combBCAT_clean$Salience,       levels = c("High", "Low"))

manip_check <- glmer(Accuracy ~ DirectionLabel * Salience + (1 | id),
                     data = combBCAT_clean, family = binomial)
tab_model(manip_check)

message("08 | Hypotheses complete.")
