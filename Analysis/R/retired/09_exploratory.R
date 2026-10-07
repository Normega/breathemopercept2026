# R/09_exploratory.R
# Exploratory analyses and multilevel path model.
# ---------------------------------------------------------------

message("09 | Exploratory analyses...")

# ── Burnout and arousal ────────────────────────────────────────
model.burnout.arousal <- lmer(Arousal ~ BATtotal + (1 | id), data = fullData)
tab_model(model.burnout.arousal)

with(traitData,    cor.test(BATtotal, combinedArousal))
with(bothBaseQ_data, cor.test(BATtotal, baseArousal))

# ── Arousal and GERT outcomes ──────────────────────────────────
lm.Acc.Arousal       <- lmer(emoAccuracy  ~ Arousal + (1 | id), data = fullData)
lm.Intensity.Arousal <- lmer(emoIntensity ~ Arousal + (1 | id), data = fullData)
lm.Distance.Arousal  <- lmer(emoDistance  ~ Arousal + (1 | id), data = fullData)
tab_model(lm.Acc.Arousal, lm.Intensity.Arousal, lm.Distance.Arousal,
          dv.labels = c("Accuracy", "Intensity", "Distance"))

lm.Intensity.Arousal.Burnout <- lmer(
  emoIntensity ~ Arousal + BATtotal + (1 | id), data = fullData
)
tab_model(lm.Intensity.Arousal, lm.Intensity.Arousal.Burnout,
          dv.labels = c("Intensity (arousal)", "Intensity (arousal + burnout)"))

# ── BCAT accuracy and direction on arousal / intensity ─────────
BCATfx.arousal   <- lmer(Arousal      ~ DirectionLabel * bcatAccuracy + (1 | id), data = fullData)
BCATfx.intensity <- lmer(emoIntensity ~ bcatAccuracy + (1 | id), data = fullData)
BCATfx.int.dir   <- lmer(emoIntensity ~ bcatAccuracy * DirectionLabel + (1 | id), data = fullData)
tab_model(BCATfx.arousal)
tab_model(BCATfx.intensity, BCATfx.int.dir,
          dv.labels = c("Intensity (acc)", "Intensity (acc x direction)"))

# ── Trait-level correlation matrix ────────────────────────────
# Select numeric columns only after dropping id and any residual
# character/factor columns that survive the merge (e.g. Gender, seat).
traitData_complete <- traitData |>
  dplyr::select(-id) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.factor), as.numeric)) |>
  dplyr::select(dplyr::where(is.numeric)) |>
  dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.)))
cmat <- cor(traitData_complete)

# ── Multilevel path model ──────────────────────────────────────
# Person-mean centre within-person variables; z-score between-person burnout
fullData <- fullData |>
  dplyr::group_by(id) |>
  dplyr::mutate(
    bcatAccuracy_c = bcatAccuracy - mean(bcatAccuracy, na.rm = TRUE),
    Arousal_c      = Arousal      - mean(Arousal,      na.rm = TRUE)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    BATtotal_z = as.numeric(scale(BATtotal)),
    Dir_acc    = as.integer(DirectionLabel == "Acc"),
    Sal_hi     = as.integer(Salience == "High")
  )

path_model <- '
  level: 1
    Arousal      ~ a1*Dir_acc + a2*Sal_hi + a3*bcatAccuracy_c
                 + a4*Dir_acc:Sal_hi + a5*Dir_acc:bcatAccuracy_c
    emoIntensity ~ b1*Arousal + cp1*Dir_acc
    emoAccuracy  ~ b2*Arousal + cp2*Dir_acc
    emoDistance  ~ b3*Arousal + cp3*Dir_acc
  level: 2
    Arousal      ~ g1*BATtotal_z
    Arousal      ~~ Arousal
    emoIntensity ~~ emoIntensity
    emoAccuracy  ~~ emoAccuracy
    emoDistance  ~~ emoDistance
    BATtotal_z   ~~ BATtotal_z
'
path_defined <- '
  indirect_intensity       := a1 * b1
  indirect_accuracy        := a1 * b2
  indirect_distance        := a1 * b3
  indirect_burnout_intensity := g1 * b1
'

path_fit <- sem(paste(path_model, path_defined),
                data = fullData, cluster = "id",
                estimator = "MLR", se = "robust")

summary(path_fit, fit.measures = TRUE, rsquare = TRUE)

parameterEstimates(path_fit) |>
  dplyr::filter(op == ":=") |>
  dplyr::select(label, est, se, ci.lower, ci.upper, pvalue) |>
  print()

message("09 | Exploratory complete.")
