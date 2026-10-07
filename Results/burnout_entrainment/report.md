# Burnout and entrainment

Run 2026-10-07. Script `Analysis/burnout_entrainment.R`; full output in `models.txt`.
Exploratory (not preregistered); p values uncorrected. Breathing change is the four-cycle measure (`Methods/breath_trial_features.md`).

## Sample

- 224 combined-task participants whose belt followed the pacer and who have a burnout score; 7009 usable paced trials.
- Burnout (BAT total) here: M = 69.5, SD = 14.7, range 31-110.
- Per-participant entrainment gain: M = 0.88, SD = 0.18; split-half reliability (Spearman-Brown) 0.36.

## Did burnout weaken the body's response to the pacer?

- Burnout and entrainment gain: r = .010, 95% CI [-.122, .141], p = .885, n = 224; 90% CI [-.101, .120], equivalence yes (p_TOST = .008).
- Burnout and direction compliance: r = -.038, 95% CI [-.169, .093], p = .568, n = 224; 90% CI [-.148, .072], equivalence yes (p_TOST = .024).
- Burnout and belt-pacer synchrony: r = -.087, 95% CI [-.215, .045], p = .197, n = 224; 90% CI [-.195, .024], equivalence not shown (p_TOST = 0.103).
- Trial level, combined task, burnout (per SD) x prescribed change: b = 0.002, 95% CI [-0.019, 0.023], p = .867 (main effect of prescribed change, i.e. the gain at mean burnout: b = 0.869, 95% CI [0.848, 0.891], p < .001).
- Trial level, baseline BCAT: burnout x prescribed change b = -0.004, 95% CI [-0.018, 0.010], p = .562.

## Felt arousal

- Burnout and mean felt arousal in this subsample: r = -0.164, p = .014.
- Entrainment gain and mean felt arousal: r = 0.069, p = .303.
- Holding gain and compliance constant, burnout still predicted lower felt arousal: standardised beta = -0.161, p = .016.
- Trial level: felt arousal rose with the prescribed speeding (b = 0.497, 95% CI [0.378, 0.617], p < .001). Burnout (per SD) lowered its level: b = -0.167, 95% CI [-0.298, -0.037], p = .013. Burnout x prescribed speeding: b = 0.126, 95% CI [0.008, 0.244], p = .037.
- Sensitivity, change trials, direction coding: accelerate vs decelerate b = 0.260, 95% CI [0.196, 0.325], p < .001; burnout b = -0.167, 95% CI [-0.297, -0.037], p = .012; burnout x direction b = 0.059, 95% CI [-0.007, 0.124], p = .081.
