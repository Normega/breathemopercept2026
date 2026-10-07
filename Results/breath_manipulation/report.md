# Breath manipulation check

Run 2026-10-07. Script `Analysis/breath_manipulation_check.R`; full output in `models.txt`.
Exploratory (not preregistered); p values uncorrected.

## Sample

- 283 participants in the combined-task sample with physio; 224 have a belt that follows the pacer (median belt-pacer correlation >= 0.4). The distribution is bimodal: belts either do not follow at all (about 0) or follow well (mostly .6-.9).
- 12145 paced trials from those participants; 1018 (8.4%) dropped because fewer than 2 of 4 pacer cycles were matched to a breath, or the observed change was beyond a factor of 2.

## Results

1. **Entrainment.** Observed change tracks prescribed change with gain b = 0.898, 95% CI [0.883, 0.913], p = 2.2e-184 (1 = perfect following; between-person SD of the gain 0.08).
2. **Direction.** Acceleration trials sped breathing relative to deceleration trials: b = 0.434, 95% CI [0.424, 0.443], p = 0.0e+00 (log units of cycle duration).
   Salience: b = -0.002, 95% CI [-0.011, 0.008], p = 0.702; direction x salience b = 0.199, 95% CI [0.180, 0.218], p = 6.7e-89.
3. **Ramp vs step.** Gain differs by salience (lr_exp x sal): b = -0.014, 95% CI [-0.046, 0.018], p = 0.383.
4. **Detection follows the body.** Holding the prescribed change fixed, trials where the participant's breathing changed more in the prescribed direction were detected more often: combined task b = 1.814, 95% CI [1.389, 2.240], p = 6.4e-17 (OR per 0.1 log-unit = 1.20); baseline b = 2.586, 95% CI [1.877, 3.296], p = 8.8e-13 (OR per 0.1 log-unit = 1.30).
5. **Felt arousal and the body.** Felt arousal rose with the prescribed speed change (b = 0.463, 95% CI [0.343, 0.583], p = 4.7e-14); the participant's own speeding of breathing beyond it (within person): b = 0.041, 95% CI [-0.071, 0.153], p = 0.474.
6. **Depth.** Breath amplitude ratio (post/pre) by direction: b = -0.168, 95% CI [-0.184, -0.153], p = 8.0e-93.

Breathing change is measured from the cycle-3 onset to the cycle-4 inhale peak (the last paced event), against cycles 1-2; see `Methods/breath_trial_features.md`.

**Sensitivity: cycle 3 only against cycles 1-2** (the earlier measure):

- Entrainment gain b = 0.708, 95% CI [0.677, 0.739], p = 8.4e-118.
- Detection follows the body: combined b = 1.182, 95% CI [0.847, 1.517], p = 4.6e-12 (OR per 0.1 log-unit = 1.13); baseline b = 1.968, 95% CI [1.526, 2.411], p = 2.9e-18 (OR per 0.1 log-unit = 1.22).
- Own speeding of breathing -> felt arousal: b = 0.097, 95% CI [0.014, 0.181], p = 0.022.

Cell means (percent change in mean cycle length, cycle-3 onset to cycle-4 peak vs cycles 1-2):

       cond    n prescribed_pct observed_pct followed_dir
 accel high 1053          -27.9      -24.977      0.94397
  accel low 1149          -18.5      -17.145      0.90688
 decel high 1171           33.1       28.155      0.91033
  decel low 1220           18.8       16.182      0.83852
  no change 2416            0.0       -0.462      0.00414
