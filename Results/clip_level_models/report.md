# Clip-level versions of H3 and E2C

Run 2026-10-07. Script `Analysis/clip_level_models.R`; full output in `models.txt`. 286 participants, 3248 blocks, 16200 clips (combined-task sample).

Clip-level models add a random intercept for clip (60 clips, each seen once per person) and attempt a by-participant slope for the block predictor (kept unless singular).

| Model | Level | Term | b | 95% CI | p | Random effects | N |
|---|---|---|---|---|---|---|---|
| H3 emoAccuracy | block means | AccelAwareTRUE | -0.004 | [-0.020, 0.012] | 0.601 | (1 | id) | 3248 |
| H3 emoIntensity | block means | AccelAwareTRUE | 0.042 | [0.002, 0.081] | 0.038 | (1 | id) | 3248 |
| H3 emoDistance | block means | AccelAwareTRUE | 0.004 | [-0.047, 0.055] | 0.873 | (1 | id) | 3248 |
| H3 accuracy (logit) | clip | AccelAwareTRUE | -0.054 | [-0.127, 0.019] | 0.150 | intercepts only (slope singular or not converged) | 16200 |
| H3 IntensityRating | clip | AccelAwareTRUE | 0.035 | [0.001, 0.069] | 0.045 | with by-participant slope | 16200 |
| H3 emo_distance | clip | AccelAwareTRUE | 0.020 | [-0.026, 0.066] | 0.385 | intercepts only (slope singular or not converged) | 16200 |
| H3 expansion, intensity | block means | aw_e | 0.051 | [0.005, 0.097] | 0.031 | (1 | id) | 3248 |
| H3 expansion, intensity | block means | dir_e | -0.004 | [-0.047, 0.038] | 0.845 | (1 | id) | 3248 |
| H3 expansion, intensity | block means | dir_e:aw_e | 0.068 | [-0.022, 0.157] | 0.137 | (1 | id) | 3248 |
| H3 expansion, intensity | clip | aw_e | 0.045 | [0.005, 0.084] | 0.029 | with by-participant slope | 16200 |
| H3 expansion, intensity | clip | dir_e | -0.009 | [-0.045, 0.027] | 0.621 | with by-participant slope | 16200 |
| H3 expansion, intensity | clip | dir_e:aw_e | 0.065 | [-0.011, 0.141] | 0.093 | with by-participant slope | 16200 |
| E2C intensity (thesis model) | block means | Arousal | 0.078 | [0.047, 0.108] | 7.0e-07 | (1 | id) | 3248 |
| E2C intensity | block means | ar_pc | 0.077 | [0.043, 0.112] | 1.2e-05 | (1 | id) | 3248 |
| E2C intensity | block means | ar_pm | 0.080 | [0.012, 0.149] | 0.022 | (1 | id) | 3248 |
| E2C intensity | clip | ar_pc | 0.033 | [-0.012, 0.079] | 0.156 | with by-participant slope | 16200 |
| E2C intensity | clip | ar_pm | 0.079 | [0.011, 0.148] | 0.024 | with by-participant slope | 16200 |
| E2C accuracy (logit) | clip | ar_pc | 0.031 | [-0.040, 0.103] | 0.388 | with by-participant slope | 16200 |

Preregistered BH family (7 tests) with H3A and H3B at clip level:

| Test | p | p (BH) |
|---|---|---|
| H1A | 0.855 | 0.932 |
| H1B | 0.436 | 0.763 |
| H2A | 0.732 | 0.932 |
| H2B | 0.002 | 0.016 |
| H2C | 0.932 | 0.932 |
| H3A (clip) | 0.150 | 0.351 |
| H3B (clip) | 0.045 | 0.159 |
