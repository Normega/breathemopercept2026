# Random-slope models (primary) and random-intercept models (sensitivity)

Run 2026-10-07. Script `Analysis/random_slopes.R`.
Each row is one fixed effect. `b, 95% CI, p` come from the model with by-participant random slopes for the listed predictors; `b (int.)` and `p (int.)` from the same model with random intercepts only. Clip-level models also have random intercepts for clip. `re` says whether the slope model kept correlated random effects or needed uncorrelated ones (||).

| Model | Term | Slopes | re | b [95% CI] | p | b (int.) | p (int.) |
|---|---|---|---|---|---|---|---|
| R1 felt arousal ~ direction x salience | dir_e | dir_e + sal_e | uncorrelated, singular | 0.248 [0.196, 0.300] | < .001 | 0.247 | < .001 |
| R1 felt arousal ~ direction x salience | sal_e | dir_e + sal_e | uncorrelated, singular | -0.001 [-0.033, 0.032] | .976 | -0.001 | .939 |
| R1 felt arousal ~ direction x salience | dir_e:sal_e | dir_e + sal_e | uncorrelated, singular | 0.063 [-0.003, 0.129] | .060 | 0.062 | .072 |
| R2A detection ~ salience x direction (logit) | sal_e | sal_e + dir_e | uncorrelated | 0.633 [0.519, 0.746] | < .001 | 0.582 | < .001 |
| R2A detection ~ salience x direction (logit) | dir_e | sal_e + dir_e | uncorrelated | 0.139 [-0.046, 0.324] | .142 | 0.137 | .013 |
| R2A detection ~ salience x direction (logit) | sal_e:dir_e | sal_e + dir_e | uncorrelated | -0.004 [-0.227, 0.219] | .971 | 0.005 | .960 |
| R2B felt arousal ~ direction x detection | dir_e:acc_pc | dir_e + acc_pc | correlated | 0.206 [0.127, 0.285] | < .001 | 0.208 | < .001 |
| R2B felt arousal ~ direction x detection | acc_pc | dir_e + acc_pc | correlated | 0.058 [0.014, 0.103] | .011 | 0.057 | .004 |
| H3A block accuracy ~ accelerate-and-aware | accel_aware | accel_aware | correlated | -0.004 [-0.021, 0.012] | .604 | -0.004 | .601 |
| H3B block intensity ~ accelerate-and-aware | accel_aware | accel_aware | uncorrelated, singular | 0.042 [0.002, 0.081] | .038 | 0.042 | .038 |
| H3B clip intensity ~ accelerate-and-aware | accel_aware | accel_aware | correlated | 0.035 [0.001, 0.069] | .045 | 0.034 | .044 |
| Awareness x direction, clip intensity (effect coded) | aware_e | aware_e + dir_e | uncorrelated | 0.045 [0.005, 0.084] | .027 | 0.045 | .025 |
| Awareness x direction, clip intensity (effect coded) | dir_e | aware_e + dir_e | uncorrelated | -0.008 [-0.045, 0.028] | .650 | -0.008 | .644 |
| Awareness x direction, clip intensity (effect coded) | aware_e:dir_e | aware_e + dir_e | uncorrelated | 0.064 [-0.012, 0.140] | .100 | 0.064 | .099 |
| E2C block intensity ~ felt arousal | ar_pc | ar_pc | correlated | 0.063 [0.017, 0.109] | .007 | 0.077 | < .001 |
| E2C block intensity ~ felt arousal | ar_pm | ar_pc | correlated | 0.080 [0.011, 0.149] | .023 | 0.080 | .022 |
| E2C clip intensity ~ felt arousal | ar_pc | ar_pc | correlated | 0.033 [-0.013, 0.079] | .156 | 0.054 | < .001 |
| E2C clip intensity ~ felt arousal | ar_pm | ar_pc | correlated | 0.079 [0.011, 0.148] | .024 | 0.080 | .022 |
| E2C block accuracy ~ felt arousal | ar_pc | ar_pc | correlated | 0.014 [-0.001, 0.029] | .072 | 0.013 | .069 |
| Clip intensity ~ salience x direction | sal_e | sal_e + dir_e | correlated | 0.000 [-0.034, 0.034] | .991 | 0.000 | .976 |
| Clip intensity ~ salience x direction | dir_e | sal_e + dir_e | correlated | 0.007 [-0.025, 0.038] | .683 | 0.006 | .686 |
| Clip intensity ~ salience x direction | sal_e:dir_e | sal_e + dir_e | correlated | -0.013 [-0.076, 0.049] | .673 | -0.013 | .677 |
| Clip accuracy ~ salience x direction (linear probability) | sal_e | sal_e + dir_e | uncorrelated, singular | -0.001 [-0.015, 0.013] | .875 | -0.001 | .875 |
| Clip accuracy ~ salience x direction (linear probability) | dir_e | sal_e + dir_e | uncorrelated, singular | -0.011 [-0.025, 0.003] | .118 | -0.011 | .118 |
| Breathing change ~ direction x salience | dir_e | dir_e + sal_e | correlated | 0.445 [0.422, 0.469] | < .001 | 0.434 | < .001 |
| Breathing change ~ direction x salience | sal_e | dir_e + sal_e | correlated | -0.000 [-0.010, 0.009] | .923 | -0.002 | .702 |
| Breathing change ~ direction x salience | dir_e:sal_e | dir_e + sal_e | correlated | 0.210 [0.194, 0.227] | < .001 | 0.199 | < .001 |
| Ramp vs step: observed ~ prescribed x salience | lr_exp | lr_exp | correlated | 0.873 [0.851, 0.896] | < .001 | 0.869 | < .001 |
| Ramp vs step: observed ~ prescribed x salience | lr_exp:sal_e | lr_exp | correlated | -0.020 [-0.052, 0.012] | .226 | -0.014 | .383 |
| Detection ~ breathing change made (combined, logit) | aligned_pc | aligned_pc | correlated | 2.025 [1.583, 2.466] | < .001 | 1.814 | < .001 |
| Detection ~ breathing change made (baseline, logit) | aligned_pc | aligned_pc | uncorrelated, singular | 2.860 [2.075, 3.645] | < .001 | 2.586 | < .001 |
| Felt arousal ~ prescribed and own speeding | spd_exp | spd_exp + spd_obs_pc | correlated | 0.475 [0.322, 0.629] | < .001 | 0.463 | < .001 |
| Felt arousal ~ prescribed and own speeding | spd_obs_pc | spd_exp + spd_obs_pc | correlated | 0.026 [-0.099, 0.150] | .685 | 0.041 | .474 |
| Intensity ~ awareness | aware_pc | aware_pc | correlated | 0.049 [-0.020, 0.117] | .161 | 0.055 | .067 |
| Intensity ~ breathing change made | body_pc | body_pc | correlated | 0.105 [-0.061, 0.271] | .214 | 0.115 | .112 |
| Intensity ~ awareness + breathing change | aware_pc | aware_pc + body_pc | correlated | 0.043 [-0.026, 0.112] | .219 | 0.049 | .114 |
| Intensity ~ awareness + breathing change | body_pc | aware_pc + body_pc | correlated | 0.084 [-0.084, 0.253] | .323 | 0.095 | .198 |
| Intensity ~ awareness + breathing change + felt arousal | aware_pc | aware_pc + body_pc + arousal_pc | correlated | 0.038 [-0.027, 0.104] | .252 | 0.044 | .151 |
| Intensity ~ awareness + breathing change + felt arousal | body_pc | aware_pc + body_pc + arousal_pc | correlated | 0.128 [-0.033, 0.290] | .119 | 0.092 | .213 |
| Intensity ~ awareness + breathing change + felt arousal | arousal_pc | aware_pc + body_pc + arousal_pc | correlated | 0.049 [-0.004, 0.101] | .070 | 0.072 | < .001 |
| Intensity ~ awareness + breathing change + felt arousal | arousal_pm | aware_pc + body_pc + arousal_pc | correlated | 0.114 [0.039, 0.190] | .003 | 0.116 | .003 |
| Chain: awareness ~ breathing change | body_pc | body_pc | correlated | 0.445 [0.336, 0.555] | < .001 | 0.431 | < .001 |
| Chain: felt arousal ~ awareness + breathing change | aware_pc | aware_pc + body_pc | correlated | 0.099 [0.022, 0.176] | .012 | 0.099 | .007 |
| Chain: felt arousal ~ awareness + breathing change | body_pc | aware_pc + body_pc | correlated | 0.065 [-0.124, 0.255] | .498 | 0.066 | .453 |
| Trial HR change ~ direction x salience | dir_e | dir_e + sal_e | uncorrelated | -0.251 [-0.578, 0.076] | .132 | -0.247 | .129 |
| Trial HR change ~ direction x salience | sal_e | dir_e + sal_e | uncorrelated | -0.166 [-0.491, 0.159] | .316 | -0.163 | .314 |
| Felt arousal ~ trial HR change | d_hr_pc | spd_exp + d_hr_pc | correlated | 0.002 [-0.001, 0.006] | .205 | 0.002 | .103 |
| Clip intensity ~ clip HR | hr_clip_pc | hr_clip_pc | correlated | -0.002 [-0.005, 0.001] | .129 | -0.002 | .136 |

Serial indirect effect, breathing change -> awareness -> felt arousal -> intensity, all paths with slopes: 0.0022, Monte Carlo 95% CI [-0.0002, 0.0058].
Within-person SD of breathing change (blocks): 0.140 log-units.

## Equivalence tests on the slope models

| Effect | Estimate | 90% CI | SESOI | Equivalent |
|---|---|---|---|---|
| Salience (high - low) -> clip intensity | 0.000 | [-0.028, 0.029] | +-0.05 scale points | yes |
| Direction (accelerate - decelerate) -> clip intensity | 0.007 | [-0.020, 0.033] | +-0.05 scale points | yes |
| Salience x direction -> clip intensity | -0.013 | [-0.066, 0.039] | +-0.05 scale points | no |
| Breathing change made (per within-person SD) -> clip intensity | 0.012 | [-0.008, 0.032] | +-0.05 scale points | yes |
| Salience (high - low) -> recognition accuracy | -0.001 | [-0.013, 0.011] | +-0.02 proportion | yes |
| Direction (accelerate - decelerate) -> recognition accuracy | -0.011 | [-0.023, 0.001] | +-0.02 proportion | no |
| Direction (accelerate - decelerate) -> HR change | -0.251 | [-0.525, 0.023] | +-1 bpm | yes |
| Salience (high - low) -> HR change | -0.166 | [-0.439, 0.107] | +-1 bpm | yes |
