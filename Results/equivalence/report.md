# Equivalence tests for the key null results

Run 2026-10-07. Script `Analysis/equivalence_tests.R`. Two one-sided tests (TOST), alpha = .05: an effect is *equivalent to zero* when its 90% CI lies inside +-SESOI. Exploratory.

SESOI: correlations |r| = .17 (the size of H2B, the association that replicated); clip intensity 0.05 scale points (about the awareness-intensity association; 0.10 also shown); recognition accuracy 2 percentage points; heart rate 1 bpm. *Smallest supported bound* = the narrowest symmetric bound the 90% CI fits inside.

| Test | Estimate | 90% CI | SESOI | p (TOST) | Equivalent | Smallest supported bound |
|---|---|---|---|---|---|---|
| Salience (high - low) -> clip intensity | 0.000 | [-0.026, 0.027] | +-0.05 scale points | < .001 | yes | 0.027 |
| Direction (accelerate - decelerate) -> clip intensity | 0.006 | [-0.020, 0.033] | +-0.05 scale points | 0.003 | yes | 0.033 |
| Salience x direction -> clip intensity | -0.013 | [-0.066, 0.039] | +-0.05 scale points | 0.125 | no | 0.066 |
| Salience (high - low) -> clip intensity | 0.000 | [-0.026, 0.027] | +-0.10 scale points | < .001 | yes | 0.027 |
| Direction (accelerate - decelerate) -> clip intensity | 0.006 | [-0.020, 0.033] | +-0.10 scale points | < .001 | yes | 0.033 |
| Salience x direction -> clip intensity | -0.013 | [-0.066, 0.039] | +-0.10 scale points | 0.003 | yes | 0.066 |
| Salience (high - low) -> recognition accuracy | -0.001 | [-0.013, 0.011] | +-0.02 proportion | 0.004 | yes | 0.013 |
| Direction (accelerate - decelerate) -> recognition accuracy | -0.011 | [-0.023, 0.001] | +-0.02 proportion | 0.104 | no | 0.023 |
| Breathing change made (per within-person SD) -> clip intensity | 0.013 | [-0.004, 0.030] | +-0.05 scale points | < .001 | yes | 0.030 |
| Breathing change made (per within-person SD) -> clip intensity | 0.013 | [-0.004, 0.030] | +-0.10 scale points | < .001 | yes | 0.030 |
| Direction (accelerate - decelerate) -> HR change | -0.247 | [-0.514, 0.020] | +-1.00 bpm | < .001 | yes | 0.514 |
| Salience (high - low) -> HR change | -0.163 | [-0.430, 0.103] | +-1.00 bpm | < .001 | yes | 0.430 |
| H1A burnout - baseline recognition accuracy (n = 315) | -0.010 | [-0.103, 0.083] | +-0.17 r | 0.002 | yes | 0.103 |
| H1B burnout - baseline perceived intensity (n = 315) | 0.044 | [-0.049, 0.136] | +-0.17 r | 0.012 | yes | 0.136 |
| H2A burnout - BCAT threshold (n = 305) | 0.020 | [-0.075, 0.114] | +-0.17 r | 0.004 | yes | 0.114 |
| H2C BCAT threshold - baseline perceived intensity (n = 305) | -0.005 | [-0.099, 0.090] | +-0.17 r | 0.002 | yes | 0.099 |
| E1A resting lnRMSSD - baseline recognition accuracy (n = 211) | 0.020 | [-0.093, 0.134] | +-0.17 r | 0.015 | yes | 0.134 |
| Burnout - resting heart rate (n = 211) | 0.097 | [-0.017, 0.208] | +-0.17 r | 0.141 | no | 0.208 |
| Burnout - resting lnRMSSD (n = 211) | -0.078 | [-0.190, 0.036] | +-0.17 r | 0.089 | no | 0.190 |
