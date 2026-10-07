# H2B robustness: breath-change threshold and emotion recognition

Run 2026-10-07. Script `Analysis/h2b_robustness.R`; full output in `models.txt`. H2B is preregistered; these checks are exploratory, p values uncorrected.

Higher threshold = worse breath-change sensitivity. Standardised betas for the threshold, with 95% CIs.

| Check | Threshold effect on GERT accuracy |
|---|---|
| H2B as preregistered (baseline GERT) | r = -0.174 [-0.281, -0.063], p = 0.002, n = 305 |
| **Replication: combined-task GERT, 60 different clips** | r = -0.176 [-0.285, -0.064], p = 0.002, n = 296 |
| Accelerate threshold only | r = -0.123 [-0.232, -0.011], p = 0.032, n = 305 |
| Decelerate threshold only | r = -0.157 [-0.264, -0.045], p = 0.006, n = 305 |
| Rank correlation | rho = -0.198, p = 5.1e-04, n = 305 |
| Without the 6 ceiling thresholds (1.0) | r = -0.163 [-0.271, -0.050], p = 0.005, n = 299 |
| Robust regression (Huber) | beta = -0.184, t = -3.29 |
| Without 10 influential points (Cook's D > 4/n) | beta = -0.213 [-0.325, -0.100], p = 2.3e-04, n = 295 |
| + response bias (false alarms, 'no change' rate) | beta = -0.145 [-0.268, -0.022], p = 0.021, n = 301 |
| + bias + confidence | beta = -0.141 [-0.264, -0.018], p = 0.024, n = 301 |
| + bias, confidence, MAIA, age, gender, PHQ-4, burnout | beta = -0.138 [-0.262, -0.015], p = 0.028, n = 301 |
| Belt follows pacer: unadjusted | beta = -0.133 [-0.260, -0.007], p = 0.039, n = 240 |
| Belt follows pacer: + synchrony, entrainment gain, false alarms | beta = -0.095 [-0.234, 0.044], p = 0.178, n = 236 |

Measurement checks:

- Threshold reliability: accelerate vs decelerate threshold r = 0.323; Spearman-Brown reliability of their mean = 0.488.
- H2B corrected for threshold unreliability (GERT reliability not corrected): r = -0.249.
- Calibration: threshold vs combined-task detection rate at that threshold: r = 0.029 [-0.087, 0.144], p = 0.627, n = 286.
- False-alarm rate vs threshold: r = 0.249 [0.140, 0.353], p = 1.2e-05, n = 301; vs GERT accuracy: r = -0.091 [-0.202, 0.022], p = 0.115, n = 301.
- MAIA vs threshold: r = 0.043 [-0.070, 0.154], p = 0.459, n = 305.
