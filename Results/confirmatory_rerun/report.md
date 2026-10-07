# Confirmatory hypotheses on the cleaned sample

Run 2026-10-07 with the exclusions in `R/06_exclusions.R` (flow below). Thesis values for comparison.

## Participant flow

| Step | Excluded | Remaining | Note |
|---|---|---|---|
| Participants with data (non-test IDs) | 0 | 329 |  |
| No complete questionnaire | 7 | 322 |  |
| Failed attention check (first submission) | 7 | 315 | 17 participants had repeat complete submissions; the first was kept |
|   of whom have a GERT baseline run | 0 | 315 | H1 sample (baseline GERT) |
| BCAT technical failure (no thresholds) | 10 | 305 | H2 sample (BCAT thresholds) |
| Fewer than 6 combined-task blocks | 19 | 286 | Combined-task sample (H3, H4) |

## Results

| Test | Estimate | 95% CI | p | p (BH, H1-H2) | n | Thesis |
|---|---|---|---|---|---|---|
| H1A | -0.010 | [-0.121, 0.100] | 8.55e-01 | 0.9320 | 315 | r = -.008, p = .885 |
| H1B | 0.044 | [-0.067, 0.154] | 4.36e-01 | 0.9320 | 315 | r = .043, p = .443 |
| H1E | -0.032 | [-0.142, 0.079] | 5.75e-01 | 0.9320 | 315 | r = -.032, p = .566 |
| H2A | 0.020 | [-0.093, 0.132] | 7.32e-01 | 0.9320 | 305 | r = .020, p = .730 |
| H2B | -0.174 | [-0.281, -0.063] | 2.27e-03 | 0.0136 | 305 | r = -.173, p = .002 |
| H2C | -0.005 | [-0.117, 0.107] | 9.32e-01 | 0.9320 | 305 | r = -.005, p = .930 |
| H3A Acc+Aware -> accuracy | -0.004 | [-0.020, 0.012] | 6.01e-01 |  | 286 | b = -.006, p = .475 |
| H3B Acc+Aware -> intensity | 0.042 | [0.002, 0.081] | 3.76e-02 |  | 286 | b = .033, p = .100 |
| H3E Acc+Aware -> distance | 0.004 | [-0.047, 0.055] | 8.73e-01 |  | 286 | b = .012, p = .637 |
| H3 expansion: aware -> intensity | 0.085 | [0.021, 0.148] | 9.58e-03 |  | 286 | b = .077, p = .016 |
| H4A dir x burnout -> accuracy | -0.001 | [-0.003, 0.000] | 1.29e-01 |  | 286 | b = -.00, p = .111 |
| H4B dir x burnout -> intensity | -0.000 | [-0.004, 0.003] | 8.18e-01 |  | 286 | b = -.00, p = .900 |
| Manipulation: salience (low vs high) -> detection, logit | -0.448 | [-0.572, -0.324] | 1.31e-12 |  | 286 | salience robust (path model b = .18, p < .001) |

## Multiplicity

The preregistration applies Benjamini-Hochberg across "all hypotheses (7 tests in total)". Read as H1A, H1B, H2A, H2B, H2C, H3A and H3B (H1E and H3E use the exploratory distance outcome; H4 is a moderation test), the BH-adjusted p values are:

| Test | p | p (BH, 7 tests) |
|---|---|---|
| H1A | 0.85500 | 0.9320 |
| H1B | 0.43600 | 0.9320 |
| H2A | 0.73200 | 0.9320 |
| H2B | 0.00227 | 0.0159 |
| H2C | 0.93200 | 0.9320 |
| H3A Acc+Aware -> accuracy | 0.60100 | 0.9320 |
| H3B Acc+Aware -> intensity | 0.03760 | 0.1320 |
