# Body vs awareness: what drives perceived intensity?

Run 2026-10-07. Script `Analysis/body_vs_awareness.R`; full output in `models.txt`. Exploratory; p values uncorrected.

224 participants (combined-task sample, belt follows pacer), 2431 blocks, 12046 clips. Clip-level models with random intercepts for participant and clip; predictors split within / between person; all control direction x salience.

Predictors (within person): **awareness** = share of the block's 2 changes detected; **body** = log change in breath-cycle duration actually made in the prescribed direction (positive = followed the pacer); **arousal** = felt arousal.

| Model | Awareness | Body | Felt arousal |
|---|---|---|---|
| Awareness alone | b = 0.055, 95% CI [-0.004, 0.115], p = 0.067 | | |
| Body alone | | b = 0.115, 95% CI [-0.027, 0.258], p = 0.112 | |
| Both | b = 0.049, 95% CI [-0.012, 0.109], p = 0.114 | b = 0.095, 95% CI [-0.050, 0.239], p = 0.198 | |
| Both + felt arousal | b = 0.044, 95% CI [-0.016, 0.104], p = 0.151 | b = 0.092, 95% CI [-0.053, 0.236], p = 0.213 | b = 0.072, 95% CI [0.035, 0.110], p = 1.4e-04 |

- Awareness x body interaction: b = 0.327, 95% CI [-0.126, 0.781], p = 0.157.
- Thesis awareness definition (>= 2/3 of all 3 trials), with body: b = 0.035, 95% CI [-0.011, 0.082], p = 0.135.
- Recognition accuracy (logit) on awareness, with body: b = 0.046, 95% CI [-0.086, 0.178], p = 0.493.

Paths (block level, within person):

- Body -> awareness: b = 0.431, 95% CI [0.333, 0.530], p = 2.1e-17 (detection rate per log-unit).
- Body -> felt arousal, holding awareness: b = 0.066, 95% CI [-0.107, 0.239], p = 0.453; awareness -> felt arousal: b = 0.099, 95% CI [0.027, 0.171], p = 0.007.
- Indirect body -> awareness -> intensity: 0.0210 [-0.0046, 0.0484]; body -> felt arousal -> intensity: 0.0048 [-0.0082, 0.0188]; serial body -> awareness -> felt arousal -> intensity: 0.0031 [0.0007, 0.0065] (Monte Carlo 95% CI).

Random-slope sensitivity (each path with a by-participant slope for its predictor):

- Body -> awareness: b = 0.445, 95% CI [0.337, 0.554], p = 9.1e-14; awareness -> felt arousal: b = 0.101, 95% CI [0.025, 0.176], p = 0.010; felt arousal -> intensity: b = 0.052, 95% CI [-0.000, 0.105], p = 0.053.
- Serial body -> awareness -> felt arousal -> intensity with slopes: 0.0023 [-0.0000, 0.0061].

Scale: the within-person SD of the body measure is 0.140 log-units (about 15% in cycle duration). Per SD, the body effect on intensity in the 'Both' model is 0.0133 [-0.0070, 0.0336] scale points.
