# Handoff: Salience reanalysis of the breathing x emotion pilot

**Owner:** Norm Farb
**Needed by:** Friday, October 2, 2026 (feeds a grant LOI submitting Saturday)
**Repo:** `I:/Shared drives/Aya/Repo/` (Analysis/, Results/, Methods/)

## 1. Why this analysis

The pilot (N = 306; preregistration osf.io/wz32n) manipulated breathing-change **salience** within person: high-salience changes happened in one abrupt step, low-salience changes ramped gradually to the same total change. In the thesis path model, high salience raised detection accuracy (b = .18, p < .001), so salience is a working experimental manipulation of awareness.

The thesis's awareness finding (aware blocks gave higher perceived emotion intensity, b = .077, 95% CI [.014, .140], p = .016) classified blocks *post hoc* by detection accuracy (aware = 2 or 3 of 3 BCAT changes detected). That is correlational. The causally clean test, never reported, is:

1. Does **salience** (randomly assigned) raise **perceived emotion intensity** on the GERT trials that follow?
2. Does that effect run **through detection** (salience to detection to intensity)?

A yes to either lets the LOI describe experimental evidence that noticing a bodily change intensifies how others' emotions are perceived.

## 2. Ground rules

- **Plan first.** Before fitting anything, inspect the data and report back the items in Step 3. Stop and ask Norm if any column mapping is ambiguous.
- **Do not modify** existing scripts or outputs. Write a new script `Analysis/salience_reanalysis.R` and save outputs to `Results/salience_reanalysis/`.
- **Reuse** the existing loaders and exclusion logic in `Analysis/config.R`, `Analysis/functions.R`, and `Analysis/R/` where they exist, rather than reimplementing them.
- **R conventions:** start the script with the standard package block (below), build paths with `file.path()` (never `paste0()`), and call dplyr functions with the `dplyr::` prefix.

```r
# Set Up ---------
## Load libraries ---------
packages <- c("tidyverse", "lme4", "lmerTest", "MASS")
new_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
if (length(new_packages)) install.packages(new_packages)
options(readr.show_col_types = FALSE)
for (thispack in packages) {
  library(thispack, character.only = TRUE, quietly = TRUE, verbose = FALSE)
}
```

Note: `MASS` masks `dplyr::select`, which is one more reason to prefix every dplyr call.

## 3. Step 1: inspect and confirm (report before modeling)

Likely source: `Results/Combined_data.csv` (block or trial level), with `Combined_fullBCAT_data.csv` and `Combined_fullGERT_data.csv` as trial-level fallbacks. Confirm:

1. Which file(s) and columns give: participant ID, block number, salience, direction, BCAT detection correctness (per trial), BCAT arousal rating (per trial), GERT intensity rating (per clip), GERT accuracy (per clip), and clip ID.
2. Whether salience and direction are **constant within each block** (all 3 BCAT trials share one condition). If not, stop and report.
3. Exclusions matching the thesis: keep the first complete submission only (18 duplicates), drop failed attention checks (7), baseline BCAT technical failures (8), and anyone with fewer than 6 of 12 combined-task blocks (21). Report the resulting N participants, N blocks and N GERT trials.

## 4. Step 2: models

**Coding:** effect-code both factors so main effects average over the other factor.
`sal = +0.5` high salience, `-0.5` low. `dir = +0.5` acceleration, `-0.5` deceleration.
Detection at block level = proportion of the 3 BCAT changes correctly detected. For the mediation model, person-mean-center it (`det_pc`) and keep the person mean (`det_pm`).

**Random effects:** start maximal as listed. If a model fails to converge or is singular, simplify in this order: drop slope correlations, then drop the slope, then drop clip intercepts. Record what you used.

| Model | Question | Formula (lme4) |
|---|---|---|
| **M0** sanity check | Does the pipeline reproduce the thesis? | `intensity ~ aware * dir + (1 \| pid)` with aware = block detection >= 2/3. Expect aware b near .077. |
| **M1** manipulation check | Does salience raise detection? | `glmer(correct ~ sal * dir + (1 + sal \| pid), family = binomial)` on BCAT trials |
| **M2** primary | Does salience raise perceived intensity? | `intensity ~ sal * dir + (1 + sal \| pid) + (1 \| clip)` on GERT trials |
| **M3** | Does salience raise felt arousal? | `arousal ~ sal * dir + (1 + sal \| pid)` on BCAT trials |
| **M4a** mediation, a path | salience to detection | `det_block ~ sal * dir + (1 + sal \| pid)` at block level |
| **M4b** mediation, b path | detection to intensity, holding salience | `intensity ~ sal * dir + det_pc + det_pm + (1 \| pid) + (1 \| clip)` on GERT trials |
| **M5** secondary | Same as M2 for accuracy | `glmer(gert_correct ~ sal * dir + (1 + sal \| pid) + (1 \| clip), family = binomial)` |

**Indirect effect (M4):** a = `sal` from M4a, b = `det_pc` from M4b. Compute a x b with a Monte Carlo 95% CI: draw 20,000 values from normal distributions of a and b using their estimates and SEs (`set.seed(2026)`), multiply, and take the 2.5th and 97.5th percentiles. Also report the proportion of the M2 salience effect that is mediated (a x b divided by the M2 `sal` estimate), if the M2 estimate is positive.

**Primary test:** the M2 `sal` coefficient. Everything else is supporting. Report exact p values and do not correct within this set; we will describe it as a secondary analysis of the preregistered dataset.

## 5. Save to `Results/salience_reanalysis/`

- `salience_models.txt`: full `summary()` output for M0 to M5
- `salience_report.md`: the completed template in Section 6
- `fig_salience_intensity.png`: mean GERT intensity by salience x direction, with within-person 95% CIs

## 6. Report back in exactly this format

Copy the block below, fill in every field, and paste it back to Claude. Use `NA` and a one-line reason for anything you could not compute.

```
SALIENCE REANALYSIS REPORT
Date run:
Script: Analysis/salience_reanalysis.R (commit or file timestamp: )

DATA
Source file(s):
Salience/direction constant within block? (yes/no):
Exclusions applied (counts): duplicates= , attention= , BCAT technical= , <6 blocks=
Final N participants= , N blocks= , N GERT trials= , N BCAT trials=
Coding: sal (+0.5 high / -0.5 low); dir (+0.5 accel / -0.5 decel)

M0 SANITY (aware -> intensity; thesis b = .077)
aware: b= , SE= , 95% CI [ , ], p=
Matches thesis? (yes/no; if no, why):

M1 MANIPULATION CHECK (salience -> detection, logit)
sal: b= , SE= , 95% CI [ , ], p= , odds ratio=
Detection rate: high salience= %, low salience= %

M2 PRIMARY (salience -> perceived intensity, 7-pt scale)
sal: b= , SE= , df= , 95% CI [ , ], p=
dir: b= , p=
sal x dir: b= , p=
Cell means (intensity): high/accel= , high/decel= , low/accel= , low/decel=
Random effects used:

M3 (salience -> felt arousal)
sal: b= , SE= , 95% CI [ , ], p=
sal x dir: b= , p=

M4 MEDIATION (salience -> detection -> intensity)
a (sal -> det_block): b= , SE= , p=
b (det_pc -> intensity | sal): b= , SE= , p=
direct (sal in M4b): b= , p=
indirect a*b= , Monte Carlo 95% CI [ , ]
proportion mediated= (or NA if M2 sal <= 0)

M5 SECONDARY (salience -> recognition accuracy, logit)
sal: b= , SE= , p=

DEVIATIONS AND NOTES
Convergence/singularity fixes:
Anything that differed from this handoff:
Anything surprising:
```
