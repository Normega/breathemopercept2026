```
SALIENCE REANALYSIS REPORT
Date run: 2026-10-01
Script: Analysis/salience_reanalysis.R (commit or file timestamp: 2026-10-01 21:42)

DATA
Source file(s): Results/Combined_fullBCAT_data.csv, Results/Combined_fullGERT_data.csv (trial level); Results/Combined_data.csv (M0 only); Data/QualtricsMar5.xlsx (attention checks)
Salience/direction constant within block? (yes/no): yes
Exclusions applied (counts): duplicates= 17 Qualtrics re-submissions (first complete kept; task duplicates resolved upstream in 04), attention= 6, BCAT technical= 0 (not applied, per Norm), <6 blocks= 0
Final N participants= 287, N blocks= 3302, N GERT trials= 16474, N BCAT trials= 9906
Coding: sal (+0.5 high / -0.5 low); dir (+0.5 accel / -0.5 decel)

M0 SANITY (aware -> intensity; thesis b = .077)
aware: b= 0.082, SE= 0.033, 95% CI [0.017, 0.146], p= 0.013  (thesis spec, Acc reference)
       effect-coded dir: b= 0.050, SE= 0.024, 95% CI [0.003, 0.097], p= 0.035
Matches thesis? (yes/no; if no, why): yes

M1 MANIPULATION CHECK (salience -> detection, logit)
sal: b= 0.571, SE= 0.056, 95% CI [0.462, 0.680], p= 8.63e-25, odds ratio= 1.771
Detection rate: high salience= 71.5%, low salience= 60.3%  (change trials only)

M2 PRIMARY (salience -> perceived intensity, 7-pt scale)
sal: b= -0.001, SE= 0.017, df= 285.3, 95% CI [-0.035, 0.032], p= 0.935
dir: b= 0.006, p= 0.692
sal x dir: b= 0.003, p= 0.929
Cell means (intensity): high/accel= 5.045, high/decel= 5.037, low/accel= 5.044, low/decel= 5.039
Random effects used: (1 + sal | id) + (1 | clip)

M3 (salience -> felt arousal)
sal: b= 0.003, SE= 0.014, 95% CI [-0.024, 0.030], p= 0.804
sal x dir: b= 0.070, p= 0.011

M4 MEDIATION (salience -> detection -> intensity)
a (sal -> det_block): b= 0.113, SE= 0.011, p= 5.29e-24
b (det_pc -> intensity | sal): b= 0.051, SE= 0.026, p= 0.051
direct (sal in M4b): b= -0.007, p= 0.668
indirect a*b= 0.00574, Monte Carlo 95% CI [0.00003, 0.01178]
proportion mediated= NA (M2 sal <= 0)

M5 SECONDARY (salience -> recognition accuracy, logit)
sal: b= -0.004, SE= 0.034, p= 0.916

DEVIATIONS AND NOTES
Convergence/singularity fixes: M1 (1 | id); M2 (1 + sal | id) + (1 | clip); M3 (1 | id); M4a (1 | id); M4b (1 | id) + (1 | clip); M5 (1 | id) + (1 | clip) (full attempt log in salience_models.txt)
Anything that differed from this handoff:
  - Blocks contain 2 change trials + 1 no-change catch trial, not 3 changes. det_block (M1, M4) = proportion of the 2 change trials detected; M0 keeps the thesis definition (all 3 trials, aware = >= 2/3).
  - Per Norm, exclusions limited to attention-check failures (first complete Qualtrics submission) plus the existing <6-block rule; BCAT baseline technical failures retained.
  - M3 fit at trial level as specified (arousal is rated after each BCAT trial); block-averaged arousal used in exploratory S2.
  - Sensitivity S1 (detection = all 3 trials): a = 0.089 (p = 1.56e-22), indirect = 0.00601 [0.00047, 0.01190].
  - Exploratory S2 (block-mean arousal as mediator): a = 0.004 (p = 0.836), b = 0.053 (p = 7.56e-04), indirect = 0.00021 [-0.00192, 0.00249].
Anything surprising:
  - Salience is a strong manipulation of detection (M1, M4a) yet has no total effect on perceived intensity (M2), with a tight CI around zero.
  - The within-person detection -> intensity association (M4b) is small and borderline; the indirect path is tiny relative to the M2 CI width.
  - Block-mean arousal predicts GERT intensity (S2b) but salience does not move arousal (M3, S2a); M3 shows a salience x direction interaction.
  - One combined-task file stores an e-mail address in place of the participant number (pseudonymised here; upstream CSVs still contain it).
  - 04_prep_combined.R builds file paths with paste0(taskDataPath, file) and taskDataPath has no trailing separator, so the current script cannot regenerate the Combined_*.csv files; this analysis uses the existing CSVs.
```
