# Cardiac analysis

Run 2026-10-07. Script `Analysis/cardiac_analysis.R`; full output in `models.txt`. Exploratory; p values uncorrected.

## Sample

- 312 questionnaire-sample participants with ECG; 269 with a usable lead (score >= 0.6); 211 with usable resting HRV (<= 10% flagged RR); 224 with all three heartbeat-counting intervals valid.
- Pre-task baseline (the up-to-2-min window before the BCAT script; protocol rest, not verifiable from the triggers), medians: HR 88.6 bpm, RMSSD 30.7 ms, window 120 s; breathing 18.5 /min.

## Preregistered exploratory hypotheses

- **E1A** resting HRV and baseline emotion-recognition accuracy: lnRMSSD r = 0.020 [-0.115, 0.155], p = 0.769, n = 211; lnHF r = 0.034 [-0.102, 0.168], p = 0.628, n = 211.
- **E1B** lnRMSSD, combined task minus baseline GERT: mean difference -0.011 [-0.044, 0.021], t(232) = -0.68, p = 0.500. Note the combined task includes paced breathing at ~15/min, which itself shapes RSA.
- **E1C** (trial-type effect mediated by HRV change) is not estimable as specified: a 4-breath trial (~16 s) is too short for HRV. HR change per trial is used below instead.

## Breathing and the heart

- **HR response to the pacer.** HR change, 8-13 s minus 0-8 s after onset (the same latencies in every condition; cycle-defined windows confound condition with latency because HR falls through every trial and trial lengths differ), acceleration minus deceleration: b = -0.247 [-0.565, 0.071], p = 0.129 bpm; salience b = -0.163 [-0.481, 0.155], p = 0.314; interaction b = 0.043 [-0.594, 0.679], p = 0.896. No-change trials (mean change): b = -4.543 [-4.963, -4.123], p = 8.2e-56 bpm. Baseline task, acceleration minus deceleration: b = -0.160 [-0.454, 0.133], p = 0.284.
- **The body's own change.** HR change on the prescribed speed change (positive = faster): b = -0.929 [-2.004, 0.147], p = 0.091; on the participant's actual extra speeding, within person: b = 0.272 [-0.737, 1.282], p = 0.597. Negative b = faster breathing, smaller HR rise.
- **Felt arousal and the heart.** Felt arousal on the prescribed speed change: b = 0.369 [0.313, 0.425], p = 3.8e-38; on the trial's HR change, within person: b = 0.002 [-0.000, 0.005], p = 0.103.

## Burnout

- BAT and resting HR: r = 0.097 [-0.039, 0.229], p = 0.161, n = 211; resting lnRMSSD: r = -0.078 [-0.211, 0.058], p = 0.258, n = 211; HR response to acceleration: r = 0.107 [-0.019, 0.230], p = 0.095, n = 244.
- Felt arousal on BAT (standardised): -0.182, p = 0.004; with mean task HR and HR response added: -0.182, p = 0.004.

## Heartbeat counting

- Schandry accuracy median 0.70 (IQR 0.57-0.82).
- With BCAT threshold r = -0.082 [-0.211, 0.049], p = 0.221, n = 224; MAIA r = 0.099 [-0.033, 0.227], p = 0.140, n = 224; baseline GERT accuracy r = -0.049 [-0.179, 0.083], p = 0.466, n = 224; BAT r = -0.064 [-0.193, 0.068], p = 0.341, n = 224.

## Emotion clips (combined task)

- Rated intensity and HR during the clip, within person: b = -0.002 [-0.005, 0.001], p = 0.136; HR change from the 2 s before the clip: b = -0.002 [-0.006, 0.001], p = 0.187.
- HR during clips by the preceding block's breathing: direction b = -0.123 [-0.328, 0.082], p = 0.239; salience b = -0.118 [-0.322, 0.087], p = 0.260.
