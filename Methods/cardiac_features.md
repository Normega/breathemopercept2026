# Cardiac features

Written 2026-10-05. It covers `R/05d_cardiac.R` and the cardiac helpers in `R/physio_functions.R`.
Results are in `Analysis/cardiac_analysis.R`, with output in `Results/cardiac_analysis/`.

## Lead

The ECG is 3-lead, recorded at 2000 Hz and decimated to 250 Hz with an anti-alias filter (05a). Each participant's
lead is chosen in 05b. The own seat's lead is used unless the other seat was unoccupied and its R-peak score
is >= .6 and at least .3 higher. Six single-occupant participants wore the other seat's lead. The score is the
regularity of detected beats times a morphology test: R waves stand at least 6x above the band-passed baseline.
That test rejects unworn leads picking up noise or mains oscillation. It was validated by eye on worn and
unworn leads.

## R peaks (`detect_r_peaks`)

1. Band-pass 5-20 Hz, differentiate, square, and integrate over 150 ms with a centred window, so the
   integrated peak is not delayed.
2. Candidates are local maxima above 30% of a running reference: the median of 2-s block maxima over about 20 s.
   This lets the threshold follow slow amplitude changes.
3. Refractory period 0.3 s (200 bpm): of two candidates closer than that, the larger is kept.
4. Each beat is refined to the R wave on the 5-30 Hz band-passed ECG. It is located in the lead's dominant
   polarity within +-75 ms, with parabolic interpolation, so timing is not limited to the 4 ms sample grid.

The method is Pan-Tompkins-like, as in Study 5, plus refinement and amplitude bookkeeping. Detection was checked
by eye on 5 participants, including a noisy lead (P857) and one with tall T waves (P943). Every beat sat on the R wave.

## RR cleaning (`clean_rr`)

An interval is flagged if any of the following holds:
- either bounding beat is under 30% of the recording's median R height (lead off or noise);
- it lies outside 0.3-2.0 s;
- it departs by more than 20% from the median of the surrounding 11 intervals.

The amplitude rule matters. A detached lead still yields "beats" in its noise, and some pass the RR rules by chance.
In one participant (P222) the lead came off at minute 46, and the amplitude rule flags all of the remainder.
HRV windows report `pct_artifact`, and analyses use windows with <= 10% flagged.

## Measures

| Measure | Window | Notes |
|---|---|---|
| Pre-task HR/HRV (`rest_*`) | Up to 120 s ending 5 s before the BCAT script starts | The protocol's 2-minute rest. Triggers cannot confirm the participant was resting. The BCAT script starts 70-250 s after recording begins (deciles). |
| `rmssd_ms`, `sdnn_ms` | Clean intervals; successive differences only where both are clean | |
| `hf_lnms2` | ln power, 0.15-0.40 Hz, RR resampled at 4 Hz, Welch with 60-s Hann segments | Windows >= 60 s |
| Heartbeat counting | The 3 count windows (code 7 to code 8, i.e. `countScreen` start/stop), each snapped to its trigger | Actual beats are those with R height >= 30% of median. Schandry accuracy = 1 - abs(actual - reported) / actual, floored at 0. Valid if <= 10% flagged. Durations are 25, 35 and 55 s, in randomised order. |
| Task HRV | GERT baseline: first clip to last clip. Combined task: first BCAT trial to last clip | The GERT baseline clock offset comes from matching clip triggers (code 11) to `testVideo.started`. |
| BCAT trial HR | `hr_0_8`, `hr_8_13` (s after pacer onset); `hr_trial` | See below |
| GERT clip HR | `hr_clip` (`testVideo.started` to `showVideo.stopped`); `hr_pre` (2 s before) | `testVideo.stopped` is missing in many files |

### Why trial HR uses fixed latency bins

HR falls by about 4 bpm across every paced trial, including no-change trials. This is typical attentional
slowing of the heart. Trials also differ in length: accelerated trials last about 13.7 s and decelerated about 18.7 s. Windows defined by
pacer cycles (cycles 1-2 vs 3-4) therefore compare different latencies after onset, and they produced
opposite-signed artefacts. Accelerated trials showed a larger HR drop on the cycle-based change score
and a higher whole-trial mean, both because of where their windows fall on the declining curve.
Fixed bins compare like with like. The 0-8 s bin is cycles 1-2, identical in every condition, and the 8-13 s bin
lies inside every trial after the change. On those bins the HR change does not differ between acceleration,
deceleration and no change (all p > .2).
