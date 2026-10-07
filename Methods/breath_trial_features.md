# Breathing features of paced BCAT trials

Written 2026-10-05. It covers `R/05c_breath_trials.R` and the pacer helpers in `R/physio_functions.R`.
Results are in `Analysis/breath_manipulation_check.R`, with output in `Results/breath_manipulation/`.

## The pacer

Taken from `Task/*_lastrun.py`, routine `trial`. A circle grows over the first half of each cycle
(inhale) and shrinks over the second half (exhale). A trial has 4 cycles, and each cycle starts at 4 s. With
`c = 1 + direction x level`, where direction is -1 for accelerate, +1 for decelerate and 0 for no change, and
level is the participant's threshold in the combined task or the QUEST level at baseline:

| Salience | Cycle durations (s) |
|---|---|
| High (every baseline trial; `changeSalience = 1`) | 4, 4, 4c, 4c (a step after cycle 2) |
| Low (`changeSalience = 0`) | 4, 4a, 4a^2, 4a^3 with a = c^(1/3) (the same total change as a ramp) |

Pacer onset is the trial's code-3 trigger, at millisecond precision. It is found by snapping PsychoPy
`trial.started` plus that script's clock offset to the nearest trigger within 150 ms.

## Observed breaths

Breaths come from respkit `resp_analyse()`, as trough -> peak -> trough cycles at 25 Hz. For each participant a
respiratory **lag** is estimated: the median delay from pacer-cycle onset to the nearest inhale onset,
searched within -1 to 2.5 s. Each pacer cycle is then matched to the breath that starts nearest
`onset + lag`, within 40% of the cycle duration, and each breath is used at most once.

## Per-trial features (`breath_trials.csv`)

| Column | Meaning |
|---|---|
| `obs_d1..obs_d3` | Observed cycle durations, trough to trough (s) |
| `obs_d4` | Cycle 4 timed peak to peak, breath 3 to breath 4 (s); prescribed value (d3 + d4) / 2 |
| `obs_d4_trough` | Cycle 4 trough to trough (s); reported only, see below |
| `exp_d1`, `exp_d4` | Pacer durations of cycles 1 and 4 (s) |
| `obs_ratio`, `exp_ratio` | **Breathing change, all four cycles:** mean cycle length from the cycle-3 onset to the cycle-4 peak (1.5 cycles) / mean cycle length from the cycle-1 onset to the cycle-3 onset (2 cycles), observed and prescribed. Both are 1 on a no-change trial. |
| `obs_ratio3`, `exp_ratio3` | Cycle 3 / mean(cycles 1-2): the earlier three-cycle measure, kept as a sensitivity check |
| `n_matched` | Pacer cycles matched to a breath (0-4) |
| `dur_mae` | Mean absolute error of cycles 1-3 against the pacer (s) |
| `pacer_r` | Correlation of the belt signal with the circle waveform, shifted by the lag |
| `amp_rel`, `amp_ratio` | Breath amplitude relative to the participant's median; cycle 3 / cycles 1-2 |
| `onset_source` | `trigger`, or `psychopy_clock` if no trigger lay within 150 ms |

**Cycle 4 is measured up to its inhale peak.** Every pacer event except the last is paced. The
closing trough of cycle 4 falls after the pacer stops: the exhale slows and tails off into the pause before
the rating screen, and the trough lands about 0.5 s late in every condition, including no-change trials
(median 4.5 s against a 4.00 s pacer; cycles 1-3 run 3.92-3.96 s). Up to the cycle-4 inhale peak the timing
is on the pacer: on no-change trials, onsets and peaks of all four cycles fall within 0.17 s of their paced
times. So the change measure runs from the cycle-3 onset to the cycle-4 peak, the last paced event. Study 5
used cycles 3-4 against 1-2, trough to trough; in this task that version made every trial look about 10%
slower, so decelerations looked perfectly followed and accelerations poorly followed.

Measures compared on 80 randomly chosen participants with sync >= .4. Statistics are medians over
participants of the within-person regression of log observed on log prescribed change, all paced trials:

| Measure | r | Gain | Residual SD | Gain / residual SD |
|---|---|---|---|---|
| Cycle-3 onset to cycle-4 peak (adopted) | .91 | 0.95 | .122 | 7.8 |
| Cycle 3 vs cycles 1-2 (earlier measure) | .87 | 0.90 | .129 | 6.9 |
| Peak to peak, cycles 3-4 vs 1-2 | .90 | 1.13 | .138 | 8.5 |
| Cycles 3-4, cycle 4 ended at 80% of its exhale | .83 | 0.86 | .152 | 5.6 |
| Cycles 3-4 vs 1-2, trough to trough (Study 5) | .71 | 0.73 | .193 | 3.8 |

The peak-to-peak measure has a gain above 1: peaks move more than whole cycles, so it overstates
following. An exhale-completion point, as a replacement for the late trough, did not rescue cycle 4. Referenced to
the closing trough it still ran 0.15-0.33 s long, and twice as variable as cycle 3. Referenced to the opening
trough, most final exhales never got back to that level.

## Which belt, and inclusion

**Belt-pacer synchrony** (`sync`) is the median of `pacer_r` over a participant's trials. It is a direct test that
a belt is on this participant's chest. A limp or unworn belt does not follow a 4 s pacer, and the
partner's belt follows a different trial sequence. Its distribution is bimodal, with belts near 0 or at
.6-.9, and inclusion for breathing analyses is `sync >= .4`.

The other seat's belt replaces a participant's own only if three conditions hold. No participant sat there,
or that participant's own belt has sync < .2. The other belt has sync >= .4. And its sync is at least .3
higher than the own belt's. This happened once (P756, a single-occupant session: own .01, other .74).
An earlier rule based on breath-duration changes was fooled by a limp belt pinned at its floor, and
it is not used.

**Polarity.** respkit's own polarity check is uninformative under a symmetric pacer, so the pacer
decides: a belt whose median `pacer_r` is below -.2 is re-analysed as inverted. No belt in this dataset needed this.

**respkit quality.** The default `resp_quality()` grades agree closely with synchrony. Of participants
with sync >= .4, 247 grade good or degraded and 2 unusable. Of those below .4, 52 of 62 grade unusable.
The grade is reported as a secondary flag (`quality_default`), and synchrony decides inclusion.
