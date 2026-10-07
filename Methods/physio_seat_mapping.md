# Physio seat mapping and participant assignment

Written 2026-10-05. It covers `R/physio_functions.R`, `R/05a_extract_physio.R` and
`R/05b_assign_physio.R`, which replace `R/retired/05_prep_physio.R` and `R/retired/qc_physio_rds.R`.

## The setup

Two participants ran at the same time on the Left and Right computers. Both were recorded by one
BIOPAC MP160 at 2000 Hz. Each `.acq` holds two respiration belts (`Breath 1`, `Breath 2`),
two ECG channels (`Heart 1`, `Heart 2`), and one 8-bit trigger channel that both computers'
parallel ports write to.

| Seat | PsychoPy port (`*_Left/Right_lastrun.py`) | Codes sent | Trigger bits | Breath | Heart |
|---|---|---|---|---|---|
| L | 0xCFF8 | 16, 32, 48, 64, 112, 128, 144-192 | high nibble (4-7) | Breath 2 | Heart 2 |
| R | 0xD030 | 1-4, 7-12 | low nibble (0-3) | Breath 1 | Heart 1 |

## What was wrong before

The retired `05_prep_physio.R` gave the L seat the low nibble, `Breath 1` and `Heart 1`. That is the
reverse of the table above for all three signals. As a result, every `<pid>_resp/_card/_triggers.rds` file in
`Results/rds/` held the seat partner's data. Those 987 files were deleted on 2026-10-05, together with the two
QC tables that described them (`physio_summary.csv`, `physio/qc_physio_report.csv`). In single-occupant sessions the occupied seat was
never extracted, so the participant received the empty seat's flat channels. This is why 40 of
43 such participants were flagged "flat + no triggers". The script also skipped 4 files whose names it
could not parse (lower-case `l`, double dot).

None of the old RDS files were used in any reported analysis.

## Evidence for the mapping (2026-10-05)

1. **Task scripts.** The Left scripts write high-nibble values and the Right scripts write
   low-nibble values (above).
2. **Trigger timing.** For three paired sessions (P487/P222, P783/P351, P029/P883),
   intervals between BCAT onset triggers were compared with intervals between `trial.started`
   times in each participant's PsychoPy log. The stream now assigned to a participant matches their
   own log within 2-4 ms (36/36 trials). The partner's log differs by 2-3.5 s.
3. **Breathing.** In the combined task, the change in breath duration across each paced trial
   tracks the pacer direction in the participant's own channel (r = .69 for both participants of
   LP487.RP222, using contained breaths) and not in the other seat's channel (r = -.13, .01).
4. **Mixed values.** Values such as 187 (= 176 + 11) occur when both computers pulse at once,
   so the nibbles are read independently. The old note that mixed values never occur was wrong.

05b does not rely on the table for identity. Every seat's BCAT trigger train is matched
against every PsychoPy BCAT log recorded within a day. A match requires at least 80% of
trials within 100 ms. Results are in `Results/physio/physio_assignment.csv`. 05c then checks
the belt itself: every participant's own belt and the other seat's belt are correlated with that
participant's pacer, and the own belt wins in all but one case (see `breath_trial_features.md`).

## Stuck trigger line (idle at 240)

On many sessions the trigger line idles at 240, which means the high nibble is held at 15. Right-seat codes
arrive intact (241-252), but every Left-seat code is lost. Those Left-seat participants
have physiology but no triggers.

These seats are aligned by wall clock instead (`method = clock_transfer`). The `.acq` header records the
segment start on the acquisition PC (respkit `resp_acq_start_time()`), and each PsychoPy CSV records
`expStart`, the wall time at which that script's clock started. Their difference predicts the trigger-derived
clock offset. On the first session checked it did so to within 0.106-0.126 s, for both computers and two tasks.
05b measures the residual on every trigger-matched file. It applies the median as a constant and reports the
1st-99th percentile spread. In this dataset no seat needed it: every seat without triggers was empty or a
no-show. Eight sessions ran 10 s to 9.5 min off the shared clock, so a transferred seat must also pass the
belt-pacer check in 05c.

## Processing changes beyond the mapping

- Respiration is decimated to 25 Hz with an anti-alias filter (respkit `resp_downsample`,
  80 = 10 x 8, zero-phase). ECG is decimated to 250 Hz (q = 8) the same way. The retired script kept
  every Nth sample with no filtering.
- Trigger events come from respkit (>= 0.2.0) `trigger_events()`. Each seat's field is isolated with
  `trigger_mask` (0xF0 Left, 0x0F Right), the field's own idle level is subtracted (`trigger_idle = "auto"`),
  and runs shorter than 20 ms are ignored (port settle artefacts; genuine pulses last about 1 s). A field
  held at 15 is reported as dead rather than read.
- Each `.acq` is read once, decoding only the 5 channels used (respkit multi-channel `resp_read_acq()`).
  Reading all 17 channels in four parallel workers exhausted memory on the first attempt.
- Outputs use respkit's `resp_recording` container. Its `events` and `task_clock` (one row
  per matched PsychoPy file: `acq_time = psychopy_time + offset`) are on the `.acq` clock.
