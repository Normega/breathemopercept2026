# Notes for the full analysis of Aya's thesis paper

Collected 2026-10-01 during the salience reanalysis (`Analysis/salience_reanalysis.R`,
outputs in `Results/salience_reanalysis/`). Nothing in the existing pipeline was changed;
these are items to resolve before the paper analysis is finalised.

## Data and pipeline issues

**Status 2026-10-05:** items 1-4 and 6a are resolved in the pipeline. See
`Methods/participant_flow.md` and `Results/confirmatory_rerun/report.md`. Item 5 is
reported in `exclusions.csv`.

1. **[RESOLVED: the file is participant P113, by trigger match; `Data/id_corrections.csv` (private)]** **Email address in the de-identified data.** One combined-task file was saved with an
   e-mail address instead of a participant number. The id regex in `04_prep_combined.R`
   falls through, so the address is the `id` value in `Combined_data.csv`,
   `Combined_fullBCAT_data.csv` and `Combined_fullGERT_data.csv`. The salience script
   pseudonymises it in memory only. Fix upstream (map to the correct participant number,
   or exclude) and regenerate the CSVs.
2. **[RESOLVED: 02-04 now use `task_file_index()` and `file.path`]** **`04_prep_combined.R` cannot regenerate its outputs.** It builds paths with
   `paste0(taskDataPath, file)`, and `taskDataPath` (from `config.R`, built with
   `file.path()`) has no trailing separator. The current Combined_*.csv files came from an
   earlier version. Switch to `file.path(taskDataPath, file)`.
3. **[RESOLVED: 06 applies one flow to every frame]** **Exclusions do not propagate to the combined task.** `06_exclusions.R` applies
   attention-check and duplicate exclusions only via the questionnaire file, and BCAT
   technical failures only to the baseline BCAT data. Combined-task models in
   `08_hypotheses.R` therefore include participants who failed attention checks (6 with
   combined data). Decide the exclusion set and apply it to every analysis frame.
4. **[RECONCILED: 7 without a questionnaire, 7 attention, 10 BCAT technical, 19 with fewer than 6 blocks; 17 repeat submitters. See `Results/participant_flow.csv`]** **Exclusion counts do not match the thesis text** (18 duplicates, 7 attention, 8 BCAT
   technical, 21 with < 6 blocks):
   - Attention: 7 failures in Qualtrics, 6 with combined-task data.
   - Duplicates: 17 repeat Sona IDs among complete (Progress >= 96) Qualtrics rows; 21 at
     any progress level. Task-file duplicates are resolved in `04` (earliest file > 72 KB).
   - < 6 blocks: 0 remain in the CSVs, because `04` already drops files <= 72 KB
     (24 participants have only small files). The 21 in the thesis is presumably that filter.
   - BCAT technical: 0 in `BCAT_baseline_data.csv`; 11 combined-task participants have no
     baseline BCAT rows at all. Check whether these are the 8 technical failures.
   - Reconcile and report a single CONSORT-style flow (306 is the reported N; the
     combined-task CSVs contain 293 participants).
5. **Incomplete blocks.** 15 blocks have fewer than 3 BCAT trials, 26 have fewer than 5
   GERT clips, and 8 BCAT trials have missing salience. One participant has 13 BCAT
   blocks (check for a merged or restarted file).

6a. **[RESOLVED: `Data/id_corrections.csv` (private)]** **Task-ID alias: a mistyped ID is P961** (found by the physio assignment on 2026-10-05). The same
    seat's trigger train matches P961's baseline BCAT and the mistyped file's combined task, and the two
    ID sets are complementary. The behavioural prep treated the mistyped ID as a separate
    participant with no questionnaire or baseline data. Map the mistyped ID to P961 upstream
    (`Results/physio/physio_id_aliases.csv`).

## Design facts the paper should state precisely

6. **Each combined-task block = 2 change trials + 1 no-change catch trial**, with salience
   and direction fixed within the block (verified: constant in all 3,402 blocks). The
   catch trial stops participants from answering the same direction every time.
7. **The thesis "aware" definition includes the catch trial** (`bcatAccuracy` = mean of all
   3 trials, aware = >= 2/3). Detection of the changes alone is 0, 1 or 2 of 2. Report which
   definition is used and show that results hold under the other (they did here).
8. **Arousal is rated after every BCAT trial** (1 to 3 distinct values within a block). For
   predicting the GERT clips that follow, average it within block.
9. **Clip is a crossed random factor**: 60 clips, each seen once per person. The thesis
   H3 models use block-mean GERT scores with `(1 | id)` only. Trial-level models with
   `(1 | clip)` are preferable.

## Analytic points raised by the reanalysis

10. **The thesis awareness effect replicates** with the 08_hypotheses.R specification
    (aware b = .082, 95% CI [.017, .146], p = .013, vs .077 in the thesis), but it is a
    post hoc, within-person, between-block association, not an experimental effect.
11. **Randomised salience does not move perceived intensity** (b = -.001 [-.035, .032],
    p = .94), nor does direction at either salience level, nor the interaction (p = .93),
    even though salience strongly raises detection (OR = 1.77; 71.5% vs 60.3%).
12. **Salience amplifies the felt-arousal response to direction** (sal x dir b = .070,
    p = .011; accel - decel arousal .211 high vs .141 low salience).
13. **Block-level felt arousal tracks perceived intensity within person** (b = .053,
    p < .001), but **less so under high salience** (sal x arousal b = -.065, p = .033).
    This is the opposite of an "awareness amplifies" account; worth discussing.
14. **Moderated mediation (sal x dir -> block arousal -> intensity)** is positive but tiny
    (.0037; block-level CI [-.0003, .0092]; trial-level a path CI [.0007, .0081]) with no
    total effect to explain. Treat as exploratory.
15. **Random-effect simplification.** Salience random slopes were singular for detection,
    arousal and GERT accuracy. Report the structures used.
16. **Multiplicity.** The preregistration applies BH only to H1/H2 correlations. The
    combined-task models (H3, H4, manipulation check, and everything in the salience
    reanalysis) need an explicit stance on correction.
17. **H4 uses high-salience blocks only.** With salience available as a factor, a
    full sal x dir x burnout model would be more informative.
