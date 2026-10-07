# Participant identity, file selection and exclusions

Written 2026-10-05. It covers `functions.R` (`task_file_index`, `select_task_files`), `R/01`-`R/04`,
`R/06_exclusions.R` and `Analysis/id_corrections.csv`.

## Identity

- **Questionnaire.** A participant's ID is their SONA ID (`Sona1`; the confirmation field `Sona2` agrees in every row).
  The Qualtrics export is not in date order. Submissions are sorted by start time, and the **first complete**
  submission (Progress >= 96) is kept. 17 participants had repeat complete submissions.
- **Task files.** The ID is the filename prefix typed into PsychoPy, after the documented corrections in
  `id_corrections.csv`. Each correction carries its evidence:
  - One combined-task file was saved under an e-mail address. Its BCAT trigger train matches the seat whose
    baseline BCAT is participant **P113** (36/36 trials, median error 5 ms). On 2026-10-06 the raw CSV and its
    `.log` and `.psydat` were renamed to the `P113_` prefix, so no correction row is needed for it now. The address
    is still stored inside those three files (PsychoPy's `participant` field), and the pipeline never reads that field.
  - The combined task saved under a mistyped ID (the leading digit dropped) is participant **P961**: same seat and session as P961's baseline
    BCAT, and complementary task files.

  Neither ID's data appears under any other ID.
- **Tests and pilots.** Excluded before anything else: known test IDs, non-numeric IDs, and files dated
  before 1 November 2025, which were pilot sessions (Oct 1-28).

## Which file

When a participant has several files for a task (restarts), each prep step takes the **earliest file at
least as large as a complete run**: BCAT >= 20 KB, GERT >= 30 KB, combined >= 72 KB. A participant with
only smaller files keeps the largest, so 06 can count how far they got rather than losing them silently.

## Exclusions (06), applied to every analysis frame

| Step | Excluded | Remaining | Used for |
|---|---|---|---|
| Participants with data (non-test IDs) | | 329 | |
| No complete questionnaire (no attention check or trait data) | 7 | 322 | |
| Failed attention check (on the first submission) | 7 | 315 | H1 (baseline GERT) |
| BCAT technical failure (no thresholds) | 10 | 305 | H2 |
| Fewer than 6 of 12 combined-task blocks | 19 | 286 | H3, H4, combined-task exploratory |

The BCAT failure criterion applies to the combined task as well as the BCAT analyses, because the combined
task's change magnitudes are the participant's baseline thresholds. Compared with the thesis text (18
duplicates, 7 attention, 8 BCAT technical, 21 under 6 blocks), the attention count matches. The duplicate
count is 17 here; the source of the thesis's 18 is not recoverable from the scripts. The BCAT and block
counts differ by 2 each, because the rules above are explicit where the thesis's were not documented.

Per-participant flags are in `Results/exclusions.csv`, and the flow is in `Results/participant_flow.csv`.
