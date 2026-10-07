# Handoff: what the pilot shows about awareness of arousal and emotion perception

**For:** grant LOI (submitting Saturday, October 3, 2026)
**Prepared:** October 1, 2026
**Data:** breathing x emotion pilot (preregistration osf.io/wz32n), combined BCAT-GERT task
**Analysis:** `Analysis/salience_reanalysis.R`; outputs in `Results/salience_reanalysis/`
(`salience_report.md`, `salience_models.txt`, `salience_moderation.txt`)
**Sample:** 287 participants (293 with combined-task data, minus 6 attention-check failures),
3,302 blocks, 16,474 GERT clip ratings. Within-person design: each block paired a breathing
change (acceleration or deceleration; high or low salience) with 5 GERT clips that followed.

## Bottom line

The pilot supports a **correlational, within-person** claim: in blocks where people were
more aware of a change in their breathing, and in blocks where they felt more aroused, they
perceived other people's emotions as more intense. The effects are small and reliable enough
to motivate the proposal.

The pilot does **not** show that we can produce this effect experimentally. Making breathing
changes more salient made them easier to detect and amplified felt arousal, but did not by
itself change emotion perception. Experimentally manipulating awareness to test its effect on
emotion perception should be framed as an aim of the proposed research, not as a pilot result.

## Evidence we can cite

All estimates come from mixed models with participant random effects. Intensity is rated on
a 1 to 7 scale.

| Finding | Estimate | Strength |
|---|---|---|
| Blocks classified as "aware" (>= 2 of 3 BCAT trials correct) had higher perceived emotion intensity | b = .082, 95% CI [.017, .146], p = .013 (thesis: b = .077, p = .016) | Replicates the thesis; post hoc classification |
| Detecting more of the block's breathing changes than one's own average went with higher perceived intensity | b = .051, SE = .026, p = .051 | Borderline; same direction as above |
| Feeling more aroused than one's own average in a block went with higher perceived intensity of the clips that followed | b = .053, 95% CI [.022, .084], p < .001 | Most robust link between felt arousal and emotion perception |
| Breathing changes are felt: acceleration raised felt arousal more than deceleration | b = .176, SE = .014, p < 10^-36 | Strong; validates the physiological manipulation |
| Salience is an effective manipulation of detection | OR = 1.77; 71.5% vs 60.3% of changes detected; p < 10^-24 | Strong; feasibility for the proposed manipulation |

## What not to claim

- **That we can manipulate the awareness-perception effect.** Randomly assigned salience did
  not change perceived intensity (b = -.001, 95% CI [-.035, .032], p = .94), and neither did
  breathing direction or their interaction (all p > .69). The interval rules out anything
  more than a very small effect.
- **That awareness amplifies the arousal-perception link.** Salience did amplify felt arousal
  (salience x direction b = .070, p = .011), but the indirect path to perceived intensity is
  tiny (about .004 scale points; block-level 95% CI [-.0003, .0092]). The arousal-intensity
  association was, if anything, weaker in high-salience blocks (p = .033).
- **Causal language** about the associations in the table above. They are within-person,
  between-block correlations: awareness and arousal were measured, not assigned.
- **Effect sizes framed as large.** All emotion-perception effects are under a tenth of a
  scale point.

## Suggested framing for the LOI

> In a preregistered pilot (N = 287), participants detected externally paced changes to
> their breathing while rating others' emotional expressions. When participants were more
> aware of a bodily change, or felt more aroused, they perceived others' emotions as more
> intense. A salience manipulation reliably increased detection of the changes and amplified
> their felt intensity, establishing that bodily awareness can be experimentally shifted.
> The proposed research will test whether directly manipulating awareness of arousal causally
> alters emotion perception.

The final sentence proposes the manipulation test as future work. Even so, reviewers may ask
whether the pilot salience manipulation already tested it. If asked, the honest answer is yes:
increasing the salience of the breathing change did not by itself shift emotion perception.
That motivates stronger or more direct manipulations of awareness of arousal (for example,
explicit attention to the change, or feedback) rather than salience alone.

## Caveats

- This is a secondary analysis of the preregistered dataset, and the p values are uncorrected.
- The exclusions differ from the thesis: only attention-check failures were removed (see
  `NOTES_thesis_reanalysis.md`). The thesis awareness effect replicates under both.
- Detection was scored on the 2 change trials per block; each block also contains 1
  no-change catch trial. Results hold if detection is scored on all 3 trials.
