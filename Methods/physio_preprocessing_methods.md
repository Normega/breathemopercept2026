# Physiological Data Preprocessing: Sampling Rate Decisions

## Acquisition

Raw physiological data were acquired with a BIOPAC MP160 at **2000 Hz** for all channels (respiration belt, ECG, and event triggers).

## Respiration

Respiration was recorded via a piezoelectric belt. Spectral analysis of the raw signal confirmed that 99.999% of signal power lies below 2 Hz, consistent with the expected physiological range for respiratory rate (0.1--0.5 Hz) and belt harmonics. No meaningful signal content was found above 5 Hz.

| Candidate rate | Nyquist | Power captured |
|---------------|---------|----------------|
| 10 Hz | 5 Hz | 99.9994% |
| 20 Hz | 10 Hz | 99.9997% |
| 25 Hz | 12.5 Hz | 99.9997% |
| 40 Hz | 20 Hz | 99.9998% |

A target rate of **25 Hz** was selected (downsample factor 80 from 2000 Hz, integer). This provides a Nyquist frequency of 12.5 Hz -- well above the highest respiratory frequency of interest -- while reducing file size by 98.75% relative to the raw signal. Rates as low as 10 Hz would be spectrally adequate, but 25 Hz provides comfortable headroom for any motion artifact or belt resonance components that may be present in individual recordings.

## Cardiac (ECG)

ECG was recorded with standard limb leads. Spectral analysis showed that 99.9% of signal power lies below 48 Hz and 99.99% below 75 Hz. The clinically and analytically relevant components are:

- **R-peak detection**: QRS complex energy concentrated 5--40 Hz; sharp transient requires adequate temporal resolution
- **HRV analysis**: frequency-domain HRV uses LF (0.04--0.15 Hz) and HF (0.15--0.4 Hz) bands -- trivially captured at any rate above 1 Hz
- **Waveform morphology** (P, QRS, T waves): up to ~40 Hz

| Candidate rate | Nyquist | Power captured |
|---------------|---------|----------------|
| 125 Hz | 62.5 Hz | 99.9776% |
| 250 Hz | 125 Hz | 99.9968% |
| 500 Hz | 250 Hz | 99.9989% |

A target rate of **250 Hz** was selected (downsample factor 8 from 2000 Hz, integer). This captures 99.997% of ECG signal power and meets standard recommendations for HRV analysis (minimum 250 Hz; Task Force of the European Society of Cardiology, 1996). The 125 Hz option is spectrally acceptable for R-peak detection but loses ~0.02% of power in the 62--125 Hz band where some QRS energy resides; 250 Hz was preferred as the established standard.

## Downsampling method

Both signals were downsampled by **integer decimation** (keeping every Nth sample, factor = raw Hz / target Hz). No anti-aliasing filter was applied prior to decimation. Given that the signal bandwidth is negligible above the Nyquist frequency of the target rate (confirmed spectrally above), aliasing risk is minimal. If waveform morphology analysis is added in future, a low-pass filter prior to decimation would be recommended.

## Triggers

Event triggers were extracted from the raw 2000 Hz trigger channel as an **onset event list** (sample index, time in seconds, event code) rather than being stored as a downsampled per-sample vector. Because trigger pulses in this dataset have a minimum duration of ~780 ms (1566 samples at 2000 Hz), no events are at risk of being missed by decimation. The event-list representation is sampling-rate-agnostic: to align events to a downsampled physio signal, multiply `time_sec` by the target signal's sample rate. Triggers are stored in a separate `<pid>_triggers.rds` file shared across both physio channels.

## Output files

| File | Sample rate | Downsample factor |
|------|------------|-------------------|
| `<pid>_resp.rds` | 25 Hz | 80 |
| `<pid>_card.rds` | 250 Hz | 8 |
| `<pid>_triggers.rds` | event list (no resampling) | -- |
