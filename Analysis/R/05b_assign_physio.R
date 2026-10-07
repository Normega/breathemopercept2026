# R/05b_assign_physio.R
# Assigns each seat extract from 05a to a participant and aligns it to their
# PsychoPy clocks. Writes physioRdsPath/<id>_physio.rds and physio_assignment.csv.
#
# Identity comes from the data, not the filename: each seat's BCAT trigger
# train (code 3) is matched against the trial onsets in every PsychoPy BCAT
# file recorded within a day of the .acq. A match needs >= 80% of a file's
# trials within 100 ms of a trigger (chance alignment of 20+ response-paced
# intervals at this tolerance does not happen). The match also gives that
# script's clock offset: acq_time = psychopy_time + offset.
#
# Which belt each participant wore is decided in 05c against the pacer; the
# ECG lead is chosen here (Pass 4).
#
# Seats whose trigger field is dead (line idling at 240: Left bits held high)
# have no codes. They are assigned by filename and aligned by wall clock: the
# .acq header start plus PsychoPy's expStart, with the constant calibrated on
# every trigger-matched file (method "clock_transfer"). The breath check then
# tests each transfer independently.
# ---------------------------------------------------------------

message("05b | Physio assignment...")

load_respkit()
source(file.path(analysisPath, "R", "physio_functions.R"))

MATCH_TOL      <- 0.10   # s
MATCH_MIN_FRAC <- 0.80
MATCH_MIN_N    <- 5L

# ── Inputs ─────────────────────────────────────────────────────
onsetCache <- file.path(physioQCPath, "task_bcat_onsets.rds")
if (file.exists(onsetCache)) {
  taskOn <- readRDS(onsetCache)
} else {
  taskOn <- read_task_bcat_onsets(taskDataPath)
  saveRDS(taskOn, onsetCache)
}
n_email <- length(unique(taskOn$task_file[grepl("@", taskOn$id)]))
taskOn  <- taskOn[!grepl("@", taskOn$id) & !(taskOn$id %in% TEST_IDS), ]
if (n_email) message(sprintf("  %d task file(s) named with an e-mail address skipped (see docs/NOTES_thesis_reanalysis.md item 1; the file is corrected in id_corrections.csv)", n_email))
task_files <- split(taskOn, taskOn$task_file)

# The combined-task file the behavioural pipeline uses for each id (04: files
# > 72 KB, earliest first).
comb_sizes <- file.info(file.path(taskDataPath, unique(taskOn$task_file[taskOn$task == "combined"])))$size
comb_used <- data.frame(task_file = unique(taskOn$task_file[taskOn$task == "combined"]),
                        size = comb_sizes, stringsAsFactors = FALSE)
comb_used <- comb_used[comb_used$size > 72000, ]
comb_used$id <- sub("^([^_]+)_.*$", "\\1", comb_used$task_file)
comb_used <- comb_used[order(comb_used$task_file), ]
comb_used <- comb_used[!duplicated(comb_used$id), ]

ext_files <- sort(list.files(physioExtractPath, "__[LR][.]rds$", full.names = TRUE))
message(sprintf("  %d seat extracts, %d PsychoPy BCAT files", length(ext_files), length(task_files)))

# ── Pass 1: match every seat to task files ─────────────────────
seat_rows  <- list()
seat_align <- list()
for (ef in ext_files) {
  ext  <- readRDS(ef)
  key  <- sub("[.]rds$", "", basename(ef))
  # Session date from the .acq header (local time); file mtime as a fallback.
  acq_date <- if (!is.null(ext$acq_start_utc) && !is.na(ext$acq_start_utc))
    as.Date(ext$acq_start_utc, tz = "America/Toronto") else
    as.Date(file.info(file.path(physioPath, ext$source_file))$mtime)
  ev3  <- ext$trigger$events$time[ext$trigger$events$code == 3L]

  al <- NULL
  if (length(ev3) >= MATCH_MIN_N) {
    cand <- names(task_files)[vapply(task_files, function(t)
      abs(as.numeric(t$date[1] - acq_date)) <= 1, logical(1))]
    if (!length(cand)) cand <- names(task_files)    # no dated candidate: search all
    al <- dplyr::bind_rows(lapply(cand, function(tf) {
      t <- task_files[[tf]]
      m <- match_clock(ev3, t$onset, tol = MATCH_TOL)
      data.frame(extract = key, task_file = tf, task = t$task[1], id = t$id[1],
                 n_trials = nrow(t), n_matched = m$n_matched, frac = m$frac,
                 offset = m$offset, med_err = m$med_err, stringsAsFactors = FALSE)
    }))
    al <- al[al$frac >= MATCH_MIN_FRAC & al$n_matched >= MATCH_MIN_N, ]
  }
  seat_align[[key]] <- al

  ids <- if (is.null(al) || !nrow(al)) character(0) else unique(al$id)
  seat_rows[[key]] <- data.frame(
    extract = key, source_file = ext$source_file, seat = ext$seat,
    filename_id = ifelse(is.na(ext$filename_id), "", ext$filename_id),
    acq_start_utc = if (is.null(ext$acq_start_utc)) NA_character_ else
      format(ext$acq_start_utc, "%Y-%m-%d %H:%M:%OS3", tz = "UTC"),
    field_stuck = ext$trigger$stuck, n_bcat_trig = length(ev3),
    matched_ids = paste(ids, collapse = ";"),
    n_task_files_matched = if (is.null(al)) 0L else nrow(al),
    stringsAsFactors = FALSE)
}
seats <- dplyr::bind_rows(seat_rows)
align <- dplyr::bind_rows(seat_align)
align$source <- rep("trigger", nrow(align))

# ── Wall-clock calibration ────────────────────────────────────
# acq header start time + PsychoPy expStart predict each clock offset. The
# trigger-matched files measure the residual (PsychoPy stamps expStart a
# little before its clock starts); its median is the transfer constant.
acq_starts <- seats[!duplicated(seats$source_file), c("source_file", "acq_start_utc")]
acq_t0 <- setNames(as.POSIXct(acq_starts$acq_start_utc, tz = "UTC"), acq_starts$source_file)
if (anyNA(acq_t0))
  message(sprintf("  %d .acq file(s) have no header start time; their seats cannot be clock-transferred",
                  sum(is.na(acq_t0))))
exp_t0 <- tapply(taskOn$exp_start, taskOn$task_file, function(x) x[1])
exp_t0 <- as.POSIXct(exp_t0, origin = "1970-01-01", tz = "UTC")
predict_offset <- function(task_file, source_file)
  as.numeric(difftime(exp_t0[task_file], acq_t0[source_file], units = "secs"))

align$source_file <- seats$source_file[match(align$extract, seats$extract)]
align$predicted   <- predict_offset(align$task_file, align$source_file)
align$residual    <- align$offset - align$predicted
CLOCK_CONST <- stats::median(align$residual, na.rm = TRUE)
clock_q <- stats::quantile(align$residual - CLOCK_CONST, c(.01, .05, .5, .95, .99), na.rm = TRUE)
message(sprintf("  Clock transfer: constant %.3f s from %d trigger-matched files; residual 1/5/50/95/99%% = %s s",
                CLOCK_CONST, sum(!is.na(align$residual)),
                paste(sprintf("%+.3f", clock_q), collapse = " / ")))
# Residuals are a property of a session (that day's PC clocks), so summarise
# them per .acq file: a few sessions run minutes off while the rest agree to
# ~0.1 s. Any clock-transferred seat is still re-checked against its breathing.
CLOCK_TOL <- 0.5
sess_resid <- tapply(align$residual - CLOCK_CONST, align$source_file, stats::median, na.rm = TRUE)
n_off <- sum(abs(sess_resid) > CLOCK_TOL, na.rm = TRUE)
message(sprintf("  Clock transfer: %d of %d trigger-matched sessions have |residual| > %.1f s (%s)",
                n_off, sum(!is.na(sess_resid)), CLOCK_TOL,
                paste(sprintf("%+.1f", sort(sess_resid[abs(sess_resid) > CLOCK_TOL])), collapse = ", ")))

# ── Pass 2: decide identity per seat ──────────────────────────
seats$assigned_id <- NA_character_
seats$method      <- NA_character_
seats$note        <- ""
for (i in seq_len(nrow(seats))) {
  m <- strsplit(seats$matched_ids[i], ";")[[1]]
  m <- m[nzchar(m)]
  fid <- seats$filename_id[i]
  if (length(m) == 1L) {
    seats$assigned_id[i] <- m
    seats$method[i] <- "trigger_match"
    if (nzchar(fid) && fid != m)
      seats$note[i] <- paste0("filename says ", fid)
  } else if (length(m) > 1L) {
    # One seat's trigger train matches task files under several ids. Two cases,
    # told apart by which tasks each id holds:
    #  - complementary (e.g. baseline BCAT under one id, combined task under
    #    the same id with a digit dropped): one person, an ID typed differently in one script -> alias,
    #    assigned to the id that holds the baseline BCAT;
    #  - each id has its own baseline BCAT: consecutive participants on a
    #    recording that was never stopped -> the recording is shared, and
    #    each id gets only its own task clocks (rows added below).
    al_i <- align[align$extract == seats$extract[i], ]
    has_base <- vapply(m, function(id) any(al_i$task[al_i$id == id] == "bcat_baseline"), logical(1))
    if (sum(has_base) == 1L) {
      seats$assigned_id[i] <- m[has_base]
      seats$method[i] <- "trigger_match_alias"
      seats$note[i] <- paste0("same person under ids ", paste(m, collapse = ","),
                              " (complementary task files)")
    } else {
      seats$assigned_id[i] <- m[1]
      seats$method[i] <- "trigger_match_shared"
      seats$note[i] <- paste0("recording shared by consecutive participants ",
                              paste(m, collapse = ","))
      for (id in m[-1]) {
        extra <- seats[i, ]
        extra$assigned_id <- id
        seats <- rbind(seats, extra)
      }
    }
  } else if (nzchar(fid) && fid %in% taskOn$id) {
    seats$assigned_id[i] <- fid
    seats$method[i] <- if (seats$field_stuck[i]) "filename_only" else "filename_unmatched"
  } else {
    seats$method[i] <- if (nzchar(fid)) "no_task_data" else "empty_seat"
  }
}

# Task ids whose clocks belong to a seat row: all matched ids for an alias,
# otherwise only the assigned id (a shared recording is split by id).
row_ids <- function(i) {
  if (identical(seats$method[i], "trigger_match_alias"))
    strsplit(seats$matched_ids[i], ";")[[1]] else seats$assigned_id[i]
}
row_clock <- function(i) align[align$extract == seats$extract[i] & align$id %in% row_ids(i), ]

# A trigger match outranks a filename claim on the same id.
trig_ids <- seats$assigned_id[grepl("^trigger_match", seats$method)]
clash <- seats$method %in% c("filename_only", "filename_unmatched") &
  seats$assigned_id %in% trig_ids
seats$note[clash]   <- paste0("filename id ", seats$assigned_id[clash],
                              " is trigger-matched elsewhere")
seats$method[clash] <- "superseded_filename"
seats$assigned_id[clash] <- NA

# Clock transfer for seats assigned by filename: every BCAT file of that id
# recorded within a day of the .acq, at the calibrated wall-clock offset.
for (i in which(seats$method %in% c("filename_only", "filename_unmatched"))) {
  acq_date <- as.Date(acq_t0[seats$source_file[i]])
  tf <- unique(taskOn$task_file[taskOn$id == seats$assigned_id[i] &
                                  abs(as.numeric(taskOn$date - acq_date)) <= 1])
  if (!length(tf) || is.na(acq_date)) next
  pr <- predict_offset(tf, seats$source_file[i])
  ok <- !is.na(pr)
  if (!any(ok)) next
  align <- dplyr::bind_rows(align, data.frame(
    extract = seats$extract[i], task_file = tf[ok],
    task = taskOn$task[match(tf[ok], taskOn$task_file)], id = seats$assigned_id[i],
    n_trials = as.integer(table(taskOn$task_file)[tf[ok]]),
    offset = pr[ok] + CLOCK_CONST, source = "clock_transfer",
    source_file = seats$source_file[i], predicted = pr[ok], stringsAsFactors = FALSE))
  seats$method[i] <- "clock_transfer"
}

# ── Pass 3: one recording per participant ─────────────────────
# The seat holding the combined-task file the behavioural pipeline uses, else
# the most trigger-matched trials, else any.
seats$uses_behavioural_file <- vapply(seq_len(nrow(seats)), function(i)
  !is.na(seats$assigned_id[i]) && any(row_clock(i)$task_file %in% comb_used$task_file),
  logical(1))
seats$n_matched_total <- vapply(seq_len(nrow(seats)), function(i)
  sum(row_clock(i)$n_matched, na.rm = TRUE), numeric(1))
cand <- seats[!is.na(seats$assigned_id), ]
cand <- cand[order(cand$assigned_id, -cand$uses_behavioural_file,
                   -cand$n_matched_total), ]
cand$canonical <- !duplicated(cand$assigned_id)
seats$canonical <- paste(seats$extract, seats$assigned_id) %in%
  paste(cand$extract, cand$assigned_id)[cand$canonical]

# ── Pass 4: which ECG lead did the participant wear? ──────────
# The hardware map is the default, but the transmitters are wireless and
# interchangeable, and in single-occupant sessions RAs sometimes fitted the
# other seat's lead. Switch (unoccupied other seat only) when the other lead's
# R-peak score is >= .6 and beats this participant's by >= .3.
# The belt is decided in 05c, where the pacer gives a far stronger test than
# anything available here; this step stores the other seat's belt as an
# alternative (`resp_alt`) and whether a participant sat there.
ECG_SWITCH_MIN <- 0.6; ECG_SWITCH_MARGIN <- 0.3
other_extract <- function(key) sub("__([LR])$", ifelse(grepl("__L$", key), "__R", "__L"), key)
seats$other_seat_occupied <- NA
seats$other_seat_id       <- NA_character_
seats$heart_source        <- NA_character_
seats$ecg_quality_own     <- NA_real_
seats$ecg_quality_other   <- NA_real_
for (i in which(seats$canonical)) {
  j <- match(other_extract(seats$extract[i]), seats$extract)
  occupied <- !is.na(j) && !is.na(seats$assigned_id[j])
  seats$other_seat_occupied[i] <- occupied
  if (occupied) seats$other_seat_id[i] <- seats$assigned_id[j]

  e_own <- readRDS(file.path(physioExtractPath, paste0(seats$extract[i], ".rds")))$card
  seats$ecg_quality_own[i] <- ecg_quality(e_own$signal, e_own$fs)
  seats$heart_source[i] <- "own_seat"
  if (!is.na(j) && !occupied) {
    e_oth <- readRDS(file.path(physioExtractPath, paste0(seats$extract[j], ".rds")))$card
    seats$ecg_quality_other[i] <- ecg_quality(e_oth$signal, e_oth$fs)
    if (seats$ecg_quality_other[i] >= ECG_SWITCH_MIN &&
        seats$ecg_quality_other[i] - seats$ecg_quality_own[i] >= ECG_SWITCH_MARGIN)
      seats$heart_source[i] <- "other_seat"
  }
}

# ── Write per-participant files ───────────────────────────────
for (i in which(seats$canonical)) {
  key <- seats$extract[i]
  ext <- readRDS(file.path(physioExtractPath, paste0(key, ".rds")))
  oth_path <- file.path(physioExtractPath, paste0(other_extract(key), ".rds"))
  oth <- if (file.exists(oth_path)) readRDS(oth_path) else NULL
  pid <- seats$assigned_id[i]

  resp <- ext$resp; resp$id <- pid
  resp_alt <- NULL
  if (!is.null(oth)) {
    resp_alt <- oth$resp
    resp_alt$id <- pid
    resp_alt$events <- ext$resp$events     # this participant's triggers
    resp_alt$meta$seat <- ext$seat
    resp_alt <- add_provenance(resp_alt, "candidate belt from the other seat (%s)",
                               oth$channels$breath)
  }
  card <- if (seats$heart_source[i] == "other_seat") oth$card else ext$card

  saveRDS(list(
    schema        = "aya_physio_v2",
    id            = pid,
    source_file   = ext$source_file,
    seat          = ext$seat,
    method        = seats$method[i],
    breath_source = "own_seat",          # may be revised by 05c
    heart_source  = seats$heart_source[i],
    resp          = resp,                # resp_recording on the acq clock, events attached
    resp_alt      = resp_alt,            # the other seat's belt, same clock and events
    alt_occupied_by = seats$other_seat_id[i],
    card          = card,                # list(signal, fs, units, channel), acq clock
    events        = ext$trigger$events,
    task_clock    = row_clock(i),
    checks        = seats[i, c("ecg_quality_own", "ecg_quality_other")],
    respkit_version = ext$respkit_version
  ), file.path(physioRdsPath, paste0(pid, "_physio.rds")))
}

write.csv(seats, physioAssignmentFile, row.names = FALSE)
# Task ids that the physiology shows to be one person (fix these upstream in
# the behavioural prep too: the alias currently looks like a separate person).
alias_rows <- which(seats$method == "trigger_match_alias")
aliases <- do.call(rbind, lapply(alias_rows, function(i) {
  ids <- strsplit(seats$matched_ids[i], ";")[[1]]
  data.frame(alias_id = setdiff(ids, seats$assigned_id[i]), assigned_id = seats$assigned_id[i],
             source_file = seats$source_file[i], seat = seats$seat[i], stringsAsFactors = FALSE)
}))
if (!is.null(aliases)) {
  write.csv(aliases, file.path(physioQCPath, "physio_id_aliases.csv"), row.names = FALSE)
  message(sprintf("  %d task id alias(es) found; see physio_id_aliases.csv", nrow(aliases)))
}
write.csv(align, file.path(physioQCPath, "physio_task_clock.csv"), row.names = FALSE)

message("  Assignment methods (all seats):")
print(table(seats$method))
message(sprintf("  Participants with physio: %d (trigger-matched %d, clock-transferred %d, unaligned %d)",
                sum(seats$canonical),
                sum(seats$canonical & grepl("^trigger_match", seats$method)),
                sum(seats$canonical & seats$method == "clock_transfer"),
                sum(seats$canonical & !grepl("^trigger_match|^clock_transfer", seats$method))))
message("  ECG source (participants):")
print(table(seats$heart_source[seats$canonical], useNA = "ifany"))
message("05b | Physio assignment complete.")
