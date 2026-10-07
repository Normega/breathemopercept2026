# R/01_prep_questionnaires.R
# Loads raw Qualtrics export, cleans, scores all questionnaires,
# and writes questionnaireFile.csv.
# ---------------------------------------------------------------

message("01 | Questionnaire prep...")

qData <- read_xlsx(qualtricsFile)
qData <- qData[-1, ]
qData$Progress <- as.numeric(qData$Progress)
qData <- qData[qData$Progress >= 96, ]

varNames <- c(
  "StartDate", "EndDate", "Status", "IPAddress", "Progress",
  "Duration", "Finished", "RecordedDate", "ResponseId",
  "RecipientLastName", "RecipientFirstName", "RecipientEmail",
  "ExternalReference", "LocationLatitude", "LocationLongitude",
  "DistributionChannel", "UserLanguage", "Sona1", "Sona2",
  "CreditOrPay", "Email1", "Email2",
  paste0("BFI10_", 1:10), "SWLS",
  paste0("BIPS_", 1:10),
  paste0("SPANE_", 1:12),
  paste0("BAT_", 1:23),
  paste0("BriefMAIA_", 1:24),
  paste0("BARQR_", 1:12),
  "Catch1",
  paste0("PHQ4_", 1:4),
  "Age", "YearStudy",
  "GenderMale", "GenderFemale", "GenderNB", "GenderTrans",
  "GenderNotListed", "GenderNoAnswer", "GenderText",
  "Indigenous", "POC",
  "Eth_Arab", "Eth_Black", "Eth_Chinese", "Eth_Filipino",
  "Eth_Indigenous", "Eth_Japanese", "Eth_Korean", "Eth_Latin",
  "Eth_Mixed", "Eth_SouthAsian", "Eth_SEAsian", "Eth_WestAsian",
  "Eth_White", "Eth_3rd", "Eth_Other", "Eth_NoAnswer", "Eth_Text",
  "CountryofBirth", "CommunityLadder_1", "PrimaryLanguage",
  "Languages", "ParentEducation", "ParentEducationTEXT",
  "FirstGen", "FirstGenTEXT", "Program", "ProgramTEXT",
  "MaritalStatus", "EmploymentStatus", "JobStatus",
  "HouseholdIncome", "Residence", "OffcampusHousing", "LivingwithFamily",
  "IPAQ1", "IPAQ1_1_TEXT", "IPAQ2", "IPAQ2_1_TEXT",
  "IPAQ3", "IPAQ3_1_TEXT", "IPAQ4", "IPAQ4_1_TEXT",
  "IPAQ5", "IPAQ5_1_TEXT", "IPAQ6", "IPAQ6_1_TEXT", "IPAQ7",
  "IPAQ7_1_TEXT", "SC1"
)
names(qData) <- varNames

numericList <- c(
  paste0("BFI10_", 1:10), "SWLS",
  paste0("BIPS_", 1:10),
  paste0("SPANE_", 1:12),
  paste0("BAT_", 1:23),
  paste0("BriefMAIA_", 1:24),
  paste0("BARQR_", 1:12),
  paste0("PHQ4_", 1:4),
  "Age", "YearStudy",
  "GenderMale", "GenderFemale", "GenderNB", "GenderTrans",
  "GenderNotListed", "GenderNoAnswer",
  "Indigenous", "POC",
  "Eth_Arab", "Eth_Black", "Eth_Chinese", "Eth_Filipino",
  "Eth_Indigenous", "Eth_Japanese", "Eth_Korean", "Eth_Latin",
  "Eth_Mixed", "Eth_SouthAsian", "Eth_SEAsian", "Eth_WestAsian",
  "Eth_White", "Eth_3rd", "Eth_Other", "Eth_NoAnswer",
  "CountryofBirth", "CommunityLadder_1", "PrimaryLanguage",
  "Languages", "ParentEducation", "FirstGen", "Program",
  "MaritalStatus", "EmploymentStatus", "JobStatus",
  "HouseholdIncome", "Residence", "OffcampusHousing", "LivingwithFamily",
  "IPAQ1", "IPAQ2", "IPAQ3", "IPAQ4", "IPAQ5", "IPAQ6", "IPAQ7", "SC1"
)
qData <- qData |>
  dplyr::mutate(dplyr::across(dplyr::all_of(numericList), as.numeric))

# First complete submission per participant. The export is not in date order,
# so sort by start time first (Excel serial days). Attention checks are
# flagged, not applied: 06_exclusions.R applies them to every analysis frame,
# and a participant's attention status must come from the submission kept.
qData$Sona1 <- sub("\\.0$", "", qData$Sona1)
qData <- qData[!(qData$Sona1 %in% sub("\\.0$", "", TEST_IDS)), ]
qData$start_serial <- suppressWarnings(as.numeric(qData$StartDate))
qData <- qData[order(qData$start_serial), ]
n_sub <- table(qData$Sona1)
qData <- qData[!duplicated(qData$Sona1), ]
qData$n_complete_submissions <- as.integer(n_sub[qData$Sona1])
message(sprintf("  First complete submissions: %d (%d participants had repeats; %d later submissions dropped)",
                nrow(qData), sum(qData$n_complete_submissions > 1),
                sum(qData$n_complete_submissions - 1L)))

qData$attention_pass <- !is.na(qData$BIPS_4) & qData$BIPS_4 == 4 &
  !is.na(qData$Catch1) & qData$Catch1 == "4.0"
message(sprintf("  Attention check failures (flagged): %d", sum(!qData$attention_pass)))

qData <- qData |>
  dplyr::mutate(Gender = dplyr::case_when(
    GenderMale      == 1 ~ "Male",
    GenderFemale    == 1 ~ "Female",
    GenderNB        == 1 ~ "NB",
    GenderTrans     == 1 ~ "Trans",
    GenderNotListed == 1 ~ "Other",
    GenderNoAnswer  == 1 ~ "NoAnswer",
    TRUE ~ NA_character_
  ))

# Item-level responses for every scored scale, plus the fields the analysis
# uses. Scoring, reliability and descriptives happen in 01b, which in analysis
# mode reads the de-identified copy of this file (deidentify.R).
itemCols <- c(paste0("BFI10_", 1:10), "SWLS", paste0("BIPS_", 1:10), paste0("SPANE_", 1:12),
              paste0("BAT_", 1:23), paste0("BriefMAIA_", 1:24), paste0("BARQR_", 1:12),
              paste0("PHQ4_", 1:4))
qItems <- data.frame(id = qData$Sona1, qData[, itemCols], Age = qData$Age, YearStudy = qData$YearStudy,
                     Gender = qData$Gender, attention_pass = qData$attention_pass,
                     n_complete_submissions = qData$n_complete_submissions, stringsAsFactors = FALSE)
write.csv(qItems, questionnaireItemsFile, row.names = FALSE)
message(sprintf("  -> Written: questionnaire_items.csv (%d participants)", nrow(qItems)))
