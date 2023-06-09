
# CONSTANTS ---------------------------------------------------------------

## CLINICAL_EVENTS_FIELD_IDS ---------------------------------------------------------

CLINICAL_EVENTS_FIELD_IDS <- list(
  # for death data, code_fid includes primary and secondary causes
  primary_death_icd10 = c(code_fid = "40001", date_fid = "40000"),
  secondary_death_icd10 = c(code_fid = "40002", date_fid = "40000"),
  self_report_medication = c(code_fid = "20003", date_fid = "53"),
  self_report_non_cancer = c(code_fid = "20002", date_fid = "20008"),
  self_report_non_cancer_icd10 = c(code_fid = "20002", date_fid = "20008"),
  self_report_cancer = c(code_fid = "20001", date_fid = "20006"),
  self_report_operation = c(code_fid = "20004", date_fid = "20010"),
  cancer_register_icd9 = c(code_fid = "40013", date_fid = "40005"),
  cancer_register_icd10 = c(code_fid = "40006", date_fid = "40005"),
  summary_hes_icd9 = c(code_fid = "41271", date_fid = "41281"),
  summary_hes_icd10 = c(code_fid = "41270", date_fid = "41280"),
  summary_hes_opcs3 = c(code_fid = "41273", date_fid = "41283"),
  summary_hes_opcs4 = c(code_fid = "41272", date_fid = "41282")
)

## CLINICAL EVENTS SOURCES --------------------------------------------------

# This relates to the clinical events table generated by the `tidy_clinical_events`

# sources - describe possible values under the `source` column

# "f*" = fieldID with "f" prefix, numerical data_codings are UKB data-codings.
# description and category are copied from the UKB data showcase

# gpc_r2 and gpc_r3 are read2 and 3 codes from the gp_clinical table

CLINICAL_EVENTS_SOURCES <- tibble::tribble(
  ~source, ~data_coding, ~description, ~category, ~file,
  "f40001", "icd10", "Underlying (primary) cause of death", "Death register", "ukb_main",
  "f40002", "icd10", "Contributory (secondary) cause of death", "Death register", "ukb_main",
  "f20002", "data_coding_6", "Non-cancer illness code, self-reported", "Medical conditions", "ukb_main",
  "f20002_icd10", "icd10", "Non-cancer illness code, self-reported", "Medical conditions", "ukb_main",
  "f20001", "data_coding_3", "Cancer code, self-reported", "Medical conditions", "ukb_main",
  "f20004", "data_coding_5", "Operation code, self-reported", "Operations", "ukb_main",
  "f40013", "icd9", "Type of cancer: ICD9", "Cancer register", "ukb_main",
  "f40006", "icd10", "Type of cancer: ICD10", "Cancer register", "ukb_main",
  "f41270", "icd10", "Diagnoses - ICD10", "Summary Diagnoses - Hospital inpatient - Health-related outcomes", "ukb_main",
  "f41271", "icd9", "Diagnoses - ICD9", "Summary Diagnoses - Hospital inpatient - Health-related outcomes", "ukb_main",
  "f41272", "opcs4", "Operative procedures - OPCS4", "Summary Operations - Hospital inpatient - Health-related outcomes", "ukb_main",
  "f41273", "opcs3", "Operative procedures - OPCS3", "Summary Operations - Hospital inpatient - Health-related outcomes", "ukb_main",
  "f20003", "data_coding_4", "Treatment/medication code, self-reported", "Medications", "ukb_main",
  "gpc1_r2", "read2", "`read_2` column, data provider England (Vision)", "Primary care", "gp_clinical",
  "gpc2_r2", "read2", "`read_2` column, data provider Scotland", "Primary care", "gp_clinical",
  "gpc4_r2", "read2", "`read_2` column, data provider Wales", "Primary care", "gp_clinical",
  "gpc3_r3", "read3", "`read_3` column, data provider England (TPP)", "Primary care", "gp_clinical",
  "gps1_r2", "read2_drugs", "`read_2` column, data provider England (Vision)", "Primary care", "gp_scripts",
  "gps1_dmd", "dmd", "`dmd_code` column, data provider England (Vision)", "Primary care", "gp_scripts",
  "gps2_r2", "read2_drugs", "`read_2` column, data provider Scotland", "Primary care", "gp_scripts",
  "gps2_bnf", "bnf", "`bnf_code` column, data provider Scotland", "Primary care", "gp_scripts",
  "gps3_bnf", "bnf", "`bnf_code` column, data provider England (TPP)", "Primary care", "gp_scripts",
  "gps4_r2", "read2_drugs", "`read_2` column, data provider Wales", "Primary care", "gp_scripts"
)

## NONSENSE DATES ----------------------------------------------------------

# make list containing nonsense dates
nonsense_dates_categories <- c(
  "PRIMARY_CARE",
  "MAIN_DATASET"
)

NONSENSE_DATES <- vector(mode = "list", length = length(nonsense_dates_categories))
names(NONSENSE_DATES) <- nonsense_dates_categories

# populate list
NONSENSE_DATES$PRIMARY_CARE <- c(
  "01/01/1901",
  "02/02/1902",
  "03/03/1903",
  "07/07/2037"
)

# from codings 1313, 272, 586 and 819
NONSENSE_DATES$MAIN_DATASET <- c(
  "1904-04-04",
  "1900-01-01",
  "1910-01-01",
  "1920-01-01",
  "1930-01-01",
  "1901-01-01",
  "1902-02-02",
  "1903-03-03",
  "2037-07-07"
)
