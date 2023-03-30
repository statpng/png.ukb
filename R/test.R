# ukbwranglr

# devtools::install_github("rmgpanw/ukbwranglr")


if(FALSE){


  {
    library(tidyverse)
    
    library(png.ukb)
    library(devtools)
    library(dplyr)
    library(tidyr)
    library(tidyselect)
    library(readr)
    library(tibble)

  }
  
  
  example_data = TRUE
  if(example_data) {
    
    dummy_ukb_dict <- get_ukb_dummy("dummy_Data_Dictionary_Showcase.tsv")
    dummy_ukb_codings <- get_ukb_dummy("dummy_Codings.tsv")
    dummy_ukb_main_path <- get_ukb_dummy("dummy_ukb_main.tsv", path_only = TRUE)
    
    ukb_data <- read_ukb(
      path = dummy_ukb_main_path,
      ukb_dict = dummy_ukb_dict,
      ukb_codings = dummy_ukb_codings
    ) %>% tibble::as_tibble()
    ukb_dict <- get_ukb_dict()
    ukb_coding <- get_ukb_codings()
    ukb_data_dict <- make_data_dict( ukb_main = dummy_ukb_main_path,
                                     ukb_dict = ukb_dict)
    
  } else {
    
    path <- "/Volumes/T7/Yonsei/3. UKB/Clinical/Maindata3/"
    filename <- "ukb669778"
    
    ukb_data <- data.table::fread( path%_%filename%_%".tab", nrows=100)
    ukb_field <- ukb_df_field(filename, path)
    ukb_dict <- get_ukb_data_dict()
    ukb_coding <- get_ukb_codings()
    ukb_data_dict <- make_data_dict( ukb_main=path%_%filename%_%".tab",
                                     ukb_data_dict = ukb_dict)
    
  }
  
  ukb_dict2 <- split.ukb_dict(ukb_dict)
  ukb_data_dict2 <- split.ukb_data_dict(ukb_data_dict)
  
  
  df_path <- ukb_dict2 %>% select(starts_with("path"))
  
  
  
  
  #
  
  
  ukb_icd_diagnosis(ukb_data, id=1:5, icd.version=10)
  ukb_icd_code_meaning(icd.code = "E10", icd.version = 10)
  ukb_icd_keyword("cardio|atrial", icd.version = 10)
  ukb_icd_prevalence(ukb_data, icd.code="I4[8-9].", icd.version = 10)
  ukb_icd_prevalence(ukb_data, icd.code="E1[0-1]", icd.version = 10)
  
  
  ukb_data_dict %>% head()
  ukb_data_dict %>% filter(colheaders_processed == "f41270_0_0") %>% as.data.frame
  ukb_data_dict %>% filter(colheaders_processed == "f41270_0_3") %>% .$Field
  ukb_data_dict
  
  #
  
  
  


  dim(data_dict) # nrow(.) is equal to ncol(df)


  ## 2. Read selected variables into R
  # Read a main UK Biobank dataset into R using `read_ukb()`:

  var1 <- c(
    "eid", # Participant ID
    "31", # Sex
    "34", # Year of birth
    "21001", # Body mass index
    "4080", # Systolic blood pressure
    "20002", "20008", # Self-reported non-cancer medical conditions
    "41270", "41280" # Summary hospital diagnoses (ICD10)
  )

  ukb_main <- read_ukb(path = path%_%"ukb669778.tab",
                  data_dict = ukb_data_dict %>% filter(FieldID %in% var1),
                  nrows=10) %>% as_tibble()

  
  

  ## 3. Summarise continuous variables

  # calculate the mean value across all repeated continuous variable measurements
  ukb_main_numerical_vars_summarised <- summarise_numerical_variables(
    ukb_main = ukb_main,
    ukb_data_dict = ukb_data_dict,
    summary_function = "mean",
    .drop = TRUE
  ) %>% select(eid,
               sex_f31_0_0,
               year_of_birth_f34_0_0,
               mean_body_mass_index_bmi_x21001,
               mean_systolic_blood_pressure_automated_reading_x4080) %>%
    as_tibble()


  ## 4. Tidy clinical events data and extract outcomes of interest

  # tidy clinical events
  clinical_events <- tidy_clinical_events(
    ukb_main = ukb_main,
    ukb_data_dict = ukb_data_dict,
    ukb_codings = ukb_codings,
    clinical_events_sources = c("self_report_non_cancer",
                                "summary_hes_icd10")
  )

  # returns a named list of data frames
  clinical_events

  # combine with dplyr
  clinical_events <- dplyr::bind_rows(clinical_events)

  clinical_events




  example_clinical_codes()







  # extract phenotypes
  diabetes_cases <- extract_phenotypes(clinical_events = clinical_events,
                                       clinical_codes = example_clinical_codes())

  diabetes_cases




  ## 5. Analyse

  ukb_main_processed <-
    # first summarise `diabetes_cases` - for each eid, get the earliest date
    diabetes_cases %>%
    group_by(eid,
             disease) %>%
    summarise(diabetes_min_date = min(date, na.rm = TRUE)) %>%
    ungroup() %>%

    # create indicator column for diabetes
    mutate(diabetes_indicator = case_when(!is.na(diabetes_min_date) ~ "Yes",
                                          TRUE ~ "No")) %>%

    # join with `ukb_main_numerical_vars_summarised`
    dplyr::full_join(ukb_main_numerical_vars_summarised,
                     by = "eid")

  ukb_main_processed


  # Describe:

  ukb_main_processed %>%
    select(-eid) %>%
    group_by(diabetes_indicator) %>%
    summarise(pct_female = sum(sex_f31_0_0 == "Female", na.rm = TRUE) / n(),
              across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))




  # Setup for multiple projects

  # The following setup is recommended to reduce duplicated steps between multiple projects that draw on the same datasets:
  #
  #   1.  Download the UK Biobank data dictionary and codings files (available from the [UK Biobank data showcase](https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide)) and for each new R project, place the following `.Renviron` file in the project root directory (replacing `PATH/TO` with the correct file paths):
  #
  #   UKB_DATA_DICT=/PATH/TO/Data_Dictionary_Showcase.tsv
  #
  #   UKB_CODINGS=/PATH/TO/Codings.tsv
  #
  # Functions with arguments `ukb_data_dict` and `ukb_codings` use `get_ukb_data_dict()` and `get_ukb_codings()` by default, which will automatically search for environmental variables `UKB_DATA_DICT` and `UKB_CODINGS` and read the files from these locations.
  #
  # 2.  Create a clinical events database using `make_clinical_events_db()`. This function includes the option to incorporate [primary care data](https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=3000 "Primary care health-related outcomes").[^4] Having connected to the database, phenotypes may be extracted with `extract_phenotypes()`:
  #
  #   [^4]: It will take \\\~1 hour to run if including both the primary care clinical event records and prescription records datasets.


  # build dummy clinical events SQLite DB in tempdir
  ukb_db_path <- tempfile(fileext = ".db")

  make_clinical_events_db(ukb_main_path = ukb_main_path,
                          ukb_db_path = ukb_db_path,
                          gp_clinical_path = get_ukb_dummy("dummy_gp_clinical.txt",
                                                           path_only = TRUE),
                          gp_scripts_path = get_ukb_dummy("dummy_gp_scripts.txt",
                                                          path_only = TRUE),
                          ukb_data_dict = ukb_data_dict,
                          ukb_codings = ukb_codings)



  # Connect to the database
  con <- DBI::dbConnect(RSQLite::SQLite(),
                        ukb_db_path)

  # Convert to a named list of dbplyr::tbl_dbi objects
  ukbdb <- ukbwranglr::db_tables_to_list(con)

  # Value columns (from `gp_clinical.txt`) and prescription names/quantities (from `gp_scripts.txt`) are stored separately from the main `clinical_events` table
  ukbdb



  # extract phenotypes
  diabetes_cases <-
    extract_phenotypes(clinical_events = ukbdb$clinical_events,
                       clinical_codes = example_clinical_codes(),
                       verbose = FALSE)

  diabetes_cases




}





