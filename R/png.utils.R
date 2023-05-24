#'
#'
#' @export
`%_%` <- function(x, y) {
      paste0(x, y)
}



#' @import assertive
#' @importFrom magrittr `%>%`
#' @importFrom tidyr separate
#' @export split.ukb_dict
split.ukb_dict <- function(ukb_dict, ...){
  standard_message <- "."


  assertthat::assert_that(is.data.frame(ukb_dict),
                          msg = paste0(
                            "Error! `ukb_dict` must be a data frame",
                            standard_message
                          )
  )

  path_level_max <- strsplit(ukb_dict$Path, " > ") %>% map_int(~length(.x)) %>% max
  ukb_dict_split <- ukb_dict  %>%
    tidyr::separate(Path,
                    into = paste0('path', 1:path_level_max),
                    sep = " > ")

  ukb_dict_split %>% select(starts_with("path")) %>% print

  # class(ukb_dict_split) <- c(class(ukb_dict_split), "ukb_dict_split")
  ukb_dict_split

}



#' @export png.dict2path
png.dict2path <- function(ukb_data_dict){

  standard_message <- "."
  assertthat::assert_that(is.data.frame(ukb_data_dict),
                          msg = paste0(
                            "Error! `ukb_data_dict` must be a data frame",
                            standard_message
                          )
  )

  path_level_max <- strsplit(ukb_data_dict$Path, " > ") %>% map_int(~length(.x)) %>% max
  ukb_data_dict  %>%
    tidyr::separate(Path,
                    into = paste0('path', 1:path_level_max),
                    sep = " > ") %>%
    select(starts_with("path"), everything())
}

#' @export png.path2dict
png.path2dict <- function(ukb_data_dict_path){
  path_level_max <- sum(grepl("path", colnames(ukb_data_dict_path)))

  ukb_data_dict_path %>%
    unite("Path", paste0('path', 1:path_level_max), sep=" > ") %>%
    mutate(Path = gsub(" > NA", "", Path) %>% {ifelse(.=="NA", NA, .)} ) %>%
    select(Path, everything())
}





#' @export png.path.NestOverN
png.path.NestOverN <- function(PATH, N=2000){
  # PATH <- ukb_data_dict_path %>% select(path1:path5) %>% filter(!is.na(path1))

  path <- PATH[,1,drop=T] %>% unlist
  for( i in 1:ncol(PATH) ){
    tab <- table(path)

    if( any(tab > N) ){
      path.ip1 <- apply(PATH[,1:(i+1)], 1, function(x) paste0(x, collapse=" > "))
      path.ip1.over2000 <- path.ip1[ path %in% names(tab)[tab>N] ]
      path[path %in% names(tab)[tab>N]] <- path.ip1.over2000
    } else {
      break
    }

  }

  return(path)

}



#' @export png.ukb_QC
png.ukb_QC <- function(df_sample, df_SNP){
  # https://2cjenn.github.io/PRS_Pipeline/
  
  ref <- c("Collister, J. A., Liu, X., & Clifton, L. (2022). Calculating polygenic risk scores (PRS) in UK Biobank: a practical guide for epidemiologists. Frontiers in Genetics, 13, 105.")
  
  # Sample QC -----
  df_sample %>% 
    filter(is.na(sex_chromosome_aneuploidy_f22019_0_0), # != 1,
           sex_f31_0_0 == genetic_sex_f22001_0_0, # sex_discordance
           is.na(outliers_for_heterozygosity_or_missing_rate_f22027_0_0), # != 1,
           # missing_rate_in_autosome < 0.02,
           used_in_genetic_principal_components_f22020_0_0 == 1 # relatedness >= 3
    )
  

  # SNP QC -------
  df_SNP %>% 
    filter(imputation_information >= 0.4,
           MAF >= 0.005,
           MAF < 0.4, # remove high_heterozygosity
           call_rate > 0.9,
           HWE_pvalue > 1e-7
           )
  
  # Bycroft, C., Freeman, C., Petkova, D., Band, G., Elliott, L. T., Sharp, K., et al. (2018). The UK Biobank Resource with Deep Phenotyping and Genomic Data. Nature 562 (7726), 203–209. doi:10.1038/s41586-018-0579-z
  
  # Liu, D. J., Peloso, G. M., Yu, H., Butterworth, A. S., Wang, X., Mahajan, A., et al. (2017). Exome-wide Association Study of Plasma Lipids in >300,000 Individuals. Nat. Genet. 49 (12), 1758–1766. doi:10.1038/ng.3977
}



#' @export png.var.genetics
png.var.genetics <- function(type="descriptive"){
  
  c("sex_f31_0_0",
    "genotype_measurement_batch_f22000_0_0",
    "genetic_sex_f22001_0_0",
    "heterozygosity_f22003_0_0",
    "heterozygosity_pca_corrected_f22004_0_0",
    "missingness_f22005_0_0",
    "genetic_ethnic_grouping_f22006_0_0",
    "genotype_measurement_plate_f22007_0_0",
    "genotype_measurement_well_f22008_0_0",
    "sex_chromosome_aneuploidy_f22019_0_0",
    "used_in_genetic_principal_components_f22020_0_0",
    "genetic_kinship_to_other_participants_f22021_0_0",
    "dna_concentration_f22024_0_0",
    'affymetrix_quality_control_metric_cluster_cr_f22025_0_0',
    'affymetrix_quality_control_metric_dqc_f22026_0_0', 
    "outliers_for_heterozygosity_or_missing_rate_f22027_0_0",
    "genetic_principal_components_f22009_0_1")
  
}


#' @export png.var.disease
png.var.disease <- function(ukb_data_dict, regexpr=c("diagno", "icd", "death"), type="descriptive"){
  
  out <- map(regexpr, ~ ukb_data_dict$descriptive_colnames %>% {.[grep(.x, .)]} ) %>% unlist %>% unique
  
  
  # ClinicalOutcomes <- 
  #   c(ukb_data_dict$descriptive_colnames %>% {.[grep("primary", .)]},
  #     ukb_data_dict$descriptive_colnames %>% {.[grep("disorder", .)]},
  #     ukb_data_dict$descriptive_colnames %>% {.[grep("self_report", .)]},
  #     ukb_data_dict$descriptive_colnames %>% {.[grep("cancer", .)]},
  #     ukb_data_dict$descriptive_colnames %>% {.[grep("death", .)]},
  #     ukb_data_dict$descriptive_colnames %>% {.[grep("diagno", .)]},
  #     ukb_data_dict$descriptive_colnames %>% {.[grep("icd", .)]}) %>% 
  #   unique()
  
  
  var.death <- c(
    "primary_death_icd10",
    "secondary_death_icd10"
  )
  
  var.cancer <- c(
    "cancer_register_icd9",
    "cancer_register_icd10"
  )
  
  var.summary <- c(
    "summary_hes_icd9",
    "summary_hes_icd10",
    "summary_hes_opcs3",
    "summary_hes_opcs4"
  )
  
  var.self <- c(
    "self_report_medication",
    "self_report_non_cancer",
    "self_report_cancer",
    "self_report_operation",
    "self_report_non_cancer_icd10"
  )
  
  # c(var.death, var.cancer, var.summary, var.self)
  
  unique( c("eid", out) )
}



#' @export png.var.demographics
png.var.demographics <- function(type="descriptive"){

  demographic_vars_descriptive <-
    c("sex_f31_0_0",
      "genetic_sex_f22001_0_0",
      "year_of_birth_f34_0_0",
      "month_of_birth_f52_0_0",
      "age_at_recruitment_f21022_0_0",
      "age_when_attended_assessment_centre_f21003_0_0",
      "date_of_attending_assessment_centre_f53_0_0",
      "date_lost_to_follow_up_f191_0_0",
      "reason_lost_to_follow_up_f190_0_0",

      "uk_biobank_assessment_centre_f54_0_0",
      "country_of_birth_uk_elsewhere_f1647_0_0",

      "ethnic_background_f21000_0_0",
      "genetic_ethnic_grouping_f22006_0_0",
      "year_immigrated_to_uk_united_kingdom_f3659_0_0",

      "pregnant_f3140_0_0",

      "waist_circumference_f48_0_0",
      "hip_circumference_f49_0_0",
      "standing_height_f50_0_0",
      "seated_height_f51_0_0",
      "weight_f21002_0_0",
      "body_mass_index_bmi_f21001_0_0",
      "body_fat_percentage_f23099_0_0",

      "date_of_death_f40000_0_0",
      "underlying_primary_cause_of_death_icd10_f40001_0_0",
      "age_at_death_f40007_0_0",
      "description_of_cause_of_death_f40010_0_0",

      "alcohol_drinker_status_f20117_0_0",
      "alcohol_intake_frequency_f1558_0_0",
      "alcohol_intake_versus_10_years_previously_f1628_0_0",
      "alcohol_usually_taken_with_meals_f1618_0_0",
      
      "current_tobacco_smoking_f1239_0_0",
      "smoking_status_f20116_0_0",
      "ever_smoked_f20160_0_0",
      "maternal_smoking_around_birth_f1787_0_0",
      "exposure_to_tobacco_smoke_at_home_f1269_0_0",
      "exposure_to_tobacco_smoke_outside_home_f1279_0_0",
      "past_tobacco_smoking_f1249_0_0",
      "smoking_smokers_in_household_f1259_0_0",
      
      "number_of_days_week_walked_10_plus_minutes_f864_0_0",
      "duration_of_walks_f874_0_0",
      "number_of_days_week_of_moderate_physical_activity_10_plus_minutes_f884_0_0",
      "duration_of_moderate_activity_f894_0_0",
      "number_of_days_week_of_vigorous_physical_activity_10_plus_minutes_f904_0_0",
      "duration_of_vigorous_activity_f914_0_0",


      "part_of_a_multiple_birth_f1777_0_0",
      "fathers_age_f2946_0_0",
      "father_still_alive_f1797_0_0",
      "fathers_age_at_death_f1807_0_0",
      "mothers_age_f1845_0_0",
      "mother_still_alive_f1835_0_0",
      "mothers_age_at_death_f3526_0_0",

      "systolic_blood_pressure_manual_reading_f93_0_0",
      "diastolic_blood_pressure_manual_reading_f94_0_0",
      "diastolic_blood_pressure_automated_reading_f4079_0_0",
      "systolic_blood_pressure_automated_reading_f4080_0_0",

      "number_of_self_reported_cancers_f134_0_0",
      "number_of_self_reported_non_cancer_illnesses_f135_0_0",
      "number_of_operations_self_reported_f136_0_0",
      "number_of_treatments_medications_taken_f137_0_0",
      "cancer_year_age_first_occurred_f84_0_0",
      "non_cancer_illness_year_age_first_occurred_f87_0_0",

      "genotype_measurement_batch_f22000_0_0",
      "heterozygosity_f22003_0_0",
      "heterozygosity_pca_corrected_f22004_0_0",
      "missingness_f22005_0_0",
      'affymetrix_quality_control_metric_cluster_cr_f22025_0_0',
      'affymetrix_quality_control_metric_dqc_f22026_0_0',
      "genotype_measurement_plate_f22007_0_0",
      "genotype_measurement_well_f22008_0_0",
      "outliers_for_heterozygosity_or_missing_rate_f22027_0_0",
      "genetic_principal_components_f22009_0_1" )

  demographic_vars_fnum <-
    c("f.31",
      "f.22001",
      "f.34",
      "f.52",
      "f.21022",
      "f.21003",
      "f.53",
      "f.191",
      "f.190",

      "f.54",
      "f.1647",

      "f.21000",
      "f.22006",
      "f.3659",

      "f.3140",
      "f.6138",

      "f.48",
      "f.49",
      "f.50",
      "f.51",
      "f.12144",
      "f.23098",
      "f.21002",
      "f.3160",
      "f.21001",
      "f.23104",
      "f.23099",

      "f.40000",
      "f.40001",
      "f.40007",
      "f.40010",

      "f.20161",
      "f.20116",
      "f.20160",
      "f.20162",

      "f.20117",
      "f.1558",
      "f.1618",
      "f.1628",

      "f.1239",
      "f.1249",
      "f.1259",
      "f.1269",
      "f.1279",
      "f.1787",
      "f.2867",
      "f.2877",
      "f.2887",
      "f.2897",
      "f.2907",
      "f.3436",
      "f.3456",


      "f.864",
      "f.874",
      "f.884",
      "f.894",
      "f.904",
      "f.914",


      "f.1777",
      "f.2946",
      "f.1797",
      "f.1807",
      "f.1845",
      "f.1835",
      "f.3526",

      "f.93",
      "f.94",
      "f.4079",
      "f.4080",

      "f.134",
      "f.135",
      "f.136",
      "f.137",
      "f.84",
      "f.87",
      "f.92",

      "f.22000",
      "f.22003",
      "f.22004",
      "f.22005",
      'f.22025',
      'f.22026',
      "f.22007",
      "f.22008",
      "f.22027",
      "f.22009" )


  # demographic_vars_fnum2 <- map_chr(demographic_vars_descriptive, ~gsub(".*_f(\\d+)_.*", "f.\\1", .x))


  switch(type,
         "fnum" = c("f.eid", demographic_vars_fnum),
         "descriptive" = c("eid", demographic_vars_descriptive)
         )

}






#' @importFrom dplyr filter select select_if
#' @importFrom magrittr "%>%"
#' @importFrom tibble as_tibble
#' @export
ukb_icd_diagnosis <- function(data, id, icd.version = NULL) {
  # ukb_icd_diagnosis(my_ukb_data, id = "1", icd.version = 10)

  if (!all(id %in% data$eid)) {
    stop(
      "Invalid UKB sample id. Check all ids are included in the supplied data",
      call. = FALSE
    )
  }

  if (!is.null(icd.version) && !(icd.version %in% 9:10)) {
    stop(
      "`icd.version` is an invalid ICD revision number.
      Enter 9 for ICD9, or 10 for ICD10",
      call. = FALSE
    )
  }

  icd <- if (icd.version == 9) {
    ukbtools::icd9codes
  } else if (icd.version == 10){
    ukbtools::icd10codes
  }

  individual_codes <- data %>%
    dplyr::filter(eid %in% id) %>%
    dplyr::select(matches(paste("^diagnoses.*icd", icd.version, sep = ""))) %>%
    dplyr::select_if(colSums(!is.na(.)) > 0) %>%
    t() %>%
    tibble::as_tibble()

  colnames(individual_codes) <- id

  if(ncol(individual_codes) == 1 & sum(!is.na(individual_codes[[1]])) < 1) {
    message(paste("ID", id, "has no ICD", icd.version, "diagnoses", sep = " "))
  } else {

    d <- individual_codes %>%
      purrr::map(~ ukb_icd_code_meaning(c(.), icd.version)) %>%
      dplyr::bind_rows(.id = "sample")

    no_icd <- id[!(id %in% unique(d$sample))]
    if(length(no_icd) > 0) message("ID(s) ", paste(no_icd, " "), "have no ICD ", icd.version, " diagnoses.")

    return(d)

  }
}




#' @importFrom dplyr filter
#' @importFrom magrittr "%>%"
#' @export
ukb_icd_code_meaning <- function(icd.code, icd.version = 10) {
  # ukb_icd_code_meaning(icd.code = "I74", icd.version = 10)

  icd <- if (icd.version == 9) {
    ukbtools::icd9codes
  } else if (icd.version == 10){
    ukbtools::icd10codes
  }

  if(is.name(substitute(icd.code))) {
    char_code <- deparse(substitute(icd.code))
    icd %>%
      dplyr::filter(code %in% char_code)
  } else if (is.character(icd.code)){
    icd %>%
      dplyr::filter(code %in% icd.code)
  }
}





#' @importFrom dplyr filter
#' @importFrom magrittr "%>%"
#' @export
ukb_icd_keyword <- function(description, icd.version = 10, ignore.case = TRUE) {
  # ukb_icd_keyword("cardio", icd.version = 10)


  icd <- if (icd.version == 9) {
    ukbtools::icd9codes
  } else if (icd.version == 10){
    ukbtools::icd10codes
  }

  icd %>%
    dplyr::filter(grepl(paste(description, collapse = "|"), .$meaning,
                        perl = TRUE, ignore.case = ignore.case))
}








#' @importFrom dplyr select
#' @importFrom magrittr "%>%"
#' @importFrom purrr map_df
#' @export
ukb_icd_prevalence <- function(data, icd.code, icd.version = 10) {
  # ukb_icd_prevalence(my_ukb_data, icd.code = "C|D[0-4].")

  ukb_case <- data %>%
    dplyr::select(matches(paste("^diagnoses.*icd", icd.version, sep = ""))) %>%
    purrr::map_df(~ grepl(icd.code, ., perl = TRUE)) %>%
    rowSums() > 0

  sum(ukb_case, na.rm = TRUE) / length(ukb_case)
}

























#' Create an interactive plotly from count data
#'
#' @description
#' These functions help you quickly create interactive hierarchical plots
#' from categorical data. They expect the summary of the data created by
#' `dplyr::count()` and produce either a sunburst plot (`count_to_sunburst()`) or
#' a treemap plot (`count_to_treemap()`)
#'
#' @param count_data An output of dplyr::count(), tibble or data frame
#' @param fill_by_n If TRUE, uses a continuous scale to fill plot by group size
#' @param sort_by_n If TRUE, sorts groups in plot by size, if FALSE sorts them alphabetically
#'
#' @export
#' @examples
#' library(dplyr)
#' starwars_count <- count(starwars, species, eye_color, name)
#'
#' # sunburst plot
#' count_to_sunburst(starwars_count)
#'
#' # fill by group size
#' count_to_sunburst(starwars_count, fill_by_n = TRUE)
#'
#' # treemap plot, ordered by group size
#' count_to_treemap(starwars_count, sort_by_n = TRUE)
#'
#' # display al charchaters by homeworld
#' starwars %>%
#'   count(homeworld, name) %>%
#'   count_to_treemap(sort_by_n = TRUE)
count_to_sunburst <- function(count_data, fill_by_n = FALSE, sort_by_n = FALSE){

  params <- create_all_col_params(count_data, fill_by_n, sort_by_n) %>% na.omit()

  purrr::exec(plotly::plot_ly,
              !!!params,
              type = "sunburst",
              branchvalues = "total",
              hoverinfo = "text")
}


#' @export
#' @rdname count_to_sunburst
count_to_treemap <- function(count_data, fill_by_n = FALSE, sort_by_n = FALSE){

  params <- create_all_col_params(count_data, fill_by_n, sort_by_n) %>% na.omit()

  purrr::exec(plotly::plot_ly,
              !!!params,
              type = "treemap",
              branchvalues = "total",
              hoverinfo = "text")
}


create_all_col_params <- function(count_data, fill_by_n, sort_by_n){

  assert_count_df(count_data)
  assertthat::assert_that(is.logical(fill_by_n),
                          length(fill_by_n) == 1,
                          msg = "fill_by_n must be either TRUE or FALSE")
  assertthat::assert_that(is.logical(sort_by_n),
                          length(sort_by_n) == 1,
                          msg = "sort_by_n must be either TRUE or FALSE")

  count_data <- all_non_n_cols_to_char(count_data)

  category_num <- ncol(count_data) - 1

  params <- purrr::map(1:category_num,
                       create_one_col_params,
                       df = count_data,
                       root = "") %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(sort = sort_by_n)

  if(fill_by_n){
    params <- params %>%
      dplyr::mutate(marker = list(
        colorbar = list(
          bgcolor = ""
        )
      ))
  }
  params
}

create_one_col_params <- function(df,
                                  col_num,
                                  root){
  col_name <- names(df)[col_num]

  df %>%
    dplyr::group_by(dplyr::across(1:dplyr::all_of(col_num))) %>%
    dplyr::summarise(values = sum(.data$n), .groups = "drop") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ids = paste(dplyr::c_across(1: !!col_num),
                  collapse = ".->."),
      parents = ifelse(!!col_num > 1,
                       paste(dplyr::c_across(1 :(!!col_num - 1)),
                             collapse = ".->."),
                       root)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(labels = .[[!!col_num]],
                  hovertext = stringr::str_glue(
                    "column: {col_name}\nvalue: {labels}\nn: {values}")
    ) %>%
    dplyr::select(ids, parents, labels, values, hovertext)
}







assert_count_df <- function(var){
  msg <- paste(substitute(var), "must be a count dataframe (output of dplyr::count)")
  assertthat::assert_that(is.data.frame(var),
                          assertthat::has_name(var, "n"),
                          msg = msg)

  n_col <- var$n
  assertthat::assert_that(is.numeric(n_col), msg = msg)
}

all_non_n_cols_to_char <- function(df){
  df %>%
    dplyr::mutate(dplyr::across(!matches("^n$"), as.character))
}




















png_indicate_coltype_in_data_dict <- function(ukb_data_dict,
                                          ukb_codings) {
  # helper for `read_ukb*`

  # codings in ukb_codings that can be read as integers
  ukb_codings_coercible_to_integer <- ukb_codings %>%
    dplyr::mutate("Value_coercible_to_integer" = dplyr::case_when(
      !is.na(suppressWarnings(as.integer(.data[["Value"]]))) ~ TRUE,
      is.na(suppressWarnings(as.integer(.data[["Value"]]))) ~ FALSE
    )) %>%
    dplyr::group_by(.data[["Coding"]]) %>%
    # identify codings where *all* values are coercible to integer
    dplyr::summarise("coercible_to_integer" = dplyr::case_when(
      mean(.data[["Value_coercible_to_integer"]]) == 1 ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    dplyr::filter(.data[["coercible_to_integer"]]) %>%
    dplyr::pull(.data[["Coding"]])

  # add column to ukb_data_dict indicating column type, and column indicating
  # whether the coded value is coercible to integer (as identified above)
  ukb_data_dict <- ukb_data_dict %>%
    dplyr::mutate("col_types_readr" = dplyr::case_when(
      ((.data[["ValueType"]] == "Integer") |
         (.data[["FieldID"]] == "eid")
      ) ~ "i",
      # Default is type character
      TRUE ~ "c"
    )) %>%
    # ValueType 'Continuous' overrides the above
    dplyr::mutate(
      "col_types_readr" = dplyr::case_when(
        .data[["ValueType"]] == "Continuous" ~ "d",
        .data[["ValueType"]] == "Date" ~ "c",
        TRUE ~ .data[["col_types_readr"]]
      )
    ) %>%
    dplyr::mutate(
      "col_types_fread" = dplyr::case_when(
        .data[["col_types_readr"]] == "i" ~ "integer",
        .data[["col_types_readr"]] == "d" ~ "double",
        .data[["col_types_readr"]] == "D" ~ "character",
        .data[["col_types_readr"]] == "c" ~ "character"
      )
    ) %>%
    dplyr::mutate(
      "coercible_to_integer" = dplyr::case_when(
        .data[["Coding"]] %in% !!ukb_codings_coercible_to_integer ~ "Yes",
        TRUE ~ "No"
      )
    )

  return(ukb_data_dict)
}



#' @export png.df2coding
png.df2coding <- function(df,
         ukb_data_dict,
         codings,
         data_dict_coding_col = "Coding",
         codings_coding_col = "Coding",
         data_dict_colname_col = "descriptive_colnames",
         data_dict_variable_label_col = "Variable_label",
         data_dict_coltype_col = "ValueType",
         codings_value_col = "Value",
         codings_meaning_col = "Meaning",
         colnames_col = "descriptive_colnames") {
  if(FALSE){
    # df
    # ukb_data_dict
    codings=ukb_coding
    data_dict_coding_col = "Coding"
    codings_coding_col = "Coding"
    data_dict_colname_col = "descriptive_colnames"
    data_dict_variable_label_col = "Variable_label"
    data_dict_coltype_col = "ValueType"
    codings_value_col = "Value"
    codings_meaning_col = "Meaning"
    colnames_col = "descriptive_colnames"
  }


  ukb_data_dict <- ukb_data_dict %>% filter(descriptive_colnames %in% colnames(df))


  ukb_data_dict <- png_indicate_coltype_in_data_dict(
    ukb_data_dict = ukb_data_dict,
    ukb_codings = codings
  )

  ukb_data_dict$Coding


  ukb_data_dict[["Variable_label"]] <- ifelse(
    ukb_data_dict[["descriptive_colnames"]] == "eid",
    yes = "eid",
    no = paste0(
      ukb_data_dict$Field,
      " (",
      ukb_data_dict$colheaders_processed,
      ")"
    )
  )


  # convert ukb_data_dict and codings to named lists
  data_dict_list <-
    split(ukb_data_dict, ukb_data_dict[[data_dict_coding_col]])

  codings_list <- split(codings, codings[[codings_coding_col]])

  # all codings to label
  all_codings <- intersect( names(codings_list), names(data_dict_list) )


  # integer/double codings - note that some continuous variables have coded
  # values (e.g. FID 20006, interpolated year when cancer first diagnosed)
  numeric_codings <- ukb_data_dict %>%
    dplyr::filter(.data[[data_dict_coltype_col]] %in% c("Integer", "Continuous")) %>%
    dplyr::filter(!is.na(.data[[data_dict_coding_col]])) %>%
    dplyr::pull(.data[[data_dict_coding_col]]) %>%
    as.integer()

  non_coded_columns_to_label <- ukb_data_dict %>%
    dplyr::filter(is.na(.data[[data_dict_coding_col]])) %>%
    dplyr::pull(.data[[data_dict_colname_col]])

  # progress bar - one tick per column
  pb <- progress::progress_bar$new(
    format = "[:bar] :current/:total (:percent)",
    total = nrow(ukb_data_dict)
  )
  pb$tick(0)

  # loop through codings
  for (single_coding in all_codings) {
    # colnames using this coding
    columns_to_label <-
      data_dict_list[[single_coding]][[data_dict_colname_col]]

    codings_single <- codings_list[[single_coding]]

    # order by value numerically if values are coercible to integer
    if ( (unique(data_dict_list[[single_coding]]$coercible_to_integer)=="Yes")  |
         (single_coding %in% numeric_codings) ) {
      codings_single <- codings_single %>%
        dplyr::arrange(as.numeric(.data[["Value"]]))
    }

    # label variables (all) and values (type chracter)
    for (column in columns_to_label) {
      # progress bar
      pb$tick(1)

      variable_label <- ukb_data_dict[ukb_data_dict[[data_dict_colname_col]] == column, data_dict_variable_label_col][[1]]

      # checks
      assertthat::assert_that(!is.null(df[[column]]),
                              msg = paste0("Error while labelling columns: column ", column, " does not exist. Try checking data dictionary")
      )


      # if (is.character(df[[column]])) {
        df[[column]] <- factor(df[[column]],
                               levels = codings_single[[codings_value_col]],
                               labels = codings_single[[codings_meaning_col]]
        )
      # }

      if (is.na(variable_label)) {
        variable_label <- NULL
      }

      # attributes(df[[column]])$label <- variable_label
    }
  }

  # label remaining variables (i.e. those without associated codings/value labels)
  for (column in non_coded_columns_to_label) {
    pb$tick(1)

    # for debugging
    # print(column)
    # attributes(df[[column]])$label <- ukb_data_dict[ukb_data_dict[[data_dict_colname_col]] == column, data_dict_variable_label_col][[1]]
  }

  # error if nothing was labelled
  assertthat::assert_that(!rlang::is_empty(all_codings) | !rlang::is_empty(non_coded_columns_to_label),
                          msg = "No value or variable labels were applied"
  )

  return(df)
}













#' @export png.ukb_read
png.ukb_read <- function(path, vars, ukb_data_dict, ukb_path, type=c("descriptive", "fid"), exact=FALSE, exclude=png.sample.exclude(), ...){
  # {
  #   path <- "/Volumes/png1/2.UKB/UKB_ClinicalData_descriptive"
  #   vars <- c("f.131286.0.0","f.131338.0.0","f.131350.0.0", "f.23104.0.0")
  #   vars <- c("standing_height_f50_0_0",
  #             "height_f12144_2_0",
  #             "age_at_death_f40007_0_0",
  #             "age_at_death_f40007_1_0",
  #             "weight_f21002_0_0",
  #             "weight_f23098_0_0",
  #             "body_mass_index_bmi_f21001_0_0",
  #             "body_mass_index_bmi_f23104_0_0",
  #             "sex_f31_0_0",
  #             "genetic_sex_f22001_0_0")
  #   type <- c("fid", "descriptive")[2]
  # }
  if( missing(type) ) type <- "descriptive"
  f.sep <- switch(type, "descriptive"="", "fid"=".")
  RegExp <- paste0("f",f.sep,"(\\d+)")


  if(!exact){
    vars <- ifelse( vars %in% c("eid","f.eid","feid"), "eid", stringr::str_extract(vars, RegExp, group=1) )
    wh.vars <- which(stringr::str_extract(ukb_data_dict$colheaders_raw, "f.(\\d+)", group=1) %in% vars)
  } else {
    wh.vars <- which(ukb_data_dict$descriptive_colnames %in% vars)
  }

  

  path.vars <- ukb_path[wh.vars]
  path.vars.unique <- unique(path.vars)
  df_list <- as.list(1:length(path.vars.unique))
  for( i in 1:length(path.vars.unique)){
    pathname <- path.vars.unique[i]
    filename <- paste0(path, "/ukb_data - ", pathname, ".csv")
    if( is.na(pathname) ) next
    wh.var <- wh.vars[path.vars == pathname]
    varnames <- ukb_data_dict %>% slice(wh.var) %>% .$descriptive_colnames

    df_list[[i]] <- data.table::fread(filename, select=c("eid", varnames), ...)
  }

  
  check_equivalence <- Reduce(function(x,y) if (identical(x,y)) x else FALSE, map(df_list, ~.x$eid))
  
  if( check_equivalence[1] ){
    
    out <- dplyr::bind_cols( append(df_list[1], map(df_list[-1], ~.x %>% select(-eid))) )
    # df <- df_list %>% purrr::reduce(left_join, by = "eid") %>% as_tibble()
  } else {
    out <- df_list
  }
  
  
  # library(purrr)
  # df <- df_list %>% purrr::reduce(left_join, by = "eid") %>% as_tibble()
  
  # remove withdrawals (N=43)
  out <- out[!eid %in% exclude]

  return(out)
}




#' @export png.filter_visit
png.filter_visit <- function(df, visit=0, instance="\\d+", type="descriptive"){
  # visit=1; type="descriptive"
  df <- as_tibble(df)
  df[,c(1, grep( paste0("f\\d+_",visit,"_", instance, collapse="|"), colnames(df)))]
}



#' @export png.desc2fid
png.desc2fid <- function(descriptive_vars, simple=FALSE){
  
  if( any(c("f.eid","feid","eid") %in% descriptive_vars) ){
    out <- descriptive_vars
    out <- out[!out %in% c("f.eid","feid","eid")]
    out <- map_chr(out, ~gsub(".*_f(\\d+)_(\\d)_(\\d).*", "f.\\1.\\2.\\3", .x))
    out <- c("f.eid", out)
  } else {
    out <- map_chr(descriptive_vars, ~gsub(".*_f(\\d+)_(\\d)_(\\d).*", "f.\\1.\\2.\\3", .x))
  }
  
  
  if(simple){
    out <- map_chr(out, ~gsub("f.(\\d+).*", "f.\\1", .x))
  }
  out
}





#' @export png.which_path
png.which_path <- function(vars, ukb_data_dict, ukb_path, exact=FALSE, type){
  if(FALSE){
    vars <- "genotype_measurement_plate_f22007_0_0"
    exact=TRUE
    type="descriptive"
  }
  
  if( missing(type) ){
    type <- "descriptive"
  }
  
  f.sep <- switch(type, "descriptive"="", "fid"=".")
  RegExp <- paste0("f",f.sep,"(\\d+)")
  
  if(!exact){
    vars <- ifelse( vars %in% c("eid","f.eid","feid"), "eid", stringr::str_extract(vars, RegExp, group=1) )
    wh.vars <- which(stringr::str_extract(ukb_data_dict$colheaders_raw, "f.(\\d+)", group=1) %in% vars)
  } else {
    wh.vars <- which(ukb_data_dict$descriptive_colnames %in% vars)
  }
  
  
  ukb_path[wh.vars]
}








#' @export png.write_pheno
png.write_pheno <- function (ukb_data, file="./ukb_data.pheno", vars, exact=FALSE, id = "eid", type="plink"){
  if(FALSE){
    ukb_data <- ukb_data_demographics
    id="eid"
    vars=c("genotype_measurement_plate_f22007_0_0", "genetic_principal_components_f22009_0_1")
    file="./ukb_data.pheno"
    na.strings = "NA"
    type="bgenie"
  }
  library(dplyr)
  library(data.table)
  
  na.strings <- switch(type, "plink"="NA", "bgenie"="-999")
  
  # if(!exact){
  #   f.sep <- "" # for descriptive; "." for fid.
  #   RegExp <- paste0("f",f.sep,"(\\d+)")
  #   vars <- ifelse( vars %in% c("eid","f.eid","feid"), "eid", stringr::str_extract(vars, RegExp, group=1) )
  #   wh.vars <- which(stringr::str_extract(ukb_data_dict$colheaders_raw, "f.(\\d+)", group=1) %in% vars)
  # } else {
  #   wh.vars <- which(ukb_data_dict$descriptive_colnames %in% vars)
  # }
  
  if( !exact ){
    vars_final <- map(c(id,vars), ~colnames(ukb_data) %>% {.[grep(.x,.)]}) %>% unlist
    
    vars <- vars_final
  }
  
  
  
  ukb_data2 <- ukb_data[,intersect(colnames(ukb_data), c(id, vars)), with=FALSE] %>% 
    {.[, c("FID","IID") := list(eid,eid)]} %>% 
    {.[,-"eid"]} %>% 
    setcolorder(c("FID", "IID"))
  
  # df_id <- dplyr::transmute_(ukb_data, FID = id, IID = id)
  # df_vars <- dplyr::select_(ukb_data, .dots = vars)
  
  data.table::fwrite( ukb_data2, file = file, sep=" ", na = na.strings, quote=FALSE)
  # readr::write_delim( ukb_data2, path = file, na = na.strings, col_names = TRUE)
}





#' @export png.ukb_read_regexpr
png.ukb_read_regexpr <- function(name, ukb_data_dict, path_split, ukb_path, exclude=png.sample.exclude()){
  vars_selected <- ukb_data_dict %>% 
    filter(grepl(name, descriptive_colnames)) %>% 
    # filter(grepl("f\\d+_0_\\d", descriptive_colnames) ) %>% 
    .[,1] %>% unlist
  
  ukb_data <- png.ukb_read(path=path_split, 
                            vars=vars_selected, 
                            ukb_data_dict=ukb_data_dict, 
                            ukb_path=ukb_path, 
                            exact=TRUE, exclude=exclude)
  
  ukb_data
}






#' @export png.sample.exclude
png.sample.exclude <- function(){
  id.0425 <- c(1012595, 1045711, 1178130, 1486100, 1527419, 1566279, 1736161, 2029918, 2330804, 2332166, 2359968, 2608772, 2706464, 2921349, 3010338, 3230053, 3304352, 3434544, 3449065, 3856220, 4036243, 4071416, 4080610, 4286087, 4337463, 4471834, 4502593, 4585337, 4771799, 4946365, 4962291, 5336552, 5352267, 5427057, 5461024, 5540686, 5606679, 5666673, 5677381, 5791191, 5795508, 5795570, 6024120)
  
  out <- unique( c(id.0425) )
  
  out
}





#' @export png.ukb_init
png.ukb_init <- function(path, filename, path_split){
  
  {
    library(tidyverse)
    library(png.ukb)
    
    # path <- "/Volumes/png1/Yonsei/3. UKB/Clinical/Maindata3/"
    # filename <- "ukb669778"
    # path_split <- "/Volumes/png1/2.UKB/UKB_ClinicalData_descriptive"
    
    ukb_main <- paste0(path, filename, ".tab", collapse="")
    ukb_field <- get_ukb_field(filename, path)
    ukb_dict <- get_ukb_dict()
    ukb_coding <- get_ukb_codings()
    ukb_data_dict <- make_data_dict(ukb_main=ukb_main, ukb_dict=ukb_dict)
    ukb_data_dict_path <- png.dict2path(ukb_data_dict) # split Path to path1:path5
    # New path categories with at most 2000 variables
    ukb_path <- ukb_data_dict_path %>% select(path1:path5) %>% png.path.NestOverN(N=2000)
  }
  
  
  list(path = path, filename = filename, path_split = path_split,
       ukb_main = ukb_main,
       ukb_field = ukb_field,
       ukb_dict = ukb_dict,
       ukb_coding = ukb_coding,
       ukb_data_dict = ukb_data_dict,
       ukb_data_dict_path = ukb_data_dict_path,
       ukb_path = ukb_path)
  
}




