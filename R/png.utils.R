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






#' @export png.demographic_vars
png.demographic_vars <- function(type=c("fnum", "descriptive")){

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
      "qualifications_f6138_0_0",

      "waist_circumference_f48_0_0",
      "hip_circumference_f49_0_0",
      "standing_height_f50_0_0",
      "seated_height_f51_0_0",
      "height_f12144_2_0",
      "weight_f23098_0_0",
      "weight_f21002_2_0",
      "weight_manual_entry_f3160_0_0",
      "body_mass_index_bmi_f21001_0_0",
      "body_mass_index_bmi_f23104_0_0",
      "body_fat_percentage_f23099_0_0",

      "date_of_death_f40000_0_0",
      "underlying_primary_cause_of_death_icd10_f40001_0_0",
      "age_at_death_f40007_0_0",
      "description_of_cause_of_death_f40010_0_0",

      "pack_years_of_smoking_f20161_0_0",
      "smoking_status_f20116_0_0",
      "ever_smoked_f20160_0_0",
      "pack_years_adult_smoking_as_proportion_of_life_span_exposed_to_smoking_f20162_0_0",

      "alcohol_drinker_status_f20117_0_0",
      "alcohol_intake_frequency_f1558_0_0",
      "alcohol_usually_taken_with_meals_f1618_0_0",
      "alcohol_intake_versus_10_years_previously_f1628_0_0",

      "current_tobacco_smoking_f1239_0_0",
      "past_tobacco_smoking_f1249_0_0",
      "smoking_smokers_in_household_f1259_0_0",
      "exposure_to_tobacco_smoke_at_home_f1269_0_0",
      "exposure_to_tobacco_smoke_outside_home_f1279_0_0",
      "maternal_smoking_around_birth_f1787_0_0",
      "age_started_smoking_in_former_smokers_f2867_0_0",
      "type_of_tobacco_previously_smoked_f2877_0_0",
      "number_of_cigarettes_previously_smoked_daily_f2887_0_0",
      "age_stopped_smoking_f2897_0_0",
      "ever_stopped_smoking_for_6_plus_months_f2907_0_0",
      "age_started_smoking_in_current_smokers_f3436_0_0",
      "number_of_cigarettes_currently_smoked_daily_current_cigarette_smokers_f3456_0_0",


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
      "operation_year_age_first_occurred_f92_0_0",

      "genotype_measurement_batch_f22000_0_0",
      "heterozygosity_f22003_0_0",
      "heterozygosity_pca_corrected_f22004_0_0",
      "missingness_f22005_0_0",
      'affymetrix_quality_control_metric_\"cluster_cr\"_f22025_0_0',
      'affymetrix_quality_control_metric_\"dqc\"_f22026_0_0',
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
         "descriptive" = c("f.eid", demographic_vars_descriptive)
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
