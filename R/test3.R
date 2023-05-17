if(FALSE){
  
  
  # devtools::install_github("statpng/png.ukb")
  # detach("package:png.ukb", unload=TRUE)
  library(png.ukb)
  
  library(tidyverse)
  library(devtools)
  library(dplyr)
  library(tidyr)
  library(tidyselect)
  library(readr)
  library(tibble)
  library(broom)
  
  source("https://raw.githubusercontent.com/statpng/pngfunction/master/tidyverse/functions.R")
  
  
  
  
  
  
  path <- "/Volumes/T7/Yonsei/3. UKB/Clinical/Maindata3/"
  filename <- "ukb669778"
  
  
  
  
  ukb_field <- get_ukb_field(filename, path)
  ukb_dict <- get_ukb_dict()
  ukb_coding <- get_ukb_codings()
  ukb_data_dict <- make_data_dict( ukb_main=path%_%filename%_%".tab",
                                   ukb_dict=ukb_dict)
  
  # split Path to path1:path5
  ukb_dict_path <- png.dict2path(ukb_dict)
  ukb_data_dict_path <- png.dict2path(ukb_data_dict)
  
  # New path categories with at most 2000 variables
  ukb_path <- ukb_data_dict_path %>% select(path1:path5) %>% png.path.NestOverN(N=2000)
  max(table(ukb_path)) < 2000
  
  length(table(ukb_data_dict$Path))
  # [1] 176
  length(table(ukb_path))
  # [1] 55
  
  # Note: variables with missing FieldID
  ukb_data_dict_NA <- ukb_data_dict %>% filter(is.na(Path))
  ukb_data_dict_NA %>% print(n="max") %>% as.matrix %>% .[,1:5]
  
  
  ukb_var_desc <- png.demographic_vars(type="descriptive")
  ukb_var_fid <- png.desc2fid(ukb_var_desc, simple=TRUE)
  
  cbind(ukb_var_desc, ukb_var_fid) %>% head
  cbind(ukb_var_desc, ukb_var_fid) %>% tail
  
  
  
  
  # ukb_path:  This takes a long time
  if(FALSE){
    tab <- table(ukb_path) %>% sort(decreasing=TRUE)
    title <- names(tab)
    
    library(data.table)
    
    for( i in 1:length(tab) ){
      
      wh <- c(1, which( ukb_path == names(tab)[i] ))
      
      ukb_data_dict_filtered <- 
        ukb_data_dict %>% 
        slice(wh)
      
      ukb_data_filtered <- 
        read_ukb( path = path%_%filename%_%".tab",
                  descriptive_colnames = TRUE,
                  label = FALSE, 
                  ukb_data_dict = ukb_data_dict_filtered )
      
      fwrite(ukb_data_filtered, paste0("ukb_data - ", title[i], ".csv") )
      
      gc()
      rm(ukb_data_filtered)
    }
    
  }
  
  
  
  # demographics
  ukb_data_dict_demographics <- ukb_data_dict %>% filter( FieldID %in% gsub("f.", "", ukb_var_fid) )
  ukb_data_demographics <- 
    read_ukb( path = path%_%filename%_%".tab",
              descriptive_colnames = TRUE,
              label = FALSE, 
              ukb_data_dict = ukb_data_dict_demographics )
  
  data.table::fwrite(ukb_data_demographics, paste0("ukb_data_demographics.csv") )
  data.table::fwrite(ukb_data_dict_demographics, paste0("ukb_data_dict_demographics.csv") )
  
  
  
  # diagnoses
  var_diagnoses <- png.var.disease(ukb_data_dict)
  
  ukb_data_diagnoses <- 
    png.ukb_read(path="/Volumes/T7/2.UKB/UKB_ClinicalData_descriptive", 
                 vars=var_diagnoses, 
                 ukb_data_dict=ukb_data_dict, 
                 exact=FALSE)
  
  ukb_data_dict_diagnoses <- 
    ukb_data_dict %>% 
    filter( FieldID %in% gsub( "f.", "", png.desc2fid(var_diagnoses, simple=TRUE) ) )
  
  data.table::fwrite(ukb_data_diagnoses, paste0("ukb_data_diagnoses.csv") )
  data.table::fwrite(ukb_data_dict_diagnoses, paste0("ukb_data_dict_diagnoses.csv") )
  
  
  # clincial outcomes
  
  
  #
  
  ukb_data <- data.table::fread( "./ukb_data_diagnoses.csv" ) %>% as_tibble()
  
  ukb_data %>% dim
  
  
  ukb_icd_diagnosis(ukb_data, id=1000000+c(15, 27, 39, 40), icd.version=10)
  ukb_icd_code_meaning(icd.code = "E10", icd.version = 10)
  ukb_icd_keyword("cardio|atrial", icd.version = 10)
  ukb_icd_prevalence(ukb_data, icd.code="I4[8-9].", icd.version = 10)
  ukb_icd_prevalence(ukb_data, icd.code="E1[0-1]", icd.version = 10)
  
  
  #
  #
  
  
  
  
  var_tmp <- c("date_of_attending_assessment_centre_f53_0_0",
               "age_at_recruitment_f21022_0_0",
               "uk_biobank_assessment_centre_f54_0_0",
               "ethnic_background_f21000_0_0",
               "genetic_ethnic_grouping_f22006_0_0",
               "date_of_death_f40000_0_0",
               "underlying_primary_cause_of_death_icd10_f40001_0_0",
               "standing_height_f50_0_0",
               "height_f12144_2_0",
               "age_at_death_f40007_0_0",
               "age_at_death_f40007_1_0",
               "weight_f21002_0_0",
               "weight_f23098_0_0",
               "body_mass_index_bmi_f21001_0_0",
               "body_mass_index_bmi_f23104_0_0",
               "sex_f31_0_0",
               "genetic_sex_f22001_0_0",
               "date_i10_first_reported_essential_primary_hypertension_f131286_0_0",
               "date_i42_first_reported_cardiomyopathy_f131338_0_0",
               "date_i48_first_reported_atrial_fibrillation_and_flutter_f131350_0_0",
               "source_of_report_of_i48_atrial_fibrillation_and_flutter_f131351_0_0")
  
  
  ukb_data <- png.ukb_read(path="/Volumes/T7/2.UKB/UKB ClinicalData - descriptive", vars=var_tmp, ukb_data_dict=ukb_data_dict, exact=FALSE)
  ukb_data_visit0 <- png.filter_visit(ukb_data, visit=0)
  ukb_data_visit0_coding <- png.df2coding(ukb_data_visit0, ukb_data_dict, ukb_coding)
  
  ukb_data_visit0_coding %>% elucidate::describe_na_all()
  ukb_data_visit0_coding %>% elucidate::describe_all(class = c("d","f","c","l","n")[1] )
  ukb_data_visit0_coding %>% elucidate::describe_all(class = "all" )
  
  
  
  
  
  # package: elucidate ---------------------------------------------------------------
  
  library(elucidate)
  
  
  df <- png.filter_visit(ukb_data, visit=0:4) %>% png.df2coding(ukb_data_dict, ukb_coding)
  
  
  df %>% select(standing_height_f50_0_0, height_f12144_2_0) %>% elucidate::describe_all()
  df %>% select(age_at_death_f40007_0_0, age_at_death_f40007_1_0) %>% elucidate::describe_all()
  df %>% select(weight_f21002_0_0, weight_f23098_0_0) %>% elucidate::describe_all()
  
  df %>% select(standing_height_f50_0_0, height_f12144_2_0) %>% cor(., use = "complete.obs")
  df %>% select(weight_f21002_0_0, weight_f23098_0_0) %>% cor(., use = "complete.obs")
  df %>% select(age_at_death_f40007_0_0, age_at_death_f40007_1_0) %>% cor(., use = "complete.obs")
  df %>% select(body_mass_index_bmi_f21001_0_0, body_mass_index_bmi_f23104_0_0) %>% cor(., use = "complete.obs")
  
  
  df %>% select(sex_f31_0_0, genetic_sex_f22001_0_0) %>% table()
  
  
  
  
  # Genomic data ------------------------------------------------------------
  
  var_genetics <- c("genotype_measurement_batch_f22000_0_0",
                    "heterozygosity_f22003_0_0",
                    "heterozygosity_pca_corrected_f22004_0_0",
                    "missingness_f22005_0_0",
                    'affymetrix_quality_control_metric_cluster_cr_f22025_0_0',
                    'affymetrix_quality_control_metric_dqc_f22026_0_0', 
                    "genotype_measurement_plate_f22007_0_0",
                    "genotype_measurement_well_f22008_0_0",
                    "outliers_for_heterozygosity_or_missing_rate_f22027_0_0",
                    "genetic_principal_components_f22009_0_1")
  
  
  
  
  ukb_data <- png.ukb_read(path="/Volumes/T7/2.UKB/UKB ClinicalData - descriptive", vars=var_genetics, ukb_data_dict=ukb_data_dict, exact=FALSE)
  ukb_data_visit0 <- png.filter_visit(ukb_data, visit=0)
  ukb_data_visit0_coding <- png.df2coding(ukb_data_visit0, ukb_data_dict, ukb_coding)
  
  ukb_data_visit0_coding %>% head %>% as.matrix
  png.ukb_read()
  
  
  
  
  {
    smp_list <- list.files("/Volumes/T7/2.UKB/Genetics/sample/", full.names = TRUE) %>% 
      map(~read.csv(.x, sep=" ", skip=2, header=FALSE))
    smp.ID_list <- unique(unlist(lapply(smp_list, "[", 1)))
    
    smp_list[[1]][,1:2][482560:482570,]
    smp_list[[1]] %>% {.[.[,1]<0,1]}
    smp_list[[2]] %>% {.[.[,1]<0,1]}
    
    table(smp.ID_list<0)
    # FALSE   TRUE 
    # 487181  244 
    
    intersect(df0$eid, smp.ID_list) %>% length
    # [1] 487181
    
    
    
    
    
    purrr::reduce(function(x) x[,1], smp_list)
    do.call(c, smp_list)
    
    smp_list %>% purrr::reduce(function(x, y) (c(x[1],y[1]))) -> tmp
    unique(unlist(tmp)) %>% length
    
    unique(unlist(tmp)) %>% {.[.>0]} %>% length
    
    smp_list[[1]] %>% dim
    
  }
  
  
  
  
  
  
  
  
  
}