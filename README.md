```{r}
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
```




```{r}
path <- "/Volumes/T7/Yonsei/3. UKB/Clinical/Maindata3/"
filename <- "ukb669778"
```



```{r}
ukb_field <- get_ukb_field(filename, path)
ukb_dict <- get_ukb_dict()
ukb_coding <- get_ukb_codings()
ukb_data_dict <- make_data_dict( ukb_main=path%_%filename%_%".tab",
                                 ukb_dict=ukb_dict)
```


```{r}
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
```



```{r}
# Note: variables with missing FieldID
ukb_data_dict_NA <- ukb_data_dict %>% filter(is.na(Path))
ukb_data_dict_NA %>% print(n="max") %>% as.matrix %>% .[,1:5]


ukb_var_desc <- png.demographic_vars(type="descriptive")
ukb_var_fid <- png.desc2fid(ukb_var_desc, simple=TRUE)

cbind(ukb_var_desc, ukb_var_fid) %>% head
cbind(ukb_var_desc, ukb_var_fid) %>% tail
```



```{r}
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
```




```{r}
# demographics
var_demographics <- png.demographic_vars(type="descriptive")

ukb_data_demographics <- 
  png.ukb_read(path="/Volumes/T7/2.UKB/UKB_ClinicalData_descriptive", 
               vars=var_demographics, 
               ukb_data_dict=ukb_data_dict, 
               exact=FALSE)

ukb_data_dict_demographics <- ukb_data_dict %>% 
  filter( FieldID %in% gsub( "f.", "", png.desc2fid(var_demographics, simple=TRUE) ) )

data.table::fwrite(ukb_data_demographics, paste0("ukb_data_demographics.csv") )
data.table::fwrite(ukb_data_dict_demographics, paste0("ukb_data_dict_demographics.csv") )
```




```{r}
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
```




```{r}
# icd-related
ukb_data <- data.table::fread( "./ukb_data_diagnoses.csv" ) %>% as_tibble()

ukb_data %>% dim


ukb_icd_diagnosis(ukb_data, id=1000000+c(15, 27, 39, 40), icd.version=10)
ukb_icd_code_meaning(icd.code = "E10", icd.version = 10)
ukb_icd_keyword("cardio|atrial", icd.version = 10)
ukb_icd_prevalence(ukb_data, icd.code="I4[8-9].", icd.version = 10)
ukb_icd_prevalence(ukb_data, icd.code="E1[0-1]", icd.version = 10)
```





```{r}
# package: elucidate ---------------------------------------------------------------

library(elucidate)


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

ukb_data <- png.ukb_read(path="/Volumes/T7/2.UKB/UKB_ClinicalData_descriptive", 
                         vars=var_tmp, ukb_data_dict=ukb_data_dict, exact=FALSE)

ukb_data_visit0 <- png.filter_visit(ukb_data, visit=0)
ukb_data_visit0_coding <- png.df2coding(ukb_data_visit0, ukb_data_dict, ukb_coding)

ukb_data_visit0_coding %>% elucidate::describe_na_all()
ukb_data_visit0_coding %>% elucidate::describe_all(class = c("d","f","c","l","n")[1] )
ukb_data_visit0_coding %>% elucidate::describe_all(class = "all" )
```


```{r}
# check coding conversion
ukb_data_visit0$sex_f31_0_0 <- as.factor(ukb_data_visit0$sex_f31_0_0)
ukb_data_visit0[,1:5] %>% elucidate::describe_all()
ukb_data_visit0_coding[,1:5] %>% elucidate::describe_all()
```


```{r}
# Check for similarities between variables with slightly different names
df <- png.filter_visit(ukb_data, visit=0:3) %>% png.df2coding(ukb_data_dict, ukb_coding)

df %>% select(standing_height_f50_0_0, height_f12144_2_0) %>% elucidate::describe_all()
df %>% select(age_at_death_f40007_0_0, age_at_death_f40007_1_0) %>% elucidate::describe_all()
df %>% select(weight_f21002_0_0, weight_f23098_0_0) %>% elucidate::describe_all()

df %>% select(standing_height_f50_0_0, height_f12144_2_0) %>% cor(., use = "complete.obs")
df %>% select(weight_f21002_0_0, weight_f23098_0_0) %>% cor(., use = "complete.obs")
df %>% select(age_at_death_f40007_0_0, age_at_death_f40007_1_0) %>% cor(., use = "complete.obs")
df %>% select(body_mass_index_bmi_f21001_0_0, body_mass_index_bmi_f23104_0_0) %>% cor(., use = "complete.obs")


df %>% select(sex_f31_0_0, genetic_sex_f22001_0_0) %>% table()
```




```{r}
# Genomic data ------------------------------------------------------------
var_genetics <- png.var.genetics()

ukb_data_dict_genetics <- ukb_data_dict %>% 
  filter( FieldID %in% gsub( "f.", "", png.desc2fid(var_genetics, simple=TRUE) ) )

ukb_data_genetics <- 
  png.ukb_read(path="/Volumes/T7/2.UKB/UKB_ClinicalData_descriptive", 
               vars=var_genetics, 
               ukb_data_dict=ukb_data_dict, exact=FALSE)

ukb_data_genetics_AfterQC <- ukb_data_genetics %>% 
  filter(is.na(sex_chromosome_aneuploidy_f22019_0_0), # != 1,
         sex_f31_0_0 == genetic_sex_f22001_0_0, # sex_discordance
         is.na(outliers_for_heterozygosity_or_missing_rate_f22027_0_0), # != 1,
         # missing_rate_in_autosome < 0.02,
         used_in_genetic_principal_components_f22020_0_0 == 1 # relatedness >= 3
)
ukb_data_genetics_AfterQC_coding <- 
  png.df2coding(ukb_data_genetics_AfterQC, ukb_data_dict, ukb_coding)

ukb_data_genetics_AfterQC_coding %>% dim
# [1] 406644     57

ukb_data_genetics %>% filter(!is.na(sex_chromosome_aneuploidy_f22019_0_0)) %>% nrow
# [1] 651
ukb_data_genetics %>% filter(!sex_f31_0_0 == genetic_sex_f22001_0_0) %>% nrow
# [1] 372
ukb_data_genetics %>% filter(!is.na(outliers_for_heterozygosity_or_missing_rate_f22027_0_0)) %>% nrow
# [1] 968
ukb_data_genetics %>% filter(is.na(used_in_genetic_principal_components_f22020_0_0)) %>% nrow
# [1] 95363
nrow(ukb_data_genetics) - nrow(ukb_data_genetics_AfterQC_coding)
# [1] 95757
#
#
#
write.table(ukb_data_genetics_AfterQC$eid, file="ukb_data_genetics_AfterQC_eid.txt", row.names = FALSE, col.names = FALSE)


data.table::fwrite(ukb_data_genetics, "ukb_data_genetics.csv" )
data.table::fwrite(ukb_data_dict_genetics, "ukb_data_dict_genetics.csv" )
```






```{r}
# For each GWAS conducted for each phenotype and ancestry group, we included the following covariates:
#   
# Age
# Sex
# Age * Sex
# Age2
# Age2 * Sex
# The first 10 PCs
```

