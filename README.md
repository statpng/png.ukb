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


ukb_field %>% str
# 'data.frame':	20653 obs. of  5 variables:
# $ field.showcase: chr  "eid" "3" "3" "3" ...
# $ field.html    : chr  "eid" "3-0.0" "3-1.0" "3-2.0" ...
# $ field.tab     : chr  "f.eid" "f.3.0.0" "f.3.1.0" "f.3.2.0" ...
# $ col.type      : chr  "Sequence" "Integer" "Integer" "Integer" ...
# $ col.name      : chr  "eid" "verbal_interview_duration_f3_0_0" "verbal_interview_duration_f3_1_0" "verbal_interview_duration_f3_2_0" ...

ukb_dict %>% str
# Classes ‘data.table’ and 'data.frame':	9079 obs. of  17 variables:
# $ Path        : chr  "Assessment centre > Procedural metrics > Process durations" "Assessment centre > Procedural metrics > Process durations" "Assessment centre > Procedural metrics > Process durations" "Assessment centre > Procedural metrics > Process durations" ...
# $ Category    : chr  "152" "152" "152" "152" ...
# $ FieldID     : chr  "3" "4" "5" "6" ...
# $ Field       : chr  "Verbal interview duration" "Biometrics duration" "Sample collection duration" "Conclusion duration" ...
# $ Participants: chr  "501528" "498017" "501201" "499062" ...
# $ Items       : chr  "585037" "586338" "591369" "589111" ...
# $ Stability   : chr  "Complete" "Complete" "Complete" "Complete" ...
# $ ValueType   : chr  "Integer" "Integer" "Integer" "Integer" ...
# $ Units       : chr  "seconds" "seconds" "seconds" "seconds" ...

ukb_coding %>% str
# Classes ‘data.table’ and 'data.frame':	532346 obs. of  3 variables:
# $ Coding : chr  "1" "2" "2" "2" ...
# $ Value  : chr  "1" "0" "1" "11" ...
# $ Meaning: chr  "Yes" "Other job (free text entry)" "Managers and Senior Officials" "Corporate Managers" ...
# - attr(*, ".internal.selfref")=<externalptr> 

ukb_data_dict %>% str
# tibble [20,653 × 22] (S3: tbl_df/tbl/data.frame)
# $ descriptive_colnames: chr [1:20653] "eid" "verbal_interview_duration_f3_0_0" "verbal_interview_duration_f3_1_0" "verbal_interview_duration_f3_2_0" ...
# $ colheaders_raw      : chr [1:20653] "f.eid" "f.3.0.0" "f.3.1.0" "f.3.2.0" ...
# $ colheaders_processed: chr [1:20653] "feid" "f3_0_0" "f3_1_0" "f3_2_0" ...
# $ FieldID             : chr [1:20653] "eid" "3" "3" "3" ...
# $ instance            : chr [1:20653] NA "0" "1" "2" ...
# $ array               : chr [1:20653] NA "0" "0" "0" ...
# $ Path                : chr [1:20653] NA "Assessment centre > Procedural metrics > Process durations" "Assessment centre > Procedural metrics > Process durations" "Assessment centre > Procedural metrics > Process durations" ...
# $ Category            : chr [1:20653] NA "152" "152" "152" ...
# $ Field               : chr [1:20653] "Participant identifier ('eid')" "Verbal interview duration" "Verbal interview duration" "Verbal interview duration" ...
```


```{r}
# Note: variables with missing FieldID
ukb_data_dict_NA <- ukb_data_dict %>% filter(is.na(Path))
ukb_data_dict_NA %>% as.matrix %>% .[,1]
# [1] "eid"         "f.20074.0.0" "f.20074.1.0" "f.20074.2.0" "f.20075.0.0"
# [6] "f.20075.1.0" "f.20075.2.0" "f.22400.0.0" "f.22401.0.0" "f.22402.2.0"
```




```{r}
ukb_dict$Path %>% head
# [1] "Assessment centre > Procedural metrics > Process durations"                
# [2] "Assessment centre > Procedural metrics > Process durations"                
# [3] "Assessment centre > Procedural metrics > Process durations"                
# [4] "Assessment centre > Procedural metrics > Process durations"                
# [5] "Assessment centre > Physical measures > Bone-densitometry of heel"         
# [6] "Assessment centre > Physical measures > Anthropometry > Body size measures"


# split Path to path1:path5
ukb_dict_path <- png.dict2path(ukb_dict)
ukb_data_dict_path <- png.dict2path(ukb_data_dict)
```



```{r}
# New path categories with at most 2000 variables
ukb_path <- ukb_data_dict_path %>% select(path1:path5) %>% png.path.NestOverN(N=2000)
max(table(ukb_path)) < 2000

ukb_data_dict$Path %>% table %>% length
# [1] 176
ukb_path %>% table %>% length
# [1] 55
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
# Types of col.names
var_demographics <- png.demographic_vars(type="descriptive")

var_demographics %>% head
# [1] "eid"                           "sex_f31_0_0"                  
# [3] "genetic_sex_f22001_0_0"        "year_of_birth_f34_0_0"        
# [5] "month_of_birth_f52_0_0"        "age_at_recruitment_f21022_0_0"

png.desc2fid(ukb_var_desc, simple=FALSE) %>% head
# [1] "f.eid"       "f.31.0.0"    "f.22001.0.0" "f.34.0.0"    "f.52.0.0"    "f.21022.0.0"

png.desc2fid(ukb_var_desc, simple=TRUE) %>% head
# [1] "f.eid"   "f.31"    "f.22001" "f.34"    "f.52"    "f.21022"
```




```{r}
# Load data from splitted data files
## demographics
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
## diagnoses
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
### icd-related
ukb_data_diagnoses <- data.table::fread( "./ukb_data_diagnoses.csv" ) %>% as_tibble()

ukb_icd_diagnosis(ukb_data_diagnoses, id=1000000+c(15, 27), icd.version=10)
# 9  1000015 Z961                                               Z96.1 Presence of intraocular lens
# 10 1000015 Z966                                     Z96.6 Presence of orthopaedic joint implants
# 11 1000015 H269                                                      H26.9 Cataract, unspecified
# 12 1000015 H251                                                    H25.1 Senile nuclear cataract
# 13 1000027 D370                                               D37.0 Lip, oral cavity and pharynx
# 14 1000027  K30                                                                    K30 Dyspepsia
# 15 1000027 K801                           K80.1 Calculus of gallbladder with other cholecystitis
ukb_icd_code_meaning(icd.code = "E10", icd.version = 10)
#    code   meaning
# 1  E10    E10 Insulin-dependent diabetes mellitus
ukb_icd_keyword("cardio|atrial", icd.version = 10) %>% head
#   code                                                   meaning
# 1 B334 B33.4 Hantavirus (cardio-)pulmonary syndrome [HPS] [HCPS]
# 2 A520                             A52.0 Cardiovascular syphilis
# 3 A439                            A43.9 Nocardiosis, unspecified
# 4 A438                          A43.8 Other forms of nocardiosis
# 5 A431                               A43.1 Cutaneous nocardiosis
# 6 A430                               A43.0 Pulmonary nocardiosis

ukb_icd_prevalence(ukb_data_diagnoses, icd.code="H401", icd.version = 10)
# [1] 0.007143696

# ukb_icd_prevalence(ukb_data_diagnoses, icd.code="E1[0-1]", icd.version = 10)
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








```shell
#!/bin/bash


# Download Imputation Score

## Note that you should have a directory "./QC/ukb_imp_mfi".

for i in {1..22} X XY
do
    wget https://biobank.ctsu.ox.ac.uk/ukb/ukb/auxdata/ukb_mfi_chr${i}_v3.txt -P ./QC/ukb_imp_mfi
done

# wget https://biobank.ctsu.ox.ac.uk/ukb/ukb/auxdata/ukb_snp_qc.txt
# wget https://biobank.ctsu.ox.ac.uk/ukb/ukb/auxdata/ukb_snp_bim.tar
# wget https://biobank.ctsu.ox.ac.uk/ukb/ukb/auxdata/ukb_imp_bgi.tgz
# wget https://biobank.ctsu.ox.ac.uk/ukb/ukb/auxdata/ukb_imp_mfi.tgz
# wget https://biobank.ctsu.ox.ac.uk/ukb/ukb/auxdata/ukb_hap_bgi.tgz
# wget https://biobank.ctsu.ox.ac.uk/ukb/ukb/auxdata/ukb_snp_posterior.tar
# wget https://biobank.ctsu.ox.ac.uk/ukb/ukb/auxdata/ukb_snp_posterior.batch






## 1. Imputation Score
# for i in {1..22}
# do
#   awk -v chr=$i 'BEGIN {FS="\t"; OFS="\t"} NF==8 { print chr,$0,chr":"$3"_"$4"_"$5 }' ./QC/ukb_imp_mfi/ukb_mfi_chr${i}_v3.txt
# done > ./QC/ukb_mfi_all_v3.tsv


## 2. Format Convergion
for i in {1..22}
do
  echo "Running for chromosome $i"

  ~/plink2 --bgen ./bgen/ukb22828_c${i}_b0_v3.bgen ref-first
           --hard-call-threshold 0.1 \
           --sample ./sample/ukb22828_c${i}_b0_v3_s4871*.sample \
           --memory 20000 \
           --set-all-var-ids @:\#_\$r_\$a \
           --new-id-max-allele-len 500 \
           --freq \
           --make-pgen \
           --out ukb22828_c${i}_b0_v3



## 3. SNP QC with high heterozygosity
  awk '/^[^#]/ { if( $5>0.4 && $5<0.6 && ( ($3=="A" && $4=="T") || ($4=="T" && $3=="A") || ($3=="C" && $4=="G") || ($4=="G" && $3=="C") ) ) { print $0 }}' \
ukb22828_c${i}_b0_v3.afreq > exclrsIDs_c${i}_ambiguous.txt



## 4. Sample & SNP QC
  ~/plink2  --pfile ukb22828_c${i}_b0_v3 \
          --memory 20000 \
          --exclude exclrsIDs_c${i}_ambiguous.txt \
          --extract-col-cond ./QC/ukb_imp_mfi/ukb_mfi_chr${i}_v3.txt 8 1 \
          --extract-col-cond-min 0.4 \
          --maf 0.005 \
          --keep-fam ./QC/ukb_data_genetics_AfterQC_eid.txt \
          --write-snplist --write-samples --make-pgen \
          --out ukb22828_c${i}_b0_v3_QC

done

echo "All chromosomes completed"

```
