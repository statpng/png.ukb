# ukbwranglr
# devtools::install_github("rmgpanw/ukbwranglr")

test2 <- function(path){
  
  
  # contributed by ukbtools, ukbwranglr
  
  {
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
  }
  
  
  
  {
    path <- "/Volumes/T7/Yonsei/3. UKB/Clinical/Maindata3/"
    filename <- "ukb669778"
  }
  
  
  
  
  {
    ukb_data <- data.table::fread( path%_%filename%_%".tab", nrows=100)
    ukb_field <- get_ukb_field(filename, path)
    ukb_dict <- get_ukb_dict()
    ukb_coding <- get_ukb_codings()
    ukb_data_dict <- make_data_dict( ukb_main=path%_%filename%_%".tab",
                                     ukb_dict = ukb_dict)
    ukb_dict_path <- png.dict2path(ukb_dict)
    ukb_data_dict_path <- png.dict2path(ukb_data_dict)
    
    
    # Note
    ukb_data_dict_NA <- ukb_data_dict %>% filter(is.na(Path))
    ukb_data_dict_NA %>% print(n="max") %>% as.matrix
    MissingField <- unique( gsub("f|_[0-9]", "", ukb_data_dict_NA$colheaders_processed[-1]) )
    # 20074, 20075, 22400, 22401, 22402
    MissingField %>% map(~colnames(ukb_data) %>% {.[grep(.x, .)]} )
    
  }
  
  
  
  
  # For write files
  if(FALSE){
    
    ukb_data_dict_path_unique <- ukb_data_dict_path[,1:5] %>%
      apply(2, function(x) unique(x[!is.na(x)]) ) %>%
      {
        tmp_len_max <- max(sapply(., length))
        map(., ~ c(.x, rep("", tmp_len_max-length(.x) ) ) )
      } %>% do.call("cbind", .)
    
    
    list(ukb_field, ukb_dict, ukb_coding, ukb_data_dict,
         ukb_dict_path %>% arrange(path1, path2, path3, path4, path5),
         ukb_data_dict_path %>% arrange(path1, path2, path3, path4, path5),
         ukb_data_dict_path_unique
    ) %>% 
      map2( list("ukb_field", "ukb_dict", "ukb_coding", "ukb_data_dict",
                 "ukb_dict_path", "ukb_data_dict_path", 
                 "ukb_data_dict_path_unique"), 
            ~write.csv(.x, file=paste0("./output/", .y, ".csv"), row.names=FALSE ))
    
    
  }
  
  
  
  
  {
    
    
    PATH <- ukb_data_dict_path %>% select(path1:path5)# %>% filter(!is.na(path1))
    path.new <- png.path.NestOverN(PATH, N=2000)
    sum(table(path.new)>2000) %>% print()
    
    tab <- table(path.new) %>% sort(decreasing=TRUE)
    title <- names(tab)
    
    # slow
    if(FALSE){
      library(data.table)
      
      for( i in 1:length(tab) ){
        
        wh <- c(1, which( path.new == names(tab)[i] ))
        
        ukb_data_dict_filtered <- 
          ukb_data_dict %>% 
          slice(wh)
        
        ukb_data_filtered <- 
          read_ukb( path = path%_%filename%_%".tab",
                    descriptive_colnames = FALSE,
                    label = FALSE, 
                    ukb_data_dict = ukb_data_dict_filtered )
        
        fwrite(ukb_data_filtered, paste0("ukb_data - ", title[i], ".csv") )
        
        gc()
        rm(ukb_data_filtered)
      }
      
    }
    
    
    
    
    
  }
  
  
  
  
  
  {
    
    # slow
    ukb_data <- 
      read_ukb( path = path%_%filename%_%".csv",
                descriptive_colnames = TRUE,
                label = FALSE, 
                ukb_data_dict = ukb_data_dict %>% 
                  filter(grepl("eid|Ethnic", Field)) ) %>% 
      as_tibble()
    # Time taken: 5 minutes, 53 seconds.
    
    
    ukb_data <- 
      read_ukb( path = "/Users/png/Documents/6. Yonsei/2. UKB/ukb_data - Online follow-up > Diet by 24-hour recall.csv",
                descriptive_colnames = TRUE,
                label = FALSE, 
                ukb_data_dict = ukb_data_dict %>% 
                  filter( descriptive_colnames == "eid" | path.new == "Online follow-up > Diet by 24-hour recall" ) ) %>% 
      as_tibble()
    # Time taken: 1 minutes, 10 seconds.
    
    
    attr(ukb_data, "argument")
    
    format( object.size(ukb_data), "Mb" )
    # [1] "11.5 Mb"
    
    # No duplicates in eid
    ukb_data$eid %>% {sum(duplicated(.))}
    
    # genetic_ethnic_grouping_
    ukb_data$ethnic_background_f21000_0_0 %>% stringr::str_sub(1,1) %>% table
    # -      1      2      3      4      5      6 
    # 1878 472603   2954   9879   8058   1573   4556 
    
    ukb_data[1:5,]
  }
  
  
  
  
  
  
  
}

