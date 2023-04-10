# png.ukb
> This package combines some features from both ukbwranglr and ukbtools, and we express our gratitude to their contributions.







## Library

```{r}
#| label: load-packages

  # devtools::install_github("statpng/png.ukb")
  # detach("package:png.ukb", unload=TRUE)
  library(png.ukb)
  library(tidyverse)
  
  library(dplyr)
  library(tidyr)
  library(tidyselect)
  library(readr)
  library(tibble)
  library(broom)
  
  source("https://raw.githubusercontent.com/statpng/pngfunction/master/tidyverse/functions.R")

```




## Data Import

```{r}
  # set the path and filename of "UKB - Clinical Data"
  path <- "/Volumes/T7/Yonsei/3. UKB/Clinical/Maindata3/"
  filename <- "ukb669778"
```



We will use the custom function `%_%`, from the `source()` above, to concatenate the path and filename as follows:
```{r}
path%_%filename%_%".tab"
```


---




```{r}

  ukb_field <- get_ukb_field(filename, path)
  ukb_dict <- get_ukb_dict()
  ukb_coding <- get_ukb_codings()
  ukb_data_dict <- make_data_dict( ukb_main=path%_%filename%_%".tab", ukb_dict = ukb_dict)
  ukb_dict_path <- png.dict2path(ukb_dict)
  ukb_data_dict_path <- png.dict2path(ukb_data_dict)
```

- `ukb_field`: Indicates the documentation of the variables (dictionary) for the main data that exists in `path+filename`.
- `ukb_dict`: Dictionary from UKB
- `ukb_coding`: Codebook from UKB
- `ukb_data_dict`: Dictionary for only the variables that exist in our main data
- `ukb_dict_path`: A version of `ukb_dict` that divides the Path variable into 5 categories
- `ukb_data_dict_path`: A version of `ukb_data_dict` that divides the Path variable into 5 categories


---





Looking at the variables in `ukb_data_dict` that have NA values for "Field ID". We suspect that the description for that variable has not yet been updated.
```{r}
# Note
  ukb_data_dict_NA <- ukb_data_dict %>% filter(is.na(Path))
  ukb_data_dict_NA %>% as.matrix
  
  MissingField <- unique( gsub("f|_[0-9]", "", ukb_data_dict_NA$colheaders_processed[-1]) )
  # 20074, 20075, 22400, 22401, 22402
  print(MissingField)
```

```{r}
ukb_field %>% filter( field.showcase %in% MissingField )
```

---




To save the above data, we will use `purrr:map2` function as follows:
```{r}

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
          ~write.csv(.x, file=paste0("./", .y, ".csv"), row.names=FALSE ))
  
}

```

---




You can load a dataset with specific variables in an efficient way using `read_ukb` based on the `data.table` package which handles large volumes of data efficiently.
By filtering out only the variables of interest in `ukb_data_dict`, you can load a dataset consisting of only those variables.

```{r}
if(FALSE){
  # slow
  ukb_data <- 
    read_ukb( path = path%_%filename%_%".csv",
              descriptive_colnames = TRUE,
              label = FALSE, 
              ukb_data_dict = ukb_data_dict %>% 
                filter(grepl("eid|Ethnic", Field)) ) %>% 
    as_tibble()
  # Time taken: 5 minutes, 53 seconds.
  
  # genetic_ethnic_grouping_
  
  # -      1      2      3      4      5      6 
  # 1878 472603   2954   9879   8058   1573   4556 
  ukb_data$ethnic_background_f21000_0_0 %>% stringr::str_sub(1,1) %>% table
}
```

---







Additionally, you can load a dataset containing only two specific variables (`f.eid`, `f.31.0.0`) in the following way.
```{r}
ukb_data <- data.table::fread( path%_%filename%_%".tab", nrows=1000, select=c("f.eid", "f.31.0.0"))
```


---





## Dividing Datasets Based on Path Categories

We aim to categorize approximately 20,000 variables into subgroups using Path categories (`ukb_data_dict$Path`) to minimize data retrieval time. We want each dataset to contain a maximum of `N` variables.
To achieve this, initially, retain categories with `N` or fewer variables based on path1 and save them as "UKB - path1.csv", where path1 represents the name of each category in path1 `table(ukb_data_dict_path$path1)`.
For categories in path1 with more than `N` variables, utilize path2 and save them as "path1 > path2".
Repeat the aforementioned process for all datasets containing `N` or fewer variables.

```{r}

  PATH <- ukb_data_dict_path %>% select(path1:path5)
  ukg_path <- png.path.NestOverN(PATH, N=2000)

  tab <- table(ukg_path) %>% sort(decreasing=TRUE)
  title <- names(tab)
  
  head(tab) %>% print()
  sum(tab>2000) %>% print()
  
```

---



Save a dataset for each category using `data.table::fwrite`.

```{r}

  # slow
  if(FALSE){
    library(data.table)
    
    for( i in 1:length(tab) ){
      
      wh <- c(1, which( path.new == names(tab)[i] )) # eid + variables in each category
      
      ukb_data_dict_filtered <- ukb_data_dict %>% slice(wh)
      
      ukb_data_filtered <- 
        read_ukb( path = path%_%filename%_%".tab",
                  descriptive_colnames = TRUE,
                  label = FALSE, 
                  ukb_data_dict = ukb_data_dict_filtered )
      
      fwrite(ukb_data_filtered, paste0("ukb_data - ", title[i], ".csv") )
      
      # To reduce the memory in use
      gc()
      rm(ukb_data_filtered)
    }
    
  }
```


---




## Reduced dataset
```{r}
if(FALSE){
  ukb_data <- 
    read_ukb( path = "/Volumes/T7/2.UKB/UKB ClinicalData - splitted/ukb_data - Online follow-up > Diet by 24-hour recall.csv",
              descriptive_colnames = TRUE,
              label = FALSE, 
              ukb_data_dict = ukb_data_dict %>% 
                filter( descriptive_colnames == "eid" | path.new == "Online follow-up > Diet by 24-hour recall" ) ) %>% 
    as_tibble()
  # Time taken: 0 minutes, 53 seconds.

  dim(ukb_data)
  # [1] 502401   1947
}

```
Keep in mind that it's necessary to filter out `ukb_data_dict` to correspond with the reduced dataset.

---





You can see the argument of `ukb_data_dict` used to construct `ukb_data` by `attr(ukb_data, "argument")`.

```{r}
attr(ukb_data, "argument")
# [1] "ukb_data_dict %>% filter(descriptive_colnames == \"eid\" | path.new == "
# [2] "    \"Online follow-up > Diet by 24-hour recall\")"
```


---




## Demographic variables

The `png.ukb` package includes a collection of demographic variables that have been subjectively chosen. You can access them with `png.demographic_vars(type=c("fnum", "descriptive"))`. We save a dataset containing only these demographic variables as a CSV file in the following manner:

```{r}
  ukb_data_dict$descriptive_colnames %>% head(1300) %>% tail(20)
  
  demographic_vars <- png.demographic_vars(type="fnum")
  demographic_vars_all <- 
    demographic_vars %>% 
    map(~ ukb_data_dict$colheaders_raw[grep(.x, ukb_data_dict$colheaders_raw)]) %>% 
    Reduce(c, .) %>% unique
  
  length(demographic_vars_all)
  
  ukb_data_dict_filtered <- 
      ukb_data_dict %>% 
      filter( colheaders_raw %in% demographic_vars_all )
  
  head(ukb_data_dict_filtered)
  
  if(FALSE){
    ukb_data_filtered <- 
      read_ukb( path = path%_%filename%_%".tab",
                descriptive_colnames = TRUE,
                label = FALSE, 
                ukb_data_dict = ukb_data_dict_filtered )
    
    fwrite(ukb_data_filtered, paste0("ukb_data - demographic.csv") )
  }
```



