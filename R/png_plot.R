#' @importFrom htmlwidgets saveWidget
#' @export plot_ukb_dict
plot_ukb_dict <- function(ukb_dict){
  
  {
    # assertthat::assert_that("ukb_dict_split" %in% class(ukb_dict))
    if( !"ukb_dict_split" %in% class(ukb_dict) ){
      ukb_dict_split <- split.ukb_dict(ukb_dict)
    }
    
    df_path <- ukb_dict_split %>% select(starts_with("path"))
  }
  
  
  {
    # library(plotme)
    
    df_path_summary <-
      df_path %>% 
      group_by_at(vars(paste0("path", 1:5))) %>% 
      # distinct() %>% 
      summarise(n=n())
    
    p_sun <- df_path_summary %>% 
      mutate(n = 1) %>% 
      count_to_sunburst()
    p_tree <- df_path_summary %>% 
      mutate(n = 1) %>% 
      count_to_treemap()
    
    p_sun_weighted <- df_path_summary %>% 
      count_to_sunburst()
    p_tree_weighted <- df_path_summary %>% 
      count_to_treemap()
    
    # plotly::export(p = p_sun, file = "sunburst.pdf")
    # plotly::export(p = p_tree, file = "tree.pdf")
    # plotly::export(p = p_sun_weighted, file = "sunburst_weighted.pdf")
    # plotly::export(p = p_tree_weighted, file = "tree_weighted.pdf")
    
    htmlwidgets::saveWidget(widget=p_sun, file="sunburst.html", selfcontained=TRUE)
    htmlwidgets::saveWidget(widget=p_tree, file="tree.html", selfcontained=TRUE)
    htmlwidgets::saveWidget(widget=p_sun_weighted, file="sunburst_weighted.html", selfcontained=TRUE)
    htmlwidgets::saveWidget(widget=p_tree_weighted, file="tree_weighted.html", selfcontained=TRUE)
    
    
    plotly::orca(p = p_sun, file = "sunburst.pdf")
    plotly::orca(p = p_tree, file = "tree.pdf")
    plotly::orca(p = p_sun_weighted, file = "sunburst_weighted.pdf")
    plotly::orca(p = p_tree_weighted, file = "tree_weighted.pdf")
    
    
  }
  
  
  
  
  
  
  
  
  # {
  #   
  #   # devtools::install_github("timelyportfolio/sunburstR")
  #   library(treemap)
  #   library(sunburstR)
  #   library(d3r)
  #   
  #   df_path %>%
  #     group_by(path1, path2, path3, path4) %>%
  #     summarise(N=n()) %>%
  #     d3_nest( value_cols = c("N", "path1") ) %>%
  #     sunburstR::sunburst(valueField = "N", count=TRUE)
  #   
  #   df_path %>%
  #     group_by(path1, path2, path3, path4) %>%
  #     summarise(N=n()) %>%
  #     unite("path", path1:path4, sep="-") %>% {
  #       sunburstR::sunburst(.,
  #                           count=TRUE,
  #                           # legendOrder = unique(unlist(strsplit(.[,1],"-"))),
  #                           explanation = "function(d){return d.data.name}"
  #       )
  #     }
  #   
  #   
  #   df_path %>%
  #     group_by(path1, path2, path3, path4) %>%
  #     summarise(N=n()) %>%
  #     ungroup() %>%
  #     treemap(index=c("path1", "path2", "path3", "path4"),
  #             vSize="N",
  #             vColor="path1",
  #             type="index")
  #   
  # }
  
  
  # {
  #   
  #   # devtools::install_github("yogevherz/plotme")
  #   # library(plotme)
  #   library(dplyr)
  #   
  #   df_path %>% 
  #     group_by_at(vars(paste0("path", 1:5))) %>% 
  #     summarise(N=n()) %>%
  #     rename(n = N) %>% 
  #     count_to_sunburst()
  #   
  #   df_path %>% 
  #     group_by_at(vars(paste0("path", 1:5))) %>% 
  #     summarise(N=n()) %>%
  #     rename(n = N) %>% 
  #     count_to_treemap(sort_by_n = TRUE)
  #   
  #   
  #   ukb_dict$Path[grepl("surfer", ukb_dict$Path)]
  #   
  #   ukb_dict[ ukb_dict2$path5 == "and", ] %>% head
  #   
  #   
  # }
  
  
  # {
  #   library(ggpie)
  #   ggnestedpie(df_path, group_key=c("path1", "path2"), count_type="full")
  #   
  #   ggnestedpie(data = diamonds, group_key = c("cut", "color"), count_type = "full",
  #               inner_label_info = "all", inner_label_split = NULL, inner_label_size = 2,
  #               outer_label_type = "circle", outer_label_pos = "out", outer_label_info = "all")
  #   
  # }
  
  
}








