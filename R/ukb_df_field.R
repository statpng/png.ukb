#' @importFrom dplyr `%>%`
#' @importFrom vctrs data_frame
#' @import XML
#' @import stringr
#' @export
get_ukb_field <- function (fileset, path = ".", data.pos = 2, as.lookup = FALSE)
{
  html_file <- stringr::str_interp("${fileset}.html")
  html_internal_doc <- XML::htmlParse(file.path(path, html_file))
  html_table_nodes <- XML::getNodeSet(html_internal_doc, "//table")
  html_table = XML::readHTMLTable(html_table_nodes[[data.pos]],
                                  as.data.frame = TRUE, stringsAsFactors = FALSE, colClasses = c("integer", "character", "integer", "character", "character"))
  df <- .fill_missing_description(html_table)
  lookup <- .description_to_name(df)
  old_var_names <- paste("f.", gsub("-", ".", df[, "UDI"]),
                         sep = "")
  if (as.lookup) {
    names(lookup) <- old_var_names
    return(lookup)
  } else {
    lookup.reference <- vctrs::data_frame(field.showcase = gsub("-.*$", "", df[, "UDI"]), 
                                          field.html = df[, "UDI"], 
                                          field.tab = old_var_names,
                                   col.type = df[, "Type"], 
                                   col.name = ifelse(gsub("-.*$", "", df[, "UDI"]) == "eid", "eid", stringr::str_c(lookup, "_f", stringr::str_replace_all(df[, "UDI"],
                                                                                                                                  c(`-` = "_", `\\.` = "_")))))
    return(lookup.reference)
  }
}








.fill_missing_description <-  function(data) {
  udi <- gsub(pattern = "-.*$", "", data[, "UDI"])
  for (i in 2:nrow(data)) {
    if (udi[i] == udi[i-1] & is.na(data[, "Description"][i])) {
      data[i, "Type"] <- data[i-1, "Type"]
      data[i, "Description"] <- data[i-1, "Description"]
    }
  }
  return(data)
}





.description_to_name <-  function(data) {

  name <- tolower(data[, "Description"]) %>%
    gsub(" - ", "_", x = .) %>%
    gsub(" ", "_", x = .) %>%
    gsub("uses.*data.*coding.*simple_list.$", "", x = .) %>%
    gsub("uses.*data.*coding.*hierarchical_tree.", "", x = .) %>%
    gsub("uses.*data.*coding_[0-9]*", "", x = .) %>%
    gsub("[^[:alnum:][:space:]_]", "", x = .) %>%
    gsub("__*", "_", x = .)

  return(name)
}
