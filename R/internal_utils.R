detect_filter_rule <- function(dataset_name) {

  name <- tolower(dataset_name)

  if (grepl("deter", name)) {
    return(list(type = "date", column = "VIEW_DATE"))
  }

  if (grepl("focos", name)) {
    return(list(type = "year_month", year = "Year", month = "Month"))
  }

  return(NULL)
}
