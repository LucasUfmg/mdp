check_local_data <- function(shp_path) {

  tab_path <- file.path(shp_path, "tabelas")

  if (!dir.exists(shp_path)) return(FALSE)
  if (!dir.exists(tab_path)) return(FALSE)

  shp_files <- list.files(shp_path, pattern = "\\.shp$", ignore.case = TRUE)
  tab_files <- list.files(tab_path)

  return(length(shp_files) > 0 && length(tab_files) > 0)
}
