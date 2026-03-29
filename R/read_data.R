#' Read all shapefiles from a folder
#'
#' Loads all .shp files in a directory into a list of sf objects.
#'
#' @param folder Path to folder containing shapefiles
#'
#' @return Named list of sf objects
#' @export
read_all_shapefiles <- function(folder) {

  if (!dir.exists(folder)) {
    stop("Folder does not exist")
  }

  shp_files <- list.files(
    path = folder,
    pattern = "\\.shp$",
    full.names = TRUE
  )

  if (length(shp_files) == 0) {
    stop("No shapefiles found in folder")
  }

  data_list <- lapply(shp_files, sf::read_sf)

  # give names based on file names
  names(data_list) <- tools::file_path_sans_ext(basename(shp_files))

  return(data_list)
}
