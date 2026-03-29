#' Download shapefiles and tables from Google Drive
#'
#' Downloads all shapefiles and tables from public Google Drive folders.
#'
#' @param folder_id Google Drive folder ID for shapefiles
#' @param folder_id_tabelas Google Drive folder ID for tables
#' @param dest_folder Local destination folder
#'
#' @return List with local paths
#' @export
download_drive_data <- function(
    folder_id,
    folder_id_tabelas,
    dest_folder = "data/raw"
) {

  # Create directories
  shp_dir <- file.path(dest_folder, "shp")
  tab_dir <- file.path(dest_folder, "tabelas")

  dir.create(shp_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

  # Disable authentication (public data)
  googledrive::drive_deauth()

  # List files
  files <- googledrive::drive_ls(googledrive::as_id(folder_id))
  files_tabelas <- googledrive::drive_ls(googledrive::as_id(folder_id_tabelas))

  # Filter shapefile components
  shp_ext <- "\\.(shp|shx|dbf|prj|cpg)$"

  shp_files <- files[grepl(shp_ext, files$name, ignore.case = TRUE), ]

  # -------------------------
  # Download shapefiles
  # -------------------------
  for (i in seq_len(nrow(shp_files))) {

    local_path <- file.path(shp_dir, shp_files$name[i])

    if (file.exists(local_path)) {
      message("Skipping: ", shp_files$name[i])
    } else {
      message("Downloading: ", shp_files$name[i])

      googledrive::drive_download(
        file = dplyr::slice(shp_files, i),
        path = local_path,
        overwrite = FALSE
      )
    }
  }

  # -------------------------
  # Download tables
  # -------------------------
  for (i in seq_len(nrow(files_tabelas))) {

    local_path <- file.path(tab_dir, files_tabelas$name[i])

    if (file.exists(local_path)) {
      message("Skipping: ", files_tabelas$name[i])
    } else {
      message("Downloading: ", files_tabelas$name[i])

      googledrive::drive_download(
        file = dplyr::slice(files_tabelas, i),
        path = local_path,
        overwrite = FALSE
      )

      Sys.sleep(1)
    }
  }

  return(list(
    shp_dir = shp_dir,
    tab_dir = tab_dir
  ))
}


usethis::use_package("googledrive")
usethis::use_package("dplyr")
