#' Full pipeline: data prep + model
#'
#' @param folder_id Google Drive folder ID (optional)
#' @param folder_id_tabelas Google Drive folder ID (optional)
#' @param base_path Optional local path
#' @param ano_inicial Initial year
#' @param ano_final Final year
#' @param mes_inicial Initial month
#' @param mes_final Final month
#' @param run_model Should run def_prio? (default = TRUE)
#'
#' @export
prep_pipeline <- function(
    folder_id = NULL,
    folder_id_tabelas = NULL,
    base_path = NULL,
    ano_inicial,
    ano_final,
    mes_inicial,
    mes_final,
    run_model = TRUE
) {

  # -----------------------------------
  # 1. Locate data
  # -----------------------------------
  shp_path <- find_deter_data(base_path)

  # -----------------------------------
  # 2. Download if needed
  # -----------------------------------
  if (!check_local_data(shp_path)) {

    if (is.null(folder_id) || is.null(folder_id_tabelas)) {
      stop("Data not found locally and no Google Drive IDs provided")
    }

    paths <- download_drive_data(
      folder_id = folder_id,
      folder_id_tabelas = folder_id_tabelas,
      dest_folder = dirname(shp_path)
    )

    shp_path <- paths$shp_dir
  }

  message("Data ready at: ", shp_path)

  # -----------------------------------
  # 3. Run data preparation
  # -----------------------------------
  message("Running def_prep...")

  df <- def_prep(
    folder = shp_path,
    ano_inicial = ano_inicial,
    ano_final = ano_final,
    mes_inicial = mes_inicial,
    mes_final = mes_final
  )

  message("def_prep completed.")

  # -----------------------------------
  # 4. Run model (optional)
  # -----------------------------------
  if (run_model) {

    message("Running def_prio...")

    def_prio(
      folder = shp_path,
      ano_inicial = ano_inicial,
      ano_final = ano_final,
      mes_inicial = mes_inicial,
      mes_final = mes_final
    )

    message("def_prio completed.")
  }

  # -----------------------------------
  # 5. Return result
  # -----------------------------------
  return(invisible(df))
}
