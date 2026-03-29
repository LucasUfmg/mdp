#' Run full deforestation prioritization pipeline
#'
#' @param base_path Base folder where deter_data is located or will be created
#' @param folder_id Google Drive shapefile folder ID
#' @param folder_id_tabelas Google Drive tables folder ID
#' @param ano_inicial Initial year
#' @param ano_final Final year
#' @param mes_inicial Initial month
#' @param mes_final Final month
#' @param run_model Logical: run model step
#' @param run_viz Logical: run visualization step
#'
#' @export
run_pipeline <- function(
    base_path = NULL,
    folder_id,
    folder_id_tabelas,
    ano_inicial,
    ano_final,
    mes_inicial,
    mes_final,
    run_model = TRUE,
    run_viz = TRUE
) {

  # -------------------------
  # Resolve base path
  # -------------------------
  if (is.null(base_path)) {
    message("🔎 Searching for deter_data automatically...")
    shp_path <- find_deter_data()
  } else {
    shp_path <- file.path(base_path, "base", "shp")
  }

  # -------------------------
  # Check data
  # -------------------------
  if (!check_local_data(shp_path)) {

    message("⬇️ Data not found. Downloading from Google Drive...")

    paths <- download_drive_data(
      folder_id = folder_id,
      folder_id_tabelas = folder_id_tabelas,
      dest_folder = dirname(shp_path)
    )

    shp_path <- paths$shp_dir
  }

  # -------------------------
  # Run PREP
  # -------------------------
  message("📦 Running data preparation...")

  def_prep(
    folder = shp_path,
    ano_inicial = ano_inicial,
    ano_final = ano_final,
    mes_inicial = mes_inicial,
    mes_final = mes_final
  )

  # -------------------------
  # Run MODEL
  # -------------------------
  if (run_model) {
    message("🤖 Running model...")

    def_prio(
      folder = shp_path,
      ano_inicial = ano_inicial,
      ano_final = ano_final,
      mes_inicial = mes_inicial,
      mes_final = mes_final
    )
  }

  # -------------------------
  # Run VIZ
  # -------------------------
  if (run_viz) {
    message("📊 Running visualization...")

    def_viz(
      folder = shp_path,
      mes_inicial = mes_inicial,
      mes_final = mes_final
    )
  }

  message("✅ Pipeline completed successfully!")

  invisible(shp_path)
}
