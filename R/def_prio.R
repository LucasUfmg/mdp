#' Run prioritization model
#'
#' @param folder Base folder path
#' @param ano_inicial Initial year
#' @param ano_final Final year
#' @param mes_inicial Initial month
#' @param mes_final Final month
#'
#' @export
def_prio <- function(folder, ano_inicial, ano_final, mes_inicial, mes_final) {

  sf::sf_use_s2(FALSE)

  # -------------------------
  # Ensure output folder
  # -------------------------
  if (!dir.exists(file.path(folder, "outputs"))) {
    dir.create(file.path(folder, "outputs"))
  }

  # -------------------------
  # Load data
  # -------------------------
  pt_infra_f <- utils::read.csv(
    file.path(folder, "tabelas", "infra_pa.csv")
  ) |>
    dplyr::select(-X) |>
    dplyr::rename(
      area_PA = PA_area,
      dist_road = dist_rod,
      dist_hidro = dist_hid,
      dist_hidro_road = dist_hidro_rod
    )

  year_list <- utils::read.csv(
    file.path(
      folder,
      "tabelas",
      paste0("df_operacionalizado_", ano_inicial, "_", ano_final, ".csv")
    )
  )

  year_list$area_deter_m2[is.na(year_list$area_deter_m2)] <- 0

  # -------------------------
  # Loop over months
  # -------------------------
  for (i in mes_inicial:mes_final) {

    # -------------------------
    # Prepare dataset
    # -------------------------
    df1_f <- year_list |>
      dplyr::filter(Month == i) |>
      dplyr::mutate(
        lagg = dplyr::case_when(
          lag == -1 ~ "atual",
          lag == 0  ~ "ano_ant",
          lag == 1  ~ "dois_ant",
          lag == 3  ~ "quatro_ant"
        )
      ) |>
      dplyr::select(-lag)

    df_long <- df1_f |>
      dplyr::mutate(
        Month_year = paste0(year, "_", sprintf("%02d", Month))
      )

    df_wide <- tidyr::pivot_wider(
      df_long,
      id_cols = c(OBJECTID),
      names_from = c(lagg, Month_year),
      values_from = c(area_deter_m2, focos, dist_m),
      names_glue = "{.value}_lag{lagg}_{Month_year}",
      values_fn = mean
    )

    df11 <- df_wide |>
      dplyr::left_join(pt_infra_f, by = "OBJECTID") |>
      dplyr::mutate(
        dplyr::across(
          where(~ inherits(.x, "units") && as.character(units(.x)) == "m^2"),
          ~ .x / 1e6
        )
      ) |>
      dplyr::mutate(
        dplyr::across(where(~ inherits(.x, "units")), as.numeric)
      )

    df11[is.na(df11)] <- 0

    # -------------------------
    # Output directory
    # -------------------------
    out_dir <- file.path(folder, "outputs", paste0("v", i))
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

    # -------------------------
    # Target column detection
    # -------------------------
    cols <- names(df11)[grepl("^area_deter_m2_lagatual_2022_\\d{2}$", names(df11))]

    if (length(cols) == 0) {
      stop("Target column not found. Check naming pattern.")
    }

    col_sym <- rlang::sym(cols[1])

    # -------------------------
    # Custom data loader
    # -------------------------
    read_data <- function(raw = FALSE) {

      data_tb <- df11 |>
        dplyr::mutate(
          def = !!col_sym * 1000^2,
          log_def = log(!!col_sym)
        )

      if (!raw) {
        data_tb <- dplyr::filter(data_tb, def > 0)
      }

      return(data_tb)
    }

    # -------------------------
    # Model formula
    # -------------------------
    predictors <- df11 |>
      dplyr::select(-OBJECTID) |>
      dplyr::select(where(is.numeric), -!!col_sym) |>
      names()

    my_formula <- stats::reformulate(
      termlabels = predictors,
      response = "log_def"
    )

    build_formula <- function() {
      my_formula
    }

    # -------------------------
    # Inject into model package
    # -------------------------
    assignInNamespace(".read_data", read_data,
                      ns = "prioritizedeforestationhotspots")

    assignInNamespace(".build_formula", build_formula,
                      ns = "prioritizedeforestationhotspots")

    # -------------------------
    # Run model
    # -------------------------
    prioritizedeforestationhotspots::fit_model(out_dir)

    # -------------------------
    # Load results
    # -------------------------
    results_tb <- readRDS(
      list.files(out_dir,
                 pattern = "new_data_tb.rds",
                 full.names = TRUE)[1]
    ) |>
      dplyr::mutate(pred_def_km2 = pred_def / 1e6)

    # -------------------------
    # Classification
    # -------------------------
    probs <- c(0, 0.6, 0.95, 1)
    labels <- c("Low", "Average", "High")

    results_year <- results_tb |>
      dplyr::mutate(
        priority = cut(
          pred_def_km2,
          breaks = stats::quantile(pred_def_km2, probs = probs),
          labels = labels,
          include.lowest = TRUE
        )
      )

    # -------------------------
    # Spatial output
    # -------------------------
    results_year_tmp <- prioritizedeforestationhotspots::deforestation_grid |>
      dplyr::left_join(
        dplyr::rename(results_year, id = OBJECTID),
        by = "id"
      )

    sf::st_write(
      results_year_tmp,
      file.path(out_dir, "priorizacao.gpkg"),
      delete_dsn = TRUE,
      quiet = TRUE
    )
  }

  message("Model run completed.")
}
