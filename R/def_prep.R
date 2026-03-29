#' Operational data preparation (optimized)
#'
#' @param folder Path to shapefiles folder
#' @param ano_inicial Initial year
#' @param ano_final Final year
#' @param mes_inicial Initial month
#' @param mes_final Final month
#'
#' @return data.frame with aggregated variables
#' @export
def_prep <- function(folder, ano_inicial, ano_final, mes_inicial, mes_final) {

  sf::sf_use_s2(FALSE)

  # -------------------------
  # Ensure output folder
  # -------------------------
  if (!dir.exists(file.path(folder, "tabelas"))) {
    dir.create(file.path(folder, "tabelas"))
  }

  # -------------------------
  # Read data
  # -------------------------
  deter <- sf::st_read(file.path(folder, "deter-amz-deter-public.shp"), quiet = TRUE)
  deter <- ensure_crs(deter)

  qt <- sf::st_read(file.path(folder, "Grade_Random_Forest.shp"), quiet = TRUE)
  qt <- ensure_crs(qt, sf::st_crs(deter))

  df_focos <- sf::read_sf(file.path(folder, "focos_2016_2024.shp"))
  df_focos <- ensure_crs(df_focos, sf::st_crs(deter))

  points <- sf::st_read(file.path(folder, "Pontos.shp"), quiet = TRUE)
  points <- ensure_crs(points, sf::st_crs(deter))

  q_deter <- qt

  # -------------------------
  # Preprocess DETER
  # -------------------------
  deter <- deter |>
    dplyr::mutate(
      year = lubridate::year(VIEW_DATE),
      month = lubridate::month(VIEW_DATE)
    ) |>
    dplyr::filter(
      CLASSNAME %in% c(
        "DESMATAMENTO_CR",
        "DESMATAMENTO_VEG",
        "MINERACAO"
      )
    )

  # -------------------------
  # Storage
  # -------------------------
  results <- list()
  counter <- 1

  # -------------------------
  # Main loops
  # -------------------------
  for (i in ano_inicial:ano_final) {

    for (j in mes_inicial:mes_final) {

      # -------------------------
      # FOCOS aggregation (ONCE per month)
      # -------------------------
      focos_grid <- df_focos |>
        dplyr::filter(Month == j) |>
        sf::st_join(qt) |>
        sf::st_drop_geometry() |>
        dplyr::group_by(Id) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::rename(OBJECTID = Id)

      # -------------------------
      # Distance (ONCE per month)
      # -------------------------
      nearest_id <- sf::st_nearest_feature(points, q_deter)
      dist_m <- sf::st_distance(points, q_deter[nearest_id, ], by_element = TRUE)

      points$OBJECTID <- q_deter$OBJECTID[nearest_id]
      points$dist_m <- as.numeric(dist_m)

      points_no_geom <- sf::st_drop_geometry(points)

      # -------------------------
      # Lag loop
      # -------------------------
      for (k in c(-1, 0, 1, 3)) {

        # -------------------------
        # Filter DETER (FAST now)
        # -------------------------
        if (k == -1) {

          d <- deter |>
            dplyr::filter(
              year == i,
              month == j
            )

        } else {

          d <- deter |>
            dplyr::filter(
              year >= (i - 1 - k),
              year <= (i - 1),
              month == j
            )
        }

        # -------------------------
        # Aggregate deforestation
        # -------------------------
        patches_in_region <- q_deter |>
          dplyr::left_join(
            d |>
              dplyr::mutate(area_m2 = as.numeric(sf::st_area(geometry))) |>
              sf::st_join(q_deter) |>
              sf::st_drop_geometry() |>
              dplyr::group_by(OBJECTID) |>
              dplyr::summarise(s = sum(area_m2, na.rm = TRUE), .groups = "drop"),
            by = "OBJECTID"
          ) |>
          dplyr::mutate(s = ifelse(is.na(s), 0, s))

        # -------------------------
        # Merge focos + deter + distance
        # -------------------------
        df_final <- focos_grid |>
          dplyr::left_join(
            patches_in_region |>
              sf::st_drop_geometry() |>
              dplyr::select(OBJECTID, s),
            by = "OBJECTID"
          ) |>
          dplyr::left_join(
            points_no_geom |>
              dplyr::select(OBJECTID, dist_m),
            by = "OBJECTID"
          )

        df_final <- df_final |>
          dplyr::rename(
            focos = n,
            area_deter_m2 = s
          ) |>
          dplyr::mutate(
            year = i,
            Month = j,
            lag = k
          )

        # -------------------------
        # Store
        # -------------------------
        results[[counter]] <- df_final
        counter <- counter + 1
      }
    }
  }

  # -------------------------
  # Final bind
  # -------------------------
  final_df <- dplyr::bind_rows(results)

  # -------------------------
  # Save output
  # -------------------------
  output_path <- file.path(
    folder,
    "tabelas",
    paste0("df_operacionalizado_", ano_inicial, "_", ano_final, ".csv")
  )

  utils::write.csv(final_df, output_path, row.names = FALSE)

  message("Saved: ", output_path)

  return(final_df)
}


usethis::use_package("ranger")
usethis::use_package("dplyr")
usethis::use_package("tidyr")
