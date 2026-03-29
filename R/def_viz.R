#' Visualization and validation of prioritization
#'
#' @param folder Base folder path
#' @param mes_inicial Initial month
#' @param mes_final Final month
#'
#' @export
def_viz <- function(folder, mes_inicial, mes_final) {

  sf::sf_use_s2(FALSE)

  # -------------------------
  # Load data
  # -------------------------
  deter <- sf::st_read(
    file.path(folder, "deter-amz-deter-public.shp"),
    quiet = TRUE
  )

  pf <- utils::read.csv(
    file.path(folder, "tabelas", "perc_floresta_por_grid.csv")
  ) |>
    dplyr::rename(id = OBJECTID)

  # Preprocess deter ONCE
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
  # Storage (FAST)
  # -------------------------
  result_list <- list()
  dff_list <- list()
  counter <- 1

  # -------------------------
  # Loop over months
  # -------------------------
  for (i in mes_inicial:mes_final) {

    prio <- sf::st_read(
      file.path(folder, "outputs", paste0("v", i), "priorizacao.gpkg"),
      quiet = TRUE
    )

    prio <- prio |>
      dplyr::mutate(
        priority1 = ifelse(is.na(priority), "Low", as.character(priority))
      )

    # Align CRS
    if (sf::st_crs(prio) != sf::st_crs(deter)) {
      prio <- sf::st_transform(prio, sf::st_crs(deter))
    }

    # -------------------------
    # Filter deter (simplified)
    # -------------------------
    d <- deter |>
      dplyr::filter(
        year == 2022,
        month == i
      ) |>
      sf::st_make_valid()

    # -------------------------
    # Plot priority map
    # -------------------------
    grDevices::png(
      file.path(folder, "outputs", paste0("v", i), paste0("plot_", i, ".png")),
      width = 3000, height = 2400, res = 300
    )

    print(
      ggplot2::ggplot(prio) +
        ggplot2::geom_sf(ggplot2::aes(fill = priority1)) +
        ggplot2::geom_sf(data = d, fill = NA, color = "black") +
        ggplot2::theme_minimal()
    )

    grDevices::dev.off()

    # -------------------------
    # Intersection
    # -------------------------
    patches_touching1 <- d |>
      sf::st_join(
        prio |> dplyr::select(priority),
        join = sf::st_intersects,
        left = FALSE
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(
        area_m2 = as.numeric(sf::st_area(sf::st_geometry(geometry)))
      ) |>
      sf::st_drop_geometry()

    # -------------------------
    # Commission error
    # -------------------------
    has_deter <- lengths(sf::st_intersects(prio, d)) > 0

    grid_with_patches <- prio |>
      dplyr::mutate(
        has_patch = has_deter,
        category = dplyr::case_when(
          priority1 == "High" & has_patch ~ "High + Patch",
          priority1 == "High" & !has_patch ~ "High + No Patch",
          TRUE ~ "Other"
        )
      )

    # -------------------------
    # Plot commission map
    # -------------------------
    grDevices::png(
      file.path(folder, "outputs", paste0("v", i), paste0("erro_comissao_MAPA_", i, ".png")),
      width = 3000, height = 2400, res = 300
    )

    print(
      ggplot2::ggplot() +
        ggplot2::geom_sf(data = grid_with_patches, ggplot2::aes(fill = category)) +
        ggplot2::geom_sf(data = d, fill = NA, color = "black") +
        ggplot2::scale_fill_manual(values = c(
          "High + Patch" = "green",
          "High + No Patch" = "red",
          "Other" = "lightgrey"
        )) +
        ggplot2::theme_minimal()
    )

    grDevices::dev.off()

    # -------------------------
    # Metrics
    # -------------------------
    prio1 <- prio |>
      dplyr::mutate(has_deter = has_deter)

    prio3 <- merge(prio1, pf)

    prio_summary <- prio3 |>
      dplyr::filter(interaction(priority, has_deter) %in% c("High.FALSE", "High.TRUE")) |>
      dplyr::group_by(interaction(priority, has_deter)) |>
      dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(month = i)

    dff_list[[counter]] <- prio_summary

    # Commission rate
    high_cells <- grid_with_patches |>
      dplyr::filter(priority == "High")

    n_total_high <- nrow(high_cells)

    n_commission <- grid_with_patches |>
      dplyr::filter(priority == "High" & !has_patch) |>
      nrow()

    commission_rate <- n_commission / n_total_high

    area_by_region <- patches_touching1 |>
      dplyr::group_by(priority) |>
      dplyr::summarise(area_tot = sum(area_m2), .groups = "drop") |>
      dplyr::mutate(
        perc = 100 * area_tot / sum(area_tot),
        ref_yer = i,
        comission_rate = commission_rate
      )

    result_list[[counter]] <- area_by_region
    counter <- counter + 1
  }

  # -------------------------
  # Bind results
  # -------------------------
  result <- dplyr::bind_rows(result_list)
  dff <- dplyr::bind_rows(dff_list)

  # -------------------------
  # Plot % deter
  # -------------------------
  grDevices::png(
    file.path(folder, "outputs", "plot_percentual.png"),
    width = 3000, height = 3000, res = 300
  )

  print(
    ggplot2::ggplot(result) +
      ggplot2::aes(x = ref_yer, y = perc, color = priority) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::theme_classic()
  )

  grDevices::dev.off()

  # -------------------------
  # Commission curve
  # -------------------------
  grDevices::png(
    file.path(folder, "outputs", "comissao.png"),
    width = 3000, height = 3000, res = 300
  )

  print(
    result |>
      dplyr::group_by(ref_yer) |>
      dplyr::summarise(n = mean(comission_rate * 100)) |>
      ggplot2::ggplot() +
      ggplot2::aes(x = ref_yer, y = n) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::theme_classic()
  )

  grDevices::dev.off()

  message("Visualization completed.")
}


usethis::use_package("sf")
usethis::use_package("dplyr")
usethis::use_package("ggplot2")
usethis::use_package("lubridate")
