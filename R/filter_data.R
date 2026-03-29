#' Filter datasets automatically by period
#'
#' Detects dataset type (e.g., deter, focos) and applies appropriate filtering.
#'
#' @param data_list Named list of sf objects
#' @param ano_inicial Initial year
#' @param ano_final Final year
#' @param mes_inicial Initial month
#' @param mes_final Final month
#'
#' @return List of sf objects
#' @export
filter_datasets <- function(
    data_list,
    ano_inicial,
    ano_final,
    mes_inicial,
    mes_final
) {

  for (i in seq_along(data_list)) {

    dataset_name <- names(data_list)[i]
    rule <- detect_filter_rule(dataset_name)

    if (is.null(rule)) next

    # DATE case (deter)
    if (rule$type == "date") {

      data_list[[i]] <- data_list[[i]] |>
        dplyr::mutate(
          date_tmp = lubridate::ymd(.data[[rule$column]]),
          ano_tmp  = lubridate::year(date_tmp),
          mes_tmp  = lubridate::month(date_tmp)
        ) |>
        dplyr::filter(
          ano_tmp >= ano_inicial,
          ano_tmp <= ano_final,
          mes_tmp >= mes_inicial,
          mes_tmp <= mes_final
        ) |>
        dplyr::select(-date_tmp, -ano_tmp, -mes_tmp)
    }

    # YEAR + MONTH case (focos)
    if (rule$type == "year_month") {

      data_list[[i]] <- data_list[[i]] |>
        dplyr::mutate(
          ano_tmp = as.integer(.data[[rule$year]]),
          mes_tmp = as.integer(.data[[rule$month]])
        ) |>
        dplyr::filter(
          ano_tmp >= ano_inicial,
          ano_tmp <= ano_final,
          mes_tmp >= mes_inicial,
          mes_tmp <= mes_final
        ) |>
        dplyr::select(-ano_tmp, -mes_tmp)
    }
  }

  return(data_list)
}
