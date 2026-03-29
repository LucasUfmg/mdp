#' Ensure CRS is defined and transformed
#'
#' @param x An sf object
#' @param target_crs Target CRS (default = 4326)
#' @param default_crs CRS to assign if missing (default = 4326)
#'
#' @return sf object
#' @export
ensure_crs <- function(x, target_crs = 4326, default_crs = 4326) {

  if (is.na(sf::st_crs(x))) {
    message("CRS missing → assigning default")
    sf::st_crs(x) <- default_crs
  }

  if (!is.null(target_crs)) {
    x <- sf::st_transform(x, target_crs)
  }

  return(x)
}
