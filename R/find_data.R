#' Find deter_data folder automatically
#'
#' Searches for deter_data folder in common locations.
#'
#' @param base_path Optional user-provided path
#'
#' @return Path to deter_data/base/shp
#' @export
find_deter_data <- function(base_path = NULL) {

  # 1. If user provides path → use it
  if (!is.null(base_path)) {

    shp_path <- file.path(base_path, "base", "shp")

    if (dir.exists(shp_path)) {
      return(normalizePath(shp_path))
    } else {
      stop("Provided path does not contain base/shp structure")
    }
  }

  # 2. Try current working directory
  candidate1 <- file.path(getwd(), "deter_data", "base", "shp")

  # 3. Try home directory
  candidate2 <- file.path("~", "deter_data", "base", "shp")

  # 4. Try project root (if using here)
  candidate3 <- tryCatch({
    file.path(here::here(), "deter_data", "base", "shp")
  }, error = function(e) NULL)

  candidates <- c(candidate1, candidate2, candidate3)

  for (path in candidates) {
    if (!is.null(path) && dir.exists(path)) {
      return(normalizePath(path))
    }
  }

  stop(
    "Could not find 'deter_data/base/shp'.\n",
    "Please either:\n",
    "1. Place deter_data in your working directory\n",
    "2. Place it in your home directory\n",
    "3. Or pass base_path explicitly"
  )
}
