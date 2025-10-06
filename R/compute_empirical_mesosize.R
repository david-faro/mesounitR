#' Compute empirical mesoscale size parameters
#'
#' This function computes the normalized meso sizes from a spatial object (mesohabitat survey)
#' and returns the log-normal parameters (mean and standard deviation) of their distribution.
#'
#' @param hmu_map A `sf` object representing surveyed mesohabitats.
#' @param W A numeric scalar representing the average wetted channel width
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{c_logmean}{The mean of the log-transformed normalized meso sizes.}
#'   \item{c_logsd}{The standard deviation of the log-transformed normalized meso sizes.}
#' }
#'
#' @examples
#' \dontrun{
#' library(sf)
#' # Assuming `hmu_map` is an sf object and domain width is known
#' compute_empirical_mesosize(hmu_map, W = 1000)
#' }
#'
#' @export
compute_empirical_mesosize <- function(hmu_map,W) {

  # --- Check hmu_map ---
  if (!inherits(hmu_map, "sf")) {
    stop("`hmu_map` must be an sf object.")
  }

  geom_type <- unique(as.character(sf::st_geometry_type(hmu_map)))
  if (!any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("`hmu_map` must contain polygon geometries (POLYGON or MULTIPOLYGON).")
  }

  if (nrow(hmu_map) == 0) {
    stop("`hmu_map` cannot be empty.")
  }

  if (any(!sf::st_is_valid(hmu_map))) {
    warning("`hmu_map` contains invalid geometries.")
  }

  # --- Check W ---
  if (!is.numeric(W)) {
    stop("`W` must be numeric.")
  }

  if (length(W) != 1) {
    warning("`W` should be a single numeric value.")
  }

  if (is.na(W) || W <= 0) {
    stop("`W` must be a positive numeric value.")
  }

  #### continue with main function body ####

  # compute area
  W_area <- W^2

  # compute normalized meso sizes c
  c <- as.numeric(st_area(hmu_map))/W_area

  # compute parameters of log-normal distribution
  c_logmean <- mean(log(c))
  c_logsd <- sd(log(c))

  # return results
  data.frame(c_logmean = c_logmean,
             c_logsd = c_logsd,
             W_area = W_area)
}
