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
