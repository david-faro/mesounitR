#' Compute Normalized Optimality Metrics (v,MI,ms) and combine into Global Score
#'
#' This function calculates three normalized optimality metrics, the Moran's I (MI), weighted averaged variance (v), and meso-size index (ms), for a list of mosaic regions and combines
#' them into a Global Score (GS).
#'
#' @param flow An `sf` object (sfc POLYGON) representing modeled flow areas (e.g., hydraulic model elements).
#' @param flow_regions A list of mosaic region groupings (output from `supercell_to_units()`).
#' @param n_range A numeric vector of unit counts associated with each mosaic.
#' @param c_params A data frame containing the parameters W_area, c_logmean and c_logsd
#' @param A_tot A numeric vector with the total (wetted) channel area
#'
#' @return A data frame containing:
#' \describe{
#'   \item{n}{Number of units per mosaic.}
#'   \item{MI}{Normalized Moran's I}
#'   \item{v}{Normalized weighted averaged variance}
#'   \item{ms}{Normalized meso-size index}
#'   \item{GS}{Global Score, calculated as the mean of MI, v, and ms.}
#' }
#'
#' @details
#' - MI and v are computed using the `segmentation_indicators()` function on each mosaic.
#' - ms is calculated based on an expected log-normal distribution.
#' - All three metrics are normalized using `sp_norm()` and combined into a global performance score (GS).
#'
#' @examples
#' # Assuming 'flow', 'flow_regions', 'n_range', and other parameters are defined:
#' # scores <- compute_optimality_metrics(flow, flow_regions, n_range, W_area, c_logmean, c_logsd)
#'
#' @export
compute_optimality_metrics <- function(flow, flow_regions,n_range,c_params,A_tot) {

  W_area <- c_params$W_area
  c_logmean <- c_params$c_logmean
  c_logsd <- c_params$c_logsd

  # Initialize metric vectors
  MI <- numeric()
  v <- numeric()

  # extract centroids from polygons
  flow_pts <- sf::st_centroid(flow)

  # compute MI and v for each mosaic
  for (i in 1:length(n_range)) {

    regions_i <- flow_regions[[i]]

    indicator <- segmentation_indicators(regions_i,flow_pts,"VEL")

    MI[i] <- indicator$MI
    v[i] <- indicator$v

  }

  # compute ms index

  ms <- compute_ms(c_logmean,c_logsd,W_area,A_tot,n_range,n_iter = 500)$ms

  # normalize metrics to [0,1]
  MI = sp_norm(MI,min(MI,na.rm=T),max(MI,na.rm=T))
  v = sp_norm(v,min(v,na.rm=T),max(v,na.rm=T))
  ms = sp_norm(ms,min(ms,na.rm=T),max(ms,na.rm=T))

  return(data.frame(n = n_range,
                    MI = MI,
                    v = v,
                    ms = ms,
                    GS = (MI + v + ms)/3))

}
