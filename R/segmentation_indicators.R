#' Compute Segmentation Indicators (Moran's I and Area-Weighted Variance)
#'
#' Calculates segmentation quality indicators — Moran's I (`MI`) and area-weighted variance (`v`) —
#' for spatial units derived from the function `supercell_to_units()`.
#'
#' @param units An `sf` polygon object representing segmented spatial units.
#' @param flow_points An `sf` object with point geometries (e.g., modelled flow centroids or nodes) containing the column variable.
#' @param variable A character string indicating the name of the column in `flow_points` to use for computing the metrics
#' (currently designed for the variable velocity `"VEL"`).
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{MI}{Global Moran's I.}
#'   \item{v}{Area-weighted variance.}
#' }
#'
#' @details
#' The function computes MI and v for each spatial unit
#'
#' @examples
#' \dontrun{
#' seg_units <- supercell_to_units(...)     # segmentation output as sf polygons
#' flow_pts <- your_modelled_flow_points    # sf points with a VEL column
#' segmentation_indicators(seg_units, flow_pts, "VEL")
#' }
#'
#' @export

segmentation_indicators <- function(units,flow_points,variable='VEL') {

  # --- Check units ---
  if (!inherits(units, "sf")) {
    stop("`units` must be an sf object.")
  }
  geom_type <- unique(as.character(sf::st_geometry_type(units)))
  if (!any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("`units` must contain polygon geometries (POLYGON or MULTIPOLYGON).")
  }
  if (nrow(units) == 0) {
    stop("`units` cannot be empty.")
  }

  # --- Check flow_points ---
  if (!inherits(flow_points, "sf")) {
    stop("`flow_points` must be an sf object.")
  }
  geom_type <- unique(as.character(sf::st_geometry_type(flow_points)))
  if (!any(geom_type %in% c("POINT", "MULTIPOINT"))) {
    stop("`flow_points` must contain point geometries (POINT or MULTIPOINT).")
  }

  # --- Check variable ---
  if (!is.character(variable)) {
    stop("`variable` must be of type character.")
  }

  #### main function body ####

  n <- nrow(units)

  ngbrs <- spdep::poly2nb(units)
  w <- spdep::nb2mat(ngbrs,style="B",zero.policy = TRUE)

  units$id_poly <- 1:n

  units_variable_mean <- numeric()
  units_variable_var <- numeric()

  # compute unit values

  pts_cont <- sf::st_contains(units,flow_points) # select points contained within units

  for (j in 1:n) {
    id_pts_in_poly <- pts_cont[[j]]
    pts_in_poly <- flow_points[id_pts_in_poly,]

    pts_in_poly <- as.data.frame(pts_in_poly)

    units_variable_mean[j] <- mean(pts_in_poly[,variable]) # compute average
    units_variable_var[j] <- var(pts_in_poly[,variable]) # compute variance
  }

  flow_points <- as.data.frame(flow_points)

  all_variable <- mean(flow_points[,variable])

  # compute areas of segments
  units_areas <- as.numeric(st_area(units))

  MI_num <- 0
  MI_den <- 0

  for (i in 1:n) {
    for (j in 1:n) {
      MI_num <- MI_num + w[i,j]*(units_variable_mean[i]-all_variable)*(units_variable_mean[j]-all_variable)
    }
    MI_den <- MI_den + (units_variable_mean[i]-all_variable)^2

  }

  MI_num <- n*MI_num
  MI_den <- MI_den*sum(w[row(w)!=col(w)])

  # global Moran's I
  MI <- MI_num/MI_den

  # area weighted variance
  v <- sum(units_areas*units_variable_var)/sum(units_areas)

  return(data.frame(MI=MI,v=v))

}
