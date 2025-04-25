#' Create Wetted Channel Polygon from TIN Mesh
#'
#' Extracts the polygon of the wetted area from a computational mesh using a depth threshold.
#' Optionally removes small holes from the resulting geometry.
#'
#' @param flow An `sf` object representing a mesh with a `DEPTH` column indicating water depth.
#' @param depth_min Numeric. Minimum depth threshold used to differentiate between wet and dry areas.
#' @param max_hole_area Numeric. Maximum hole area threshold. Holes smaller than this value will be filled (removed).
#'
#' @return An `sf` polygon object representing the wetted channel, with small holes removed.
#'
#' @details
#' The function flags all mesh elements with depth greater than or equal to `depth_min` as "wet", then groups and merges them
#' into a single polygon geometry. Holes smaller than `max_hole_area` are removed using `nngeo::st_remove_holes()`.
#'
#' @examples
#' \dontrun{
#' # Assuming 'mesh' is an sf object with a 'DEPTH' column
#' wetted <- flow_to_wetchannel(mesh, depth_min = 0.1, max_hole_area = 2)
#' plot(wetted)
#' }
#'
#' @export

flow_to_wetchannel <- function(flow,depth_min,max_hole_area) {

  if (!inherits(flow, "sf")) stop("`mesh` must be an sf object.")
  if (!all(c("DEPTH") %in% names(flow))) stop("`flow` must have 'DEPTH' column.")
  if (!is.numeric(depth_min) || depth_min <= 0) stop("`depth_min` must be a positive numeric value.")
  if (!is.numeric(max_hole_area) || max_hole_area <= 0) stop("`max_hole_area` must be a positive numeric value.")

  # create dry and wet variable
  flow$wet <- 0
  flow$wet[flow$DEPTH>=depth_min] <- 1

  # separate into dry and wet polygons
  flow_poly_wet <- flow %>%
    group_by(wet) %>%
    summarise()

  # remove holes
  flow_poly_wet <- nngeo::st_remove_holes(flow_poly_wet,max_hole_area)

  flow_poly_wet <- flow_poly_wet %>%
    filter(wet == 1)

  return(flow_poly_wet)

}
