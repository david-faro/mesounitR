#' Create Wetted Channel Polygon from TIN Mesh
#'
#' Extracts the polygon of the wetted area from a computational mesh.
#' Optionally removes small holes from the resulting geometry.
#'
#' @param flow An `sf` object representing a mesh.
#' @param max_hole_area Numeric. Maximum hole area threshold. Holes smaller than this value will be filled (removed).
#'
#' @return An `sf` polygon object representing the wetted channel, with small holes removed.
#'
#' @details
#' The function merges all mesh polygons into a single polygon geometry. Holes smaller than `max_hole_area` are removed using `nngeo::st_remove_holes()`.
#'
#' @examples
#' \dontrun{
#' wetted <- flow_to_wetchannel(mesh, max_hole_area = 2)
#' plot(wetted)
#' }
#'
#' @export

flow_to_wetchannel <- function(flow_wet,max_hole_area) {

  if (!inherits(flow_wet, "sf")) stop("`mesh` must be an sf object.")
  if (!is.numeric(max_hole_area) || max_hole_area <= 0) stop("`max_hole_area` must be a positive numeric value.")

  # create dummy grouping variable
  flow_wet$group <- 1

  # separate into dry and wet polygons
  flow_poly_wet <- flow_wet %>%
    group_by(group) %>%
    summarise()

  # remove holes
  flow_poly_wet <- nngeo::st_remove_holes(flow_poly_wet,max_hole_area)

  return(flow_poly_wet)

}
