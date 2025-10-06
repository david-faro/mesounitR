#' Compute Average Channel Width from Wetted Channel Polygon
#'
#' This function calculates the average channel width by intersecting evenly spaced
#' transects with a polygon representing the wetted channel.
#'
#' @param poly_wetchannel An `sf` POLYGON object representing the wetted channel area.
#' @param centerline An `sf` LINESTRING object representing the channel centerline.
#' @param trans_longdist Numeric. Longitudinal spacing between transects (default is 10, in CRS units).
#' @param trans_width Numeric. Width of transects to extend on either side of the centerline
#'                    (default is 300, in CRS units).
#'
#' @return Numeric value representing the **average channel width** based on the intersections of transects.
#'
#' @details
#' - The function uses `centerline_to_transects()` to generate perpendicular cross-sections.
#' - These transects are intersected with the wetted channel polygon to extract segments.
#' - The average length of these intersected segments is returned as the representative channel width.
#'
#' @examples
#' # Example usage:
#' # avg_width <- compute_widths(poly_wetchannel, centerline, trans_longdist = 10, trans_width = 300)
#'
#' @export
compute_widths <- function(poly_wetchannel,centerline,trans_longdist=10,trans_width=300) {

  # --- Check poly_wetchannel ---
  if (!inherits(poly_wetchannel, "sf")) {
    stop("`poly_wetchannel` must be an sf object.")
  }

  geom_type <- unique(as.character(sf::st_geometry_type(poly_wetchannel)))
  if (!any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("`poly_wetchannel` must contain polygon geometries (POLYGON or MULTIPOLYGON).")
  }

  if (nrow(poly_wetchannel) > 1) {
    warning("`poly_wetchannel` contains more than one polygon — only the first may be used.")
  }

  # --- Check centerline ---
  if (!inherits(centerline, "sf")) {
    stop("`centerline` must be an sf object.")
  }
  geom_type <- unique(as.character(sf::st_geometry_type(centerline)))
  if (!any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
    stop("`centerline` must contain line geometries (LINESTRING or MULTILINESTRING).")
  }

  if (nrow(centerline) > 1) {
    warning("`centerline` contains more than one polygon — only the first may be used.")
  }

  # --- Helper for numeric scalar > 0 ---
  check_single_positive <- function(x, name) {
    if (!is.numeric(x)) stop(paste0("`", name, "` must be numeric."))
    if (length(x) != 1) stop(paste0("`", name, "` must be a single numeric value."))
    if (is.na(x) || x <= 0) stop(paste0("`", name, "` must be a positive numeric value."))
  }

  # --- Check trans_longdist ---
  check_single_positive(trans_longdist, "trans_longdist")

  # --- Check trans_width ---
  check_single_positive(trans_width, "trans_width")

  #### main function body ####

  # Generate evenly spaced transects perpendicular to the centerline
  transects <- centerline_to_transects(centerline,trans_longdist,trans_width)

  # Intersect transects with the wetted channel polygon
  trans_inters <- st_intersection(st_make_valid(poly_wetchannel),transects)

  # Compute width as the length of each intersected segment
  widths <- as.numeric(st_length(trans_inters))

  # plot transects
  plot(trans_inters)

  # return average width value
  return(mean(widths, na.rm = T))
}
