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
