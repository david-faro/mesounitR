#' Generate Transects from a Channel Centerline
#'
#' This function creates evenly spaced transects (cross-sections) perpendicular
#' to a channel centerline
#'
#' @param centerline An `sf` LINESTRING object representing the channel centerline.
#' @param spacing Numeric. Longitudinal distance between transects (in CRS units).
#' @param trans_width Numeric. Width of each transect (in CRS units).
#'
#' @return An `sf` object containing LINESTRING features for each transect.
#'
#' @details
#' Internally, this function computes evenly spaced points along the centerline
#' using the helper function `evenspace()`, and then generates perpendicular
#' transects using the helper function `transect()`. Each transect is created
#' as an individual LINESTRING in the same coordinate reference system (CRS) as
#' the input centerline.
#'
#' @examples
#' # Assuming 'centerline' is an sf LINESTRING object:
#' # transects <- centerline_to_transects(centerline, spacing = 50, trans_width = 20)
#'
#' @export
centerline_to_transects <- function(centerline,spacing,trans_width) {

  # extract centerline coordinates
  centerline_coord <- sf::st_coordinates(centerline)

  # Generate evenly spaced points along the centerline
  tspts = evenspace(centerline_coord,spacing)

  # Generate perpendicular transect line endpoints
  tslines = transect(tspts,trans_width)

  # Create list of LINESTRING geometries for each transect
  transects <- as.matrix(data.frame(x = c(tslines$x0[1],tslines$x1[1]),
                                    y = c(tslines$y0[1],tslines$y1[1]))) %>%
    sf::st_linestring() %>%
    sf::st_sfc(crs = sf::st_crs(centerline)) %>%
    sf::st_sf()

  for (i in 2:nrow(tslines)) {

    trans_new <- as.matrix(data.frame(x = c(tslines$x0[i],tslines$x1[i]),
                                      y = c(tslines$y0[i],tslines$y1[i]))) %>%
      sf::st_linestring() %>%
      st_sfc(crs = st_crs(centerline)) %>%
      st_sf()

    transects <- rbind(transects,
                       trans_new)

  }

  return(transects)

}
