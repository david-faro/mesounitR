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
    warning("`poly_wetchannel` contains more than one polygon - only the first may be used.")
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
    warning("`centerline` contains more than one polygon - only the first may be used.")
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

# --------------------------------------------------------------------
# Internal helper functions (not exported)
# --------------------------------------------------------------------

# Evenly Space Points Along a Line
evenspace <- function(xy, sep, start=0){

  dx <- c(0,diff(xy[,1]))
  dy <- c(0,diff(xy[,2]))
  dseg <- sqrt(dx^2+dy^2)
  dtotal <- cumsum(dseg)

  linelength = sum(dseg)

  pos = seq(start,linelength, by=sep)

  whichseg = unlist(lapply(pos, function(x){sum(dtotal<=x)}))

  pos=data.frame(pos=pos,whichseg=whichseg,
                 x0=xy[whichseg,1],
                 y0=xy[whichseg,2],
                 dseg = dseg[whichseg+1],
                 dtotal = dtotal[whichseg],
                 x1=xy[whichseg+1,1],
                 y1=xy[whichseg+1,2]
  )

  pos$further =  pos$pos - pos$dtotal
  pos$f = pos$further/pos$dseg
  pos$x = pos$x0 + pos$f * (pos$x1-pos$x0)
  pos$y = pos$y0 + pos$f * (pos$y1-pos$y0)

  pos$theta = atan2(pos$y0-pos$y1,pos$x0-pos$x1)

  return(pos[,c("x","y","x0","y0","x1","y1","theta")])

}

# Generate Transects from a Channel Centerline
centerline_to_transects <- function(centerline,spacing=10,trans_width=300) {

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
      sf::st_sfc(crs = st_crs(centerline)) %>%
      sf::st_sf()

    transects <- rbind(transects,
                       trans_new)

  }

  return(transects)

}

# Compute Transects Perpendicular to a Line
transect <- function( tpts, tlen){

  tpts$thetaT = tpts$theta+pi/2
  dx = tlen*cos(tpts$thetaT)
  dy = tlen*sin(tpts$thetaT)
  return(
    data.frame(x0 = tpts$x + dx,
               y0 = tpts$y + dy,
               x1 = tpts$x - dx,
               y1 = tpts$y -dy)
  )

}
