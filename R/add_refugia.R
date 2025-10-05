#' Add Refugia Attributes to Mesohabitat Polygons
#'
#' This function adds refuge-related attributes (e.g., boulders, vegetation, roots)
#' and hydraulic connectivity information to mesohabitat polygons, and saves the
#' result as a shapefile compatible with SimStream or MesoHABSIM workflows.
#'
#' @param poly_mesohabitats An \code{sf} object with mesohabitat polygons.
#' @param flow An \code{sf} object with flow points containing at least the columns
#'   \code{Z} (bed elevation) and \code{DEPTH} (water depth).
#' @param poly_BOULDER,poly_CANOP_SHAD,poly_OVERHA_VEG,poly_ROOTS,
#'   poly_SUBMER_VEG,poly_EMERG_VEG,poly_UNDERC_BAN,poly_WOODY_DEBR,
#'   poly_RIPRAP,poly_SHALL_MARG Optional \code{sf} polygon layers representing
#'   different types of refugia (may be \code{NULL}).
#' @param file_name Character string with the output shapefile path and name. Default: 'mesohabitats.shp'
#'
#' @return Invisibly returns the updated \code{sf} object with added refuge and
#'   connectivity attributes.
#'
#' @details
#' For each mesohabitat polygon, the function checks for spatial intersections
#' with various refugia polygons (if provided) and flags their presence as
#' `"True"` or `"False"`. Connectivity between neighboring units is determined by
#' testing for intersections after buffering each polygon by 3 meters.
#'
#' The minimum and maximum water surface elevation (WSE = Z + DEPTH) for each unit are
#' also computed from the flow points located within the polygon.
#'
#' @examples
#' \dontrun{
#' poly_mesoHABSIM <- add_refugia(
#'   poly_mesohabitats = poly_mesohabitats,
#'   flow = flow_ZDV,
#'   poly_BOULDER = poly_boulders,
#'   poly_CANOP_SHAD = poly_canopyshading,
#'   poly_OVERHA_VEG = poly_overhang,
#'   poly_ROOTS = poly_roots,
#'   poly_SUBMER_VEG = NULL,
#'   poly_EMERG_VEG = NULL,
#'   poly_UNDERC_BAN = NULL,
#'   poly_WOODY_DEBR = poly_woodydebris,
#'   poly_RIPRAP = NULL,
#'   poly_SHALL_MARG = NULL,
#'   file_name = "mesohabitats.shp"
#' )
#' }
#'
#' @export

add_refugia <- function(poly_mesohabitats,
                        flow,
                        poly_BOULDER,
                        poly_CANOP_SHAD,
                        poly_OVERHA_VEG,
                        poly_ROOTS,
                        poly_SUBMER_VEG,
                        poly_EMERG_VEG,
                        poly_UNDERC_BAN,
                        poly_WOODY_DEBR,
                        poly_RIPRAP,
                        poly_SHALL_MARG,
                        file_name='mesohabitats.shp') {

  # --- Check that required attributes exist in 'flow' ---
  required_cols <- c("DEPTH", "Z")
  missing_cols <- setdiff(required_cols, names(flow))

  if (length(missing_cols) > 0) {
    stop(paste0("In 'flow' attributes 'Z' and/or 'DEPTH' are missing."), call. = FALSE)
  }

  # --- Continue with main function body ---

  # add hmu num
  n <- nrow(poly_mesohabitats)
  poly_mesohabitats$HMU_NUM <- 1:n

  # add HMU_TYPE for modelled units, if not existing already
  if (!'HMU_TYPE' %in% colnames(poly_mesohabitats)) {
    poly_mesohabitats$HMU_TYPE <- 'MODELLED_UNITS'
  }

  # find refugia
  poly_mesohabitats$BOULDER <- find_refugia(poly_mesohabitats, poly_BOULDER)
  poly_mesohabitats$CANOP_SHAD <- find_refugia(poly_mesohabitats, poly_CANOP_SHAD)
  poly_mesohabitats$OVERHA_VEG <- find_refugia(poly_mesohabitats, poly_OVERHA_VEG)
  poly_mesohabitats$ROOTS <- find_refugia(poly_mesohabitats, poly_ROOTS)
  poly_mesohabitats$SUBMER_VEG <- find_refugia(poly_mesohabitats, poly_SUBMER_VEG)
  poly_mesohabitats$EMERG_VEG <- find_refugia(poly_mesohabitats, poly_EMERG_VEG)
  poly_mesohabitats$UNDERC_BAN <- find_refugia(poly_mesohabitats, poly_UNDERC_BAN)
  poly_mesohabitats$WOODY_DEBR <- find_refugia(poly_mesohabitats, poly_WOODY_DEBR)
  poly_mesohabitats$RIPRAP <- find_refugia(poly_mesohabitats, poly_RIPRAP)
  poly_mesohabitats$SHALL_MARG <- find_refugia(poly_mesohabitats, poly_SHALL_MARG)

  # find connectivity
  poly_mesohabitats$CONNECTIV <- find_connectivity(poly_mesohabitats, dist = 3)

  # add z_min and z_max
  poly_mesohabitats <- add_zmaxmin(poly_mesohabitats, flow)

  # add comments column --> empty
  poly_mesohabitats$COMMENT <- ''

  # select only needed columns

  poly_mesohabitats <- poly_mesohabitats[,c('geometry','HMU_NUM','HMU_TYPE','Z_MAX','Z_MIN','CONNECTIV','BOULDER','CANOP_SHAD',
                          'OVERHA_VEG','ROOTS','SUBMER_VEG','EMERG_VEG','UNDERC_BAN','WOODY_DEBR','RIPRAP',
                          'SHALL_MARG','COMMENT')]

  sf::st_write(poly_mesohabitats,file_name,delete_layer=TRUE)

  invisible(poly_mesohabitats)

}

# --------------------------------------------------------------------
# Internal helper functions (not exported)
# --------------------------------------------------------------------


## define function to find intersecting refugia
find_refugia <- function(poly_mesohabitats,poly_refugia) {

  # number of hmus
  n <- nrow(poly_mesohabitats)

  if (!is.null(poly_refugia)) {

    refugia <- vector()

    # find intersecting elements
    if_intersects <- sf::st_intersects(poly_mesohabitats, poly_refugia)

    # set 'True' or 'False' for each HMU if interecting refugia were found
    for (i in 1:n) {

      if (length(if_intersects[[i]]) > 0) {
        refugia[i] <- 'True'
      } else {
        refugia[i] <- 'False'
      }

    }

  } else {

    refugia <- rep('False',n)

  }


  return(refugia)

}


## define function for connectivity.
# a buffer of 3 m (unless otherwise stated) around the unit is created, in case of small gaps between polygons
find_connectivity <- function(poly_mesohabitats, dist = 3) {

  n <- nrow(poly_mesohabitats)

  connect <- vector()

  for (i in 1:n) {

    hmu <- poly_mesohabitats$geometry[i]
    other <- poly_mesohabitats$geometry[-i]

    if_intersects <- sf::st_intersects(st_buffer(hmu, dist),other)

    if (length(if_intersects[[1]]) > 0) {
      connect[i] <- 'True'
    } else  {
      connect[i] <- 'False'
    }

  }

  return(connect)

}


## find z_max and z_min
add_zmaxmin <- function(poly_mesohabitats, flow) {

  flow$wse <- flow$Z + flow$DEPTH

  n <- nrow(poly_mesohabitats)

  z_min <- vector()
  z_max <- vector()

  for (i in 1:n) {
    # select only flow points within unit i
    pts_touching <- as.matrix(sf::st_intersects(poly_mesohabitats[i,],flow))
    wse_pts <- flow[pts_touching,]

    z_min[i] <- min(wse_pts$wse)
    z_max[i] <- max(wse_pts$wse)

  }

  poly_mesohabitats$Z_MIN <- z_min
  poly_mesohabitats$Z_MAX <- z_max

  return(poly_mesohabitats)

}

