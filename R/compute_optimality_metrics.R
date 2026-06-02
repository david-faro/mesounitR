#' Compute Normalized Optimality Metrics (v,MI,mn) and combine into Global Score
#'
#' This function calculates three normalized optimality metrics, the Moran's I (MI), weighted averaged variance (v), and meso-number index (mn), for a list of mosaic regions and combines
#' them into a Global Score (GS).
#'
#' @param mesh An `sf` object (sfc POLYGON) representing modeled mesh areas (e.g., hydraulic model elements).
#' @param list_units A list of mosaic region groupings (output from `supercell_to_units()`).
#' @param n_range A numeric vector of unit counts associated with each mosaic.
#' @param c_params A data frame containing the parameters W_area, c_logmean and c_logsd
#' @param A_tot A numeric vector with the total (wetted) channel area
#'
#' @return A data frame containing:
#' \describe{
#'   \item{n}{Number of units per mosaic.}
#'   \item{MI}{Normalized Moran's I}
#'   \item{v}{Normalized weighted averaged variance}
#'   \item{mn}{Normalized meso-number index}
#'   \item{GS}{Global Score, calculated as the mean of MI, v, and mn.}
#' }
#'
#' @details
#' - MI and v are computed using the `segmentation_indicators()` function on each mosaic.
#' - mn is calculated based on an expected log-normal distribution of meso-sizes.
#' - All three metrics are normalized using `sp_norm()` and combined into a global performance score (GS).
#'
#' @examples
#' # Assuming 'mesh', 'list_units', 'n_range', and other parameters are defined:
#' # scores <- compute_optimality_metrics(mesh, list_units, n_range, W_area, c_logmean, c_logsd)
#'
#' @export
compute_optimality_metrics <- function(mesh, list_units,n_range,c_params,A_tot) {

  # --- Check mesh ---
  if (!inherits(mesh, "sf")) {
    stop("`mesh` must be an sf object.")
  }
  geom_type <- unique(as.character(sf::st_geometry_type(mesh)))
  if (!any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("`mesh` must contain polygon geometries (POLYGON or MULTIPOLYGON).")
  }
  if (nrow(mesh) == 0) {
    stop("`mesh` cannot be empty.")
  }
  required_cols <- c("DEPTH", "VEL")
  missing_cols <- setdiff(required_cols, names(mesh))
  if (length(missing_cols) > 0) {
    stop(paste0("`mesh` is missing required attribute(s): ", paste(missing_cols, collapse = ", ")))
  }

  # --- Check list_units ---
  if (!is.list(list_units)) {
    stop("`list_units` must be a list.")
  }
  if (length(list_units) == 0) {
    stop("`list_units` must contain at least one element.")
  }
  is_sf_poly <- sapply(list_units, function(x) {
    inherits(x, "sf") && any(sf::st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON"))
  })
  if (!any(is_sf_poly)) {
    stop("`list_units` must contain at least one 'sf' polygon object.")
  }

  # --- Check n_range ---
  if (!is.numeric(n_range)) {
    stop("`n_range` must be numeric.")
  }
  if (length(n_range) == 0) {
    stop("`n_range` must contain at least one element.")
  }
  if (any(is.na(n_range))) {
    warning("`n_range` contains NA values.")
  }
  if (any(n_range %% 1 != 0)) {stop("`n_range` must be an integer value (whole number).")}

  if (any(n_range == 1)) {

    stop("only mosaics with >= 2 units can evaluated")

  }

  # --- Check c_params ---
  if (!is.data.frame(c_params)) {
    stop("`c_params` must be a data.frame.")
  }
  required_cols <- c("W_area", "c_logmean", "c_logsd")
  missing_cols <- setdiff(required_cols, names(c_params))
  if (length(missing_cols) > 0) {
    stop(paste0("`c_params` is missing required column(s): ", paste(missing_cols, collapse = ", ")))
  }
  # Verify values are numeric and > 0
  non_numeric <- !sapply(c_params[required_cols], is.numeric)
  if (any(non_numeric)) {
    stop(paste0("The following columns in `c_params` must be numeric: ",
                paste(required_cols[non_numeric], collapse = ", ")))
  }

  # --- Check A_tot ---
  if (!is.numeric(A_tot)) {
    stop("`A_tot` must be numeric.")
  }
  if (length(A_tot) != 1 || is.na(A_tot) || A_tot <= 0) {
    stop("`A_tot` must be a single positive numeric value.")
  }

  #### continue with main function body ####

  W_area <- c_params$W_area
  c_logmean <- c_params$c_logmean
  c_logsd <- c_params$c_logsd

  # Initialize metric vectors
  MI <- numeric()
  v <- numeric()

  # extract centroids from polygons
  mesh_pts <- sf::st_centroid(mesh)

  # compute MI and v for each mosaic
  for (i in 1:length(n_range)) {

    regions_i <- list_units[[i]]

    indicator <- segmentation_indicators(regions_i,mesh_pts,"VEL")

    MI[i] <- indicator$MI
    v[i] <- indicator$v

  }

  # compute mn index

  mn <- compute_mn(c_logmean,c_logsd,W_area,A_tot,n_range,n_iter = 500)$mn

  # normalize metrics to [0,1]
  MI = sp_norm(MI,min(MI,na.rm=T),max(MI,na.rm=T))
  v = sp_norm(v,min(v,na.rm=T),max(v,na.rm=T))
  mn = sp_norm(mn,min(mn,na.rm=T),max(mn,na.rm=T))

  return(data.frame(n = n_range,
                    MI = MI,
                    v = v,
                    mn = mn,
                    GS = (MI + v + mn)/3))

}


# --------------------------------------------------------------------
# Internal helper functions (not exported)
# --------------------------------------------------------------------

sp_norm <- function(x,x.min,x.max) {  (x-x.min)/(x.max-x.min)}
