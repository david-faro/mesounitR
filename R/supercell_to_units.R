#' Regionalize supercells into mosaic of n units Using SKATER Algorithm
#'
#' Applies spatial regionalization to supercell polygons using the SKATER algorithm.
#' This function segments a mesh of supercells into spatial units based on
#' average depth and velocity attributes.
#'
#' @param supercells A `sf` object containing polygons (supercells) with at least columns `D_avg` and `V_avg`
#'   representing average depth and velocity, respectively.
#' @param n_range A numeric vector specifying the target number of regions for each SKATER run.
#'
#' @return A list of:
#' \enumerate{
#'  \item{skater_res} object returned from skater segmentation
#'  \item{list_units} list of `sf` objects, each representing a segmentation into `n` spatial units (as defined in `n_range`)
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Selects relevant variables (`D_avg`, `V_avg`).
#'   \item Builds neighbor relationships using `poly2nb()`.
#'   \item Scales input variables and computes edge costs.
#'   \item Constructs a minimum spanning tree.
#'   \item Applies the SKATER algorithm for each target number of regions.
#' }
#'
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr mutate group_by summarise
#' @importFrom spdep poly2nb nbcosts nb2listw mstree skater set.coresOption set.mcOption set.ClusterOption get.coresOption
#'
#' @export
supercell_to_units <- function(supercells,n_range) {

  # --- Check supercells ---
  if (!inherits(supercells, "sf")) {
    stop("`supercells` must be an sf object.")
  }
  geom_type <- unique(as.character(sf::st_geometry_type(supercells)))
  if (!any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("`supercells` must contain polygon geometries (POLYGON or MULTIPOLYGON).")
  }
  if (nrow(supercells) == 0) {
    stop("`supercells` cannot be empty.")
  }

  # --- Check n_range ---
  if (!is.numeric(n_range)) {
    stop("`n_range` must be numeric.")
  }
  if (length(n_range) == 0) {
    stop("`n_range` cannot be empty.")
  }
  if (any(is.na(n_range))) {
    warning("`n_range` contains NA values.")
  }
  if (any(n_range %% 1 != 0)) {
    stop("`n_range` must contain integer values (whole numbers).")
  }

  # set seed
  set.seed(123)

  #### main function body ####

  supercells_sub <- supercells[, c("D_avg", "V_avg")]

  nb <- spdep::poly2nb(supercells_sub, queen = TRUE)

  # check if supercells without neighbours found
  no_neigh <- which(spdep::card(nb) == 0)

  if (length(no_neigh) > 0) {
    stop(
      paste0(
        "The following supercells have no neighbours: ",
        paste(no_neigh, collapse = ", "),
        '\n
        Neighbourless supercells should be deleted or merged to proceed.'
      ),
      call. = FALSE
    )
  }

  supercells_scaled <- supercells_sub %>%
    sf::st_drop_geometry() %>%
    mutate(
      D_avg = scale(D_avg),
      V_avg = scale(V_avg)
    )

  costs <- spdep::nbcosts(nb, data = supercells_scaled)
  w <- spdep::nb2listw(nb, costs, style = "B")

  mst <- spdep::mstree(w)

  skater_res <- skater_allcuts(mst, supercells_scaled)

  mesh_clustered <- create_nmosaic(supercells, skater_res, n_range)

  return(list(skater_res=skater_res,
              list_units=mesh_clustered))

}
