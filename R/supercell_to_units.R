#' Regionalize supercells into mosaic of n units Using SKATER Algorithm
#'
#' Applies spatial regionalization to supercell polygons using the SKATER algorithm.
#' This function segments a mesh of supercells into spatial units based on
#' average depth and velocity attributes.
#'
#' @param mesh_supercell A `sf` object containing polygons (supercells) with at least columns `D_avg` and `V_avg`
#'   representing average depth and velocity, respectively.
#' @param range_n A numeric vector specifying the target number of regions for each SKATER run.
#'
#' @return A list of `sf` objects, each representing a segmentation into `n` spatial units (as defined in `range_n`),
#'   with recalculated average depth (`D_avg`) and velocity (`V_avg`) for each unit.
#'
#' @details The function performs the following steps:
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

supercell_to_units <- function(mesh_supercell,range_n) {

  # --- Check mesh_supercell ---
  if (!inherits(mesh_supercell, "sf")) {
    stop("`mesh_supercell` must be an sf object.")
  }
  geom_type <- unique(as.character(sf::st_geometry_type(mesh_supercell)))
  if (!any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("`mesh_supercell` must contain polygon geometries (POLYGON or MULTIPOLYGON).")
  }
  if (nrow(mesh_supercell) == 0) {
    stop("`mesh_supercell` cannot be empty.")
  }

  # --- Check range_n ---
  if (!is.numeric(range_n)) {
    stop("`range_n` must be numeric.")
  }
  if (length(range_n) == 0) {
    stop("`range_n` cannot be empty.")
  }
  if (any(is.na(range_n))) {
    warning("`range_n` contains NA values.")
  }
  if (any(range_n %% 1 != 0)) {
    stop("`range_n` must contain integer values (whole numbers).")
  }

  #### main function body ####

  #### select only relevant variables
  mesh_supercell_sub <- mesh_supercell[,c('D_avg','V_avg')] # for now working only with predefined variables (average depth and vel)

  #### compute weights
  nb <- spdep::poly2nb(mesh_supercell_sub,queen=T)

  # scale variables for regionalization
  mesh_supercell_scaled <- mesh_supercell_sub %>%
    sf::st_drop_geometry() %>%
    mutate(D_avg = scale(D_avg), # average depth
           V_avg = scale(V_avg)) # average velocity

  costs <- spdep::nbcosts(nb,data=mesh_supercell_scaled)
  w <- spdep::nb2listw(nb,costs,style="B")

  mst <- spdep::mstree(w)

  mesh_clustered <- list()

  # segment using skater algorithm. ncuts defined based on range_n variable
  for (i in 1:length(range_n)) {

    clusters <- spdep::skater(edges=mst,data=mesh_supercell_scaled,ncuts=range_n[i]-1)

    mesh_clustered[[i]] <- cbind(mesh_supercell_sub,
                                 data.frame(cluster=clusters$groups)) %>%
      group_by(cluster) %>%
      summarise(D_avg = mean(D_avg),
                V_avg = mean(V_avg))

  }

  return(mesh_clustered)

}
