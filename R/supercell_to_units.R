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
#' @return A list of `sf` objects, each representing a segmentation into `n` spatial units (as defined in `n_range`),
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

  #### main function body ####

  supercells_sub <- supercells[, c("D_avg", "V_avg")]

  message("Computing weights...")

  nb <- spdep::poly2nb(supercells_sub, queen = TRUE)

  supercells_scaled <- supercells_sub %>%
    sf::st_drop_geometry() %>%
    mutate(
      D_avg = scale(D_avg),
      V_avg = scale(V_avg)
    )

  costs <- spdep::nbcosts(nb, data = supercells_scaled)
  w <- spdep::nb2listw(nb, costs, style = "B")

  mst <- spdep::mstree(w)

  message("Weights computation done.")

  mesh_clustered <- vector("list", length(n_range))

  message("Segmentation with SKATER algorithm started...")

  pb <- progress::progress_bar$new(
    format = "  SKATER [:bar] :current/:total (:percent) ETA: :eta",
    total = length(n_range),
    clear = FALSE,
    width = 60,
    show_after = 0
  )

  pb$tick(0)

  for (i in seq_along(n_range)) {

    clusters <- spdep::skater(
      edges = mst,
      data = supercells_scaled,
      ncuts = n_range[i] - 1
    )

    mesh_clustered[[i]] <- cbind(
      supercells_sub,
      data.frame(cluster = clusters$groups)
    ) %>%
      dplyr::group_by(cluster) %>%
      dplyr::summarise(
        D_avg = mean(D_avg),
        V_avg = mean(V_avg)
      )

    pb$tick()
  }

  message("Segmentation done.")

  return(mesh_clustered)

}
