#' Regionalize supercells into mosaic of n units Using SKATER Algorithm
#'
#' Applies spatial regionalization to supercell polygons using the SKATER algorithm.
#' This function segments a mesh of supercells into spatial units based on
#' average depth and velocity attributes. It supports optional parallel computation to speed up processing.
#'
#' @param mesh_supercell A `sf` object containing polygons (supercells) with at least columns `D_avg` and `V_avg`
#'   representing average depth and velocity, respectively.
#' @param range_n A numeric vector specifying the target number of regions for each SKATER run.
#' @param parallelize Logical. If `TRUE`, uses available CPU cores minus one to enable parallel processing. Default is `FALSE`.
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
#' Parallelization is enabled by setting up a background cluster and configuring `spdep` options accordingly.
#'
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr mutate group_by summarise
#' @importFrom spdep poly2nb nbcosts nb2listw mstree skater set.coresOption set.mcOption set.ClusterOption get.coresOption
#' @importFrom parallel detectCores makeCluster stopCluster
#'
#' @export

supercell_to_units <- function(mesh_supercell,range_n,parallelize=F) {

  # nc <- 1L # number of cores
  # if (parallelize) {
  #   nc <- parallel::detectCores() - 1L # set to maximum cores - 1
  # }
  # ## set parallel computations
  # cores_opt <- spdep::set.coresOption(nc)
  # mc_opt <- spdep::set.mcOption(FALSE)
  # cl <- parallel::makeCluster(spdep::get.coresOption())
  # spdep::set.ClusterOption(cl)
  # on.exit({
  #   spdep::set.coresOption(cores_opt)
  #   spdep::set.mcOption(mc_opt)
  #   spdep::set.ClusterOption(NULL)
  #   parallel::stopCluster(cl)
  # })

  #### select only relevant variables
  mesh_supercell_sub <- mesh_supercell[,c('D_avg','V_avg')] # for now working only with predefined variables (average depth and vel)

  #### compute weights
  nb <- spdep::poly2nb(mesh_supercell_sub,queen=T)

  # # delete no-neighbour nodes
  # keep = rep(T,length(nb))
  # for (i in 1:length(nb)) {
  #   if (nb[[i]][1]==0) {
  #     keep[i]=F
  #   }
  # }
  # mesh_sp.sub <- mesh_sp.sub[keep,]
  # # re-run poly2nb
  # nb <- spdep::poly2nb(mesh_sp.sub,queen=T)

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
