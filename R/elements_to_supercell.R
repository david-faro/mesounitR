#' Compute supercells using a SLIC approach on hydraulic mesh data
#'
#' This function clusters elements of a computational mesh into supercells
#' based on hydraulic variables `DEPTH` and `VEL`, using a Simple Linear Iterative
#' Clustering (SLIC)-like approach. It returns an `sf` object with supercell
#' geometries and statistics (mean and standard deviation) for hydraulic variables.
#'
#' @param mesh An `sf` object representing a TIN mesh with numeric columns `DEPTH` and `VEL`.
#' @param sc_area A positive numeric value specifying the average target  area of each supercell.
#' @param compactness A positive numeric value controlling the compactness of supercells.
#'
#' @return An `sf` object with dissolved supercell polygons and averaged hydraulic statistics.
#'
#' @details
#' The function identifies initial cluster centers on a regular grid, computes
#' local averages of hydraulic variables in the vicinity of these centers, and
#' performs K-means clustering in a space combining spatial and hydraulic information.
#'
#' The resulting supercells include:
#' * `D_avg`, `D_sd`: Mean and standard deviation of water depth.
#' * `V_avg`, `V_sd`: Mean and standard deviation of velocity.
#' * `F_avg`, `F_sd`: Mean and standard deviation of the Froude number.
#'
#' The Froude number is calculated as `VEL / sqrt(9.81 * DEPTH)`.
#'
#'
#' @export

elements_to_supercell <- function(mesh,sc_area,compactness) {

  if (!inherits(mesh, "sf")) stop("`mesh` must be an sf object.")
  if (!all(c("DEPTH", "VEL") %in% names(mesh))) stop("`mesh` must have 'DEPTH' and 'VEL' columns.")
  if (!is.numeric(sc_area) || sc_area <= 0) stop("`sc_area` must be a positive numeric value.")
  if (!is.numeric(compactness) || compactness <= 0) stop("`compactness` must be a positive numeric value.")
  if (nrow(mesh) < 2) stop("`mesh` must contain at least two features for clustering.")

  # mesh into points
  mesh_pts <- st_centroid(mesh)

  # compute bounding box
  bbox <- as.numeric(st_bbox(mesh_pts))

  # grid interval
  S <- sqrt(sc_area)

  #### step 1 --> choose K superpixel cluster centers at regular grid intervals S --> search area will be a 2S x 2S around cluster centers

  ## compute grid
  grid.x <- seq(from=bbox[1]+S/2,to=bbox[3],by=S)
  grid.y <- seq(from=bbox[2]+S/2,to=bbox[4],by=S)
  grid.xy <- expand.grid(grid.x,grid.y)
  colnames(grid.xy) <- c('X','Y')

  # cast grid into sf
  grid_sf <- sf::st_cast(st_sf(geom = st_sfc(st_multipoint(as.matrix(grid.xy))), crs = st_crs(mesh)),to='POINT')

  # select only grid points within mesh area
  inter <- sf::st_intersects(grid_sf,mesh)

  # sf mesh to data.frame
  mesh_df <- sf::st_drop_geometry(mesh)
  mesh_coord <- sf::st_coordinates(mesh_pts)
  mesh_df$X <- mesh_coord[,'X']
  mesh_df$Y <- mesh_coord[,'Y']

  # assign to each grid point the average value for DEPTH and VEL of all points falling within an S radius
  grid_valid <- mesh_df[unlist(inter),]

  for (i in 1:length(grid_valid)) {

    dist <- dist2(grid_valid[i,c('X','Y')],mesh_df[,c('X','Y')])

    mesh_buffer <- mesh_df[dist <=S, ]

    grid_valid$X[i] <- mean(mesh_buffer$X)
    grid_valid$Y[i] <- mean(mesh_buffer$Y)
    grid_valid$DEPTH[i] <- mean(mesh_buffer$DEPTH)
    grid_valid$VEL[i] <- mean(mesh_buffer$VEL)

  }

  #### compute superpixels
  pts <- sf::st_drop_geometry(mesh_pts)
  coords <- sf::st_coordinates(mesh_pts)
  pts$X <- coords[,'X']
  pts$Y <- coords[,'Y']

  # normalize values of hydraulic and spatial variables
  sc_spat <- max(c(max(pts$X)-min(pts$Y),max(pts$Y)-min(pts$Y))) #Scale of spatial dimensions
  sc_col <- max(sd(pts$DEPTH),sd(pts$VEL))
  # Scaling ratio for mesh values
  scaling_ratio <- (sc_spat/sc_col)/compactness

  # define data.frame to use for superpixel clustering
  pts_tocluster <- pts
  pts_tocluster$DEPTH <- pts_tocluster$DEPTH*scaling_ratio
  pts_tocluster$VEL <- pts_tocluster$VEL*scaling_ratio

  # mesh_cc.df <- pts_tocluster[cluster_center$elem,]

  # Run k-means
  # km <- ClusterR::KMeans_rcpp(pts_tocluster[,c('X','Y','DEPTH','VEL')],
  #                             clusters=nrow(grid_valid),
  #                             CENTROIDS=as.matrix(grid_valid[,c('X','Y','DEPTH','VEL')]))

  km <- ClusterR::KMeans_arma(pts_tocluster[,c('X','Y','DEPTH','VEL')],
                              clusters=nrow(grid_valid),
                              seed_mode='keep_existing',
                              CENTROIDS=as.matrix(grid_valid[,c('X','Y','DEPTH','VEL')]))
  clusters = ClusterR::predict_KMeans(pts_tocluster[,c('X','Y','DEPTH','VEL')], km)

  # dissolve mesh into superpixels, and compute averages and standard deviation for depth, velocity and Froude
  mesh_sc <- mesh %>%
    mutate(cluster = clusters) %>%
    group_by(cluster) %>%
    summarise(geometry = st_union(geometry),
              D_avg = mean(DEPTH),
              D_sd = sd(DEPTH),
              F_avg = mean(VEL/sqrt(9.81*DEPTH)),
              F_sd = sd(VEL/sqrt(9.81*DEPTH)),
              V_avg = mean(VEL),
              V_sd = sd(VEL)) %>%
    ungroup()

  return(mesh_sc)
}
