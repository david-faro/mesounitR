#' Clean Mesh by Removing Non-Contiguous Elements
#'
#' This function removes disconnected (non-contiguous) polygons from a mesh, retaining
#' only those elements that share edges with the main, contiguous area of the mesh.
#' It assumes the largest contiguous polygon represents the main wetted channel or region of interest.
#'
#' @param mesh An `sf` object consisting of polygonal mesh elements (e.g., hydraulic model output).
#'
#' @return An `sf` object containing only the mesh elements that are contiguous with the largest connected component.
#'
#' @details
#' The function works by:
#' \enumerate{
#'   \item Merging all mesh polygons into contiguous areas using `st_union`.
#'   \item Selecting the largest merged polygon (by area), assumed to represent the main wetted region.
#'   \item Filtering the original mesh to retain only polygons that intersect this main region.
#' }
#' This method ensures that polygons merely touching at a vertex (not an edge) are excluded,
#' which avoids over-selecting disconnected elements.
#'
#' @examples
#' # mesh_cleaned <- clean_mesh(mesh)
#'
#' @export
clean_mesh <- function(mesh) {

  # --- Check that mesh is an sf object ---
  if (!inherits(mesh, "sf")) {
    stop("`mesh` must be an sf object.")
  }

  # --- Check that geometry type is polygonal ---
  geom_type <- unique(as.character(sf::st_geometry_type(mesh)))
  if (!any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("`mesh` must contain polygon geometries (POLYGON or MULTIPOLYGON).")
  }

  # --- Optional: Warn if empty or invalid geometries ---
  if (nrow(mesh) == 0) {
    warning("'mesh' is empty - no geometries found.")
  }
  if (any(!sf::st_is_valid(mesh))) {
    warning("'mesh' contains invalid geometries.")
  }

  #### continue with main function body ####

  # Merge all neighboring polygons into contiguous areas
  mesh_merged <- sf::st_cast(st_union(mesh), "POLYGON")

  # Identify the largest merged polygon by area
  mesh_merged_largest <- mesh_merged[sf::st_area(mesh_merged) == max(sf::st_area(mesh_merged))]

  # Select polygons from original mesh that intersect with the largest contiguous polygon
  selected_polygons <- sf::st_filter(mesh, mesh_merged_largest)

  # this approach is slower
  # intersects_list <- st_intersects(mesh,mesh_merged_largest)
  # selected_polygons <- mesh[lengths(intersects_list) > 0, ]

  return(selected_polygons)

}
