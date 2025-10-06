#' Add Unit Mosaics to an Existing List of Units
#'
#' This function extends an existing list of unit mosaics by adding new units
#' generated from supercells based on a specified range of unit sizes.
#'
#' @param list_units_old A list of existing unit mosaics, each being a data.frame or similar object.
#' @param supercells An object containing the supercell structure used to generate new unit mosaics.
#' @param n_range_toadd A numeric vector indicating the sizes of new units to add from the supercells.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{range_n}{A numeric vector containing the sizes (number of rows) of all unit mosaics (old and new).}
#'   \item{list_units}{A list combining the old and new unit mosaics.}
#' }
#'
#' @examples
#' # Assuming you have defined `supercells`, `list_units_old`, and `n_range_toadd`:
#' # result <- add_units(list_units_old, supercells, n_range_toadd)
#'
#' @export
#'
add_units <- function(list_units_old,supercells,n_range_toadd) {

  # --- Check list_units_old ---
  if (!is.list(list_units_old)) {
    stop("`list_units_old` must be a list.")
  }
  if (length(list_units_old) == 0) {
    stop("`list_units_old` cannot be empty. It must contain at least one element.")
  }
  # Check if it contains at least one sf polygon
  is_sf_poly <- sapply(list_units_old, function(x) {
    inherits(x, "sf") && any(sf::st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON"))
  })
  if (!any(is_sf_poly)) {
    stop("`list_units_old` must contain at least one 'sf' polygon object.")
  }

  # --- Check supercells ---
  if (!inherits(supercells, "sf")) {
    stop("`supercells` must be an sf object.")
  }
  geom_type <- unique(as.character(sf::st_geometry_type(supercells)))
  if (!any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("`supercells` must contain polygon geometries.")
  }

  # --- Check n_range_toadd ---
  if (!is.numeric(n_range_toadd)) {
    stop("`n_range_toadd` must be numeric.")
  }
  if (length(n_range_toadd) == 0) {
    warning("`n_range_toadd` is empty.")
  }
  if (any(is.na(n_range_toadd))) {
    warning("`n_range_toadd` contains NA values.")
  }

  # continue with main function body


  # Select only new n values
  n_range_old <- numeric()

  for (i in 1:length(list_units_old)) {

    n_range_old[i] <- nrow(list_units_old[[i]])

  }

  n_range_new <- n_range_toadd[ ! n_range_toadd %in% n_range_old ]

  # Generate new units mosaics based on then n's defined in n_range_toadd
  list_units_toadd <- supercell_to_units(supercells,n_range_new)

  # Combine old and new mosaics lists
  list_units <- c(list_units_old,list_units_toadd)

  # Combine n-ranges of old and new mosaics
  n_range <- c(n_range_old,n_range_new)

  # reorder
  id.ordered <- order(n_range)

  n_range <- n_range[id.ordered]
  list_units <- list_units[id.ordered]

  # Return the combined list and the updated n-range vector
  return(list(n_range = n_range,
              list_units = list_units))

}
