#' Add Unit Mosaics to an Existing List of Units
#'
#' This function extends an existing list of unit mosaics by adding new units
#' generated from supercells based on a specified range of unit sizes.
#'
#' @param list_units_old A list of existing unit mosaics, each being a data.frame or similar object.
#' @param supercells An object containing the supercell structure used to generate new unit mosaics.
#' @param range_n_toadd A numeric vector indicating the sizes of new units to add from the supercells.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{range_n}{A numeric vector containing the sizes (number of rows) of all unit mosaics (old and new).}
#'   \item{list_units}{A list combining the old and new unit mosaics.}
#' }
#'
#' @examples
#' # Assuming you have defined `supercells`, `list_units_old`, and `range_n_toadd`:
#' # result <- add_units(list_units_old, supercells, range_n_toadd)
#'
#' @export
#'
add_units <- function(list_units_old,supercells,range_n_toadd) {
  # Generate new units mosaics based on then n's defined in range_n_toadd
  list_units_toadd <- supercell_to_units(supercells,range_n_toadd)

  # Combine old and new mosaics lists
  list_units <- c(list_units_old,list_units_toadd)

  # Compute size (number of rows) for each mosaic in the old list
  range_n_old <- numeric()

  for (i in 1:length(list_units_old)) {

    range_n_old[i] <- nrow(list_units_old[[i]])

  }

  # Combine n-ranges of old and new mosaics
  range_n <- c(range_n_old,range_n_toadd)

  # Return the combined list and the updated n-range vector
  return(list(range_n = range_n,
              list_units = list_units))

}
