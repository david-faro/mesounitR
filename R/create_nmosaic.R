#' Extract mosaics from SKATER cut matrix
#'
#' Extracts spatial mosaic partitions with n units defined in \code{n_range}
#'
#' @param supercells spatial object containing supercells
#' @param res_allcuts output object from \code{skater_allcuts}, including
#'        \code{groups.history}
#' @param n_range integer vector defining n values
#'
#' @return
#' List of segmented mosaics with n units defined in \code{n_range}.
#'
#' @export
create_nmosaic <- function(supercells, res_allcuts, n_range) {

  out <- lapply(n_range, function(n) {

    supercells$id <- res_allcuts$groups.history[, n]

    supercells %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(.groups = "drop")
  })

  names(out) <- paste0("k", n_range)

  return(out)
}
