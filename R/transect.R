#' Compute Transects Perpendicular to a Line
#'
#' Generates transects perpendicular to a given point with associated angle directions.
#' Each transect is centered at the input point and extends in both directions by the specified length.
#'
#' @param tpts A `data.frame` or `tibble` containing the coordinates (`x`, `y`) and orientation angle (`theta`) of each point.
#' @param tlen Numeric. Half-length of the transects to be generated (i.e., total transect length will be `2 * tlen`).
#'
#' @return A `data.frame` with columns `x0`, `y0`, `x1`, `y1`, representing the start and end coordinates of each transect line.
#'
#' @details The function calculates a perpendicular angle to `theta` (`thetaT = theta + π/2`) and offsets the coordinates accordingly to create a line segment.
#'
#' @export

transect <- function( tpts, tlen){

  # --- Check tpts ---
  if (!(is.data.frame(tpts) || inherits(tpts, "tbl_df"))) {
    stop("`tpts` must be a data.frame or tibble.")
  }

  required_cols <- c("x", "y")
  missing_cols <- setdiff(required_cols, names(tpts))
  if (length(missing_cols) > 0) {
    stop(paste0("`tpts` is missing required column(s): ", paste(missing_cols, collapse = ", ")))
  }

  if (nrow(tpts) == 0) {
    stop("`tpts` cannot be empty.")
  }

  # --- Check tlen ---
  if (!is.numeric(tlen)) {
    stop("`tlen` must be numeric.")
  }
  if (length(tlen) != 1) {
    warning("`tlen` must be a single numeric value.")
  }
  if (is.na(tlen) || tlen <= 0) {
    stop("`tlen` must be a positive numeric value.")
  }

  #### main function body ####

  tpts$thetaT = tpts$theta+pi/2
  dx = tlen*cos(tpts$thetaT)
  dy = tlen*sin(tpts$thetaT)
  return(
    data.frame(x0 = tpts$x + dx,
               y0 = tpts$y + dy,
               x1 = tpts$x - dx,
               y1 = tpts$y -dy)
  )

}
