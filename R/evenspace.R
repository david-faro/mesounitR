#' Evenly Space Points Along a Line
#'
#' Calculates the coordinates of evenly spaced points along a polyline defined by a sequence of 2D points.
#' Useful for geometric processing, plotting, or spatial interpolation tasks where regular spacing is needed.
#'
#' @param xy A numeric matrix or data.frame with two columns representing the x and y coordinates of a polyline.
#' @param sep Numeric. Distance (in CRS units) between consecutive points to be placed along the line.
#' @param start Numeric. Distance (in CRS units) from the start of the line at which to place the first point (default is 0).
#'
#' @return A data frame containing:
#' \describe{
#'   \item{x}{x-coordinates of the evenly spaced points.}
#'   \item{y}{y-coordinates of the evenly spaced points.}
#'   \item{x0}{x-coordinates of the segment's starting point.}
#'   \item{y0}{y-coordinates of the segment's starting point.}
#'   \item{x1}{x-coordinates of the segment's ending point.}
#'   \item{y1}{y-coordinates of the segment's ending point.}
#'   \item{theta}{Angle (in radians) of the segment direction, relative to the x-axis.}
#' }
#'
#' @examples
#' # Define a polyline
#' xy <- matrix(c(0, 0, 2, 2, 4, 0), ncol = 2, byrow = TRUE)
#' # Compute points every 0.5 units
#' evenspace(xy, sep = 0.5)
#'
#' @export
evenspace <- function(xy, sep, start=0){

  # --- Check xy ---
  if (!(is.matrix(xy) || is.data.frame(xy))) {
    stop("`xy` must be a numeric matrix or data.frame.")
  }

  if (!all(sapply(xy, is.numeric))) {
    stop("All columns in `xy` must be numeric.")
  }

  if (ncol(xy) > 2) {
    warning("`xy` has more than two columns — only the first two may be used.")
  }

  if (nrow(xy) == 0) {
    stop("`xy` cannot be empty.")
  }

  # --- Check sep ---
  if (!is.numeric(sep)) {
    stop("`sep` must be numeric.")
  }
  if (length(sep) != 1) {
    stop("`sep` must be a single numeric value.")
  }
  if (is.na(sep) || sep <= 0) {
    stop("`sep` must be a positive numeric value.")
  }
  if (is.na(sep)) {
    stop("`sep` is NA")
  }


  # --- Check start ---
  if (!is.numeric(start)) {
    stop("`start` must be numeric.")
  }
  if (length(start) != 1) {
    stop("`start` must be a single numeric value.")
  }
  if (is.na(start)) {
    stop("`start` is NA")
  }
  if (is.na(sep) || sep <= 0) {
    stop("`sep` must be a positive numeric value.")
  }

  #### main function body ####

  dx <- c(0,diff(xy[,1]))
  dy <- c(0,diff(xy[,2]))
  dseg <- sqrt(dx^2+dy^2)
  dtotal <- cumsum(dseg)

  linelength = sum(dseg)

  pos = seq(start,linelength, by=sep)

  whichseg = unlist(lapply(pos, function(x){sum(dtotal<=x)}))

  pos=data.frame(pos=pos,whichseg=whichseg,
                 x0=xy[whichseg,1],
                 y0=xy[whichseg,2],
                 dseg = dseg[whichseg+1],
                 dtotal = dtotal[whichseg],
                 x1=xy[whichseg+1,1],
                 y1=xy[whichseg+1,2]
  )

  pos$further =  pos$pos - pos$dtotal
  pos$f = pos$further/pos$dseg
  pos$x = pos$x0 + pos$f * (pos$x1-pos$x0)
  pos$y = pos$y0 + pos$f * (pos$y1-pos$y0)

  pos$theta = atan2(pos$y0-pos$y1,pos$x0-pos$x1)

  return(pos[,c("x","y","x0","y0","x1","y1","theta")])

}
