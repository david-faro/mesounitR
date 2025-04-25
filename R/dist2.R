#' Compute Euclidean Distance Between a Point and Multiple Points
#'
#' This function calculates the Euclidean distance between a single reference point (`pt1`)
#' and multiple target points (`pt2`). It is fully vectorized for efficiency.
#'
#' @param pt1 A data frame or matrix with one row and two columns (x, y), representing the reference point.
#' @param pt2 A data frame or matrix with multiple rows and two columns (x, y), representing target points.
#'
#' @return A numeric vector of distances from `pt1` to each row in `pt2`.
#'
#' @details
#' The function computes distances using the standard Euclidean formula:
#' \deqn{d = \sqrt{(x_2 - x_1)^2 + (y_2 - y_1)^2}}
#'
#'
#' @examples
#' pt1 <- data.frame(x = 1, y = 2)
#' pt2 <- data.frame(x = c(3, 4), y = c(5, 6))
#' dist2(pt1, pt2)
#'
#' @export
dist2 <- function(pt1,pt2) {


  # repeat vector pt1 to length of pt2
  pt1 <- rep(pt1,nrow(pt2))

  # Compute Euclidean distances using vectorized functions
  dx <- pt2$x - pt1$x
  dy <- pt2$y - pt1$y

  dist <- sqrt(dx^2+dy^2)

  return(dist)

}



