#' Normalize Segmentation Indicators
#'
#' Applies min-max normalization to a numeric vector, scaling values to the range 0-1.
#'
#' @param x Numeric vector to be normalized.
#' @param x.min Minimum value used for normalization (typically the observed or theoretical minimum of `x`).
#' @param x.max Maximum value used for normalization (typically the observed or theoretical maximum of `x`).
#'
#' @return A numeric vector with values scaled between 0 and 1.
#'
#' @details
#' This function is used to normalize the segmentation metrics. The formula used is:
#' \deqn{(x - x.min) / (x.max - x.min)}
#'
#' If `x.min == x.max`, the result will contain `NaN` or `Inf` due to division by zero — users should check input ranges beforehand.
#'
#' @examples
#' values <- c(0.2, 0.5, 0.9)
#' sp_norm(values, x.min = 0.2, x.max = 0.9)
#'
#' @export

sp_norm <- function(x,x.min,x.max) {(x-x.min)/(x.max-x.min)}
