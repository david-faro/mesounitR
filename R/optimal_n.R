#' Identify Optimal 'n' Value Based on Global Score (GS)
#'
#' Extracts the optimal value of the parameter `n` (number of units) corresponding to the minimum value
#' of the Global Score
#'
#' @param optimal_metrics A list or data structure containing:
#' \describe{
#'   \item{GS}{A numeric vector of GS values.}
#'   \item{n}{A vector of associated parameter values for `n`.}
#' }
#'
#' @return A data frame containing:
#' \describe{
#'   \item{i}{The index of the optimal `n` value.}
#'   \item{n}{The optimal value of `n` that minimizes `GS`.}
#' }
#'
#' @details
#' The function identifies the index of the minimum `GS` value and returns the corresponding `n` value at that index.
#'
#' @examples
#' gs_values <- c(0.3, 0.2, 0.15, 0.25)
#' n_values <- seq(1, 4)
#' optimal_metrics <- list(GS = gs_values, n = n_values)
#' optimal_n(optimal_metrics)
#'
#' @export
optimal_n <- function(optimal_metrics) {

  GS <- optimal_metrics$GS
  n <- optimal_metrics$n

  # identify optimal n
  id_min <- which(GS == min(GS,na.rm = T), arr.ind = TRUE)

  # optimal n
  n_opt <- data.frame(i = id_min,
                      n = n[id_min])

  return(n_opt)
}
