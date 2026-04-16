#' Identify Optimal 'n' Value Based on Global Score (GS)
#'
#' Extracts the optimal value of the parameter `n` (number of units) corresponding to the minimum value
#' of the Global Score
#'
#' @param optimality_metrics A list or data structure containing:
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
#' optimality_metrics <- data.frame(GS = gs_values, n = n_values)
#' optimal_n(optimality_metrics)
#'
#' @export
optimal_n <- function(optimality_metrics) {

  # --- Check object type ---
  if (!is.data.frame(optimality_metrics)) {
    stop("`optimality_metrics` must be a data.frame.")
  }

  # --- Check non-empty ---
  if (nrow(optimality_metrics) == 0) {
    stop("`optimality_metrics` cannot be empty.")
  }

  # --- Check required columns ---
  required_cols <- c("GS", "n")
  missing_cols <- setdiff(required_cols, names(optimality_metrics))
  if (length(missing_cols) > 0) {
    stop(paste0("`optimality_metrics` is missing required column(s): ",
                paste(missing_cols, collapse = ", ")))
  }

  #### main function body ####

  GS <- optimality_metrics$GS
  n <- optimality_metrics$n

  # identify optimal n
  id_min <- which(GS == min(GS,na.rm = T), arr.ind = TRUE)

  # optimal n
  n_opt <- data.frame(i = id_min,
                      n = n[id_min])

  return(n_opt)
}
