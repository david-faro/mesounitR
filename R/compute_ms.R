#' Compute the meso-size (ms) index
#'
#' This function simulates areas based on log-normal random sampling (empirical meso-sizes c)
#' and computes a normalized index of deviation from a target area (total reach area)
#'
#' @param c_logmean Mean of the log-normal distribution (on log scale).
#' @param c_logsd Standard deviation of the log-normal distribution (on log scale).
#' @param W_area reference area (width^2)
#' @param A_tot Numeric value, total reference (reach) area.
#' @param n_range Integer vector of units range
#' @param n_iter Number of iterations (default = 500).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{n}{n units}
#'   \item{ms}{ms index}
#' }
#'
#' @details
#' For each unit number in `n_range`, the function simulates random draws from
#' the empirically defined log-normal distribution, computes probable resulting area,
#' and compares the resulting area to the reference total `A_tot`.
#'
#' @importFrom dplyr group_by summarise mutate
#' @export
compute_ms <- function(
    c_logmean,
    c_logsd,
    W_area,
    A_tot,
    n_range,
    n_iter = 500
) {

  # Draw values from log-normal distribution
  x <- stats::rlnorm(100000, c_logmean, c_logsd)

  # Initialize results
  n_prob <- data.frame()

  # compute probable areas
  for (i in n_range) {
    for (j in seq_len(n_iter)) {

      A <- sum(sample(x, i) * W_area)

      n_prob <- rbind(
        n_prob,
        data.frame(
          n = i,
          A = A
        )
      )
    }
  }

  # Compute ms index for each probable area

  index <- abs(n_prob$A - A_tot) / A_tot
  index[index >= 1] <- 1

  n_prob$index <- index

  # Summarise by N and normalize
  n_summary <- n_prob |>
    dplyr::group_by(n) |>
    dplyr::summarise(ms = mean(index), .groups = "drop")

  return(n_summary)
}
