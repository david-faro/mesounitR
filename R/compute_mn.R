#' Compute the meso-number (mn) index
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
#'   \item{mn}{mn index}
#' }
#'
#' @details
#' For each unit number in `n_range`, the function simulates random draws from
#' the empirically defined log-normal distribution of meso-sizes, computes probable resulting area,
#' and compares the resulting area to the reference total `A_tot`.
#'
#' @importFrom dplyr group_by summarise mutate
#' @export
compute_mn <- function(
    c_logmean,
    c_logsd,
    W_area,
    A_tot,
    n_range,
    n_iter = 500) {

  # Helper for single positive numeric values
  check_single_positive <- function(x, name) {
    if (!is.numeric(x)) stop(paste0("`", name, "` must be numeric."))
    if (length(x) != 1) stop(paste0("`", name, "` must be a single numeric value."))
    if (is.na(x) || x <= 0) stop(paste0("`", name, "` must be a positive numeric value."))
  }

  # --- Check c_logmean ---
  check_single_positive(c_logmean, "c_logmean")

  # --- Check c_logsd ---
  check_single_positive(c_logsd, "c_logsd")

  # --- Check W_area ---
  check_single_positive(W_area, "W_area")

  # --- Check A_tot ---
  check_single_positive(A_tot, "A_tot")

  # --- Check n_range ---
  if (!is.numeric(n_range)) stop("`n_range` must be numeric.")
  if (length(n_range) == 0) stop("`n_range` cannot be empty.")
  if (any(is.na(n_range))) warning("`n_range` contains NA values.")
  if (any(n_range <= 0)) warning("`n_range` contains non-positive values.")

  # --- Check n_iter ---
  check_single_positive(n_iter, "n_iter")

  #### continue main function body ####

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

  # Compute mn index for each probable area

  index <- abs(n_prob$A - A_tot) / A_tot
  index[index >= 1] <- 1

  n_prob$index <- index

  # Summarise by N and normalize
  n_summary <- n_prob %>%
    dplyr::group_by(n) %>%
    dplyr::summarise(mn = mean(index), .groups = "drop")

  return(n_summary)
}
