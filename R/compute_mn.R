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

  # --- Check c_logmean ---
  if (!is.numeric(c_logmean) || length(c_logmean) != 1) { stop("'c_logmean' must be a single numeric value.")}
  if (is.na(c_logmean)) stop(paste0("'c_logmean' is NA"))

  # --- Check c_logsd ---
  if (!is.numeric(c_logsd) || length(c_logsd) != 1) { stop("'c_logsd' must be a single numeric value.")}
  if (is.na(c_logsd)) stop(paste0("'c_logsd' is NA"))
  if (c_logsd <= 0) warning("`c_logsd` must be positive")

  # --- Check W_area ---
  if (!is.numeric(W_area) || length(W_area) != 1) { stop("'W_area' must be a single numeric value.")}
  if (is.na(W_area)) stop(paste0("'W_area' is NA"))
  if (W_area <= 0) warning("`W_area` must be positive")

  # --- Check A_tot ---
  if (!is.numeric(A_tot) || length(A_tot) != 1) { stop("'A_tot' must be a single numeric value.")}
  if (is.na(A_tot)) stop(paste0("'A_tot' is NA"))
  if (A_tot <= 0) warning("`A_tot` must be positive")

  # --- Check n_range ---
  if (!is.numeric(n_range)) stop("`n_range` must be numeric.")
  if (length(n_range) == 0) stop("`n_range` cannot be empty.")
  if (any(is.na(n_range))) warning("`n_range` contains NA values.")
  if (any(n_range <= 0)) warning("`n_range` contains non-positive values.")
  if (any(n_range %% 1 != 0)) {stop("`n_range` must be an integer value (whole number).")}

  # --- Check n_iter ---
  if (!is.numeric(n_iter) || length(n_iter) != 1 || is.na(n_iter) || n_iter <= 0) {
    stop("`n_iter` must be a single positive numeric value.")
  }
  if (n_iter %% 1 != 0) {
    stop("`n_iter` must be an integer value (whole number).")
  }

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
