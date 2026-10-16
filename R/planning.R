#' Sample size and power of the block test
#'
#' Approximate power and required sample size of the block test \eqn{Q}
#' against dependence of strength \code{gamma} (normalized information
#' variance, approximate Bahadur slope), from Corollary 4.8 of the paper:
#' \deqn{\mathrm{power} \approx \Phi\{(K\gamma + \mu_0 - q_\alpha) / (K\tau^2 + V_0)^{1/2}\},}
#' where \eqn{K = n - m + 1}, \eqn{\mu_0} and \eqn{V_0} are the mean and
#' variance of the null limit and \eqn{q_\alpha} its critical value.
#'
#' With \code{tau = 0} the approximation is accurate for weak dependence and
#' otherwise optimistic. \code{\link{serial_test}} estimates \code{gamma}
#' (\code{gamma_hat}) and \code{tau} (\code{tau_hat}) from data, so a pilot
#' sample or a comparable series gives a realistic plan. The strength can
#' also be given as a serial Cramer coefficient through
#' \code{\link{gamma_from_cramer_v}}.
#'
#' @param gamma Strength of dependence \eqn{\Gamma} at \code{(m, d)}.
#' @param n Number of observations.
#' @param m Block length.
#' @param d Number of quantile classes.
#' @param alpha Level of the test.
#' @param power Target power(s).
#' @param tau Asymptotic standard deviation (0 for weak dependence).
#' @return \code{required_sample_size}: number of observations;
#'   \code{approximate_power}: a probability; \code{sample_size_table}: a data
#'   frame with columns \code{power} and \code{n}.
#' @examples
#' approximate_power(0.13, n = 350, m = 3, d = 4, tau = 1.13)   # simulated: 0.82
#' required_sample_size(0.13, m = 3, d = 4, power = 0.8, tau = 1.13)
#' sample_size_table(gamma_from_cramer_v(0.1, 3, 4), m = 3, d = 4, tau = 1)
#' @export
required_sample_size <- function(gamma, m = 3, d = 4, alpha = 0.05, power = 0.8, tau = 0) {
  if (!(gamma > 0)) stop("gamma must be positive", call. = FALSE)
  if (!(power > 0 && power < 1)) stop("power must be between 0 and 1", call. = FALSE)
  mom <- null_limit_moments(m, d)
  mu0 <- mom[["mean"]]
  V0 <- mom[["variance"]]
  q <- null_limit_quantile(alpha, m, d)
  z <- stats::qnorm(power)
  if (tau <= 0) {
    K <- (q - mu0 + z * sqrt(V0)) / gamma
  } else {
    a <- gamma^2
    bq <- -2 * gamma * (q - mu0) - z^2 * tau^2
    cc <- (q - mu0)^2 - z^2 * V0
    K <- (-bq + sqrt(max(bq^2 - 4 * a * cc, 0))) / (2 * a)
  }
  as.integer(ceiling(K) + m - 1)
}

#' @rdname required_sample_size
#' @export
approximate_power <- function(gamma, n, m = 3, d = 4, alpha = 0.05, tau = 0) {
  K <- n - m + 1
  mom <- null_limit_moments(m, d)
  q <- null_limit_quantile(alpha, m, d)
  stats::pnorm((K * gamma + mom[["mean"]] - q) / sqrt(K * tau^2 + mom[["variance"]]))
}

#' @rdname required_sample_size
#' @export
sample_size_table <- function(gamma, m = 3, d = 4, alpha = 0.05, tau = 0, power = c(0.5, 0.8, 0.9, 0.95)) {
  data.frame(power = power,
             n = vapply(power, function(p) required_sample_size(gamma, m, d, alpha, p, tau), integer(1)))
}
