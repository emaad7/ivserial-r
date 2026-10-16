# Null limit of Q (Theorem 4.1 of the paper):
#   L_{m,d} = sum_{s=2}^m (m - s + 1) chi2_{(d-1)^2 d^(s-2)}

limit_weights <- function(m, d) {
  s <- seq.int(2, m)
  list(w = m - s + 1, df = (d - 1)^2 * d^(s - 2))
}

check_md <- function(m, d) {
  if (length(m) != 1 || length(d) != 1 || !is.finite(m) || !is.finite(d) ||
      m != round(m) || d != round(d) || m < 2 || d < 2)
    stop("m and d must be whole numbers of at least 2", call. = FALSE)
  invisible(TRUE)
}

#' Mean and variance of the null limit
#'
#' @param m Block length (at least 2).
#' @param d Number of quantile classes (at least 2).
#' @return A numeric vector with elements \code{mean} and \code{variance}.
#' @keywords internal
#' @noRd
null_limit_moments <- function(m, d) {
  lw <- limit_weights(m, d)
  c(mean = sum(lw$w * lw$df), variance = 2 * sum(lw$w^2 * lw$df))
}

#' Null limit distribution of the block statistic Q
#'
#' Under serial independence the block statistic \eqn{Q} converges in
#' distribution to
#' \deqn{L_{m,d} = \sum_{s=2}^{m} (m-s+1)\,\chi^2_{(d-1)^2 d^{s-2}}}
#' with independent chi-square variables (Theorem 4.1 of Ashtari Nezhad).
#' \code{null_limit_sf} gives \eqn{P(L_{m,d} > q)}, computed by numerical
#' inversion of the characteristic function (Imhof, 1961);
#' \code{null_limit_quantile} gives the upper \code{alpha} critical value;
#' \code{null_limit_summary} gives the components, mean, variance and the
#' critical values at the levels 0.10, 0.05 and 0.01.
#'
#' @param q Value(s) of the statistic.
#' @param m Block length (at least 2).
#' @param d Number of quantile classes (at least 2).
#' @param alpha Upper tail probability.
#' @return \code{null_limit_sf}: upper tail probabilities;
#'   \code{null_limit_quantile}: a critical value;
#'   \code{null_limit_summary}: a list of class \code{"ivserial_null"}.
#' @references Imhof, J. P. (1961). Computing the distribution of quadratic
#'   forms in normal variables. \emph{Biometrika}, 48, 419--426.
#' @examples
#' null_limit_sf(33.76, m = 3, d = 3)      # about 0.05
#' null_limit_quantile(0.05, m = 3, d = 4)
#' null_limit_summary(3, 4)
#' @export
null_limit_sf <- function(q, m, d) {
  check_md(m, d)
  vapply(q, function(t) limit_sf1(t, m, d), numeric(1))
}

limit_sf1 <- function(t, m, d) {
  lw <- limit_weights(m, d)
  w <- lw$w
  df <- lw$df
  if (!is.finite(t)) return(if (t > 0) 0 else 1)
  if (t <= 0) return(1)
  if (length(w) == 1) return(stats::pchisq(t / w, df, lower.tail = FALSE))
  mom <- null_limit_moments(m, d)
  if (t > mom[["mean"]] + 12 * sqrt(mom[["variance"]])) {
    th <- 0.25 / max(w)
    return(min(1, exp(-th * t - 0.5 * sum(df * log(1 - 2 * th * w)))))
  }
  f <- function(u) {
    vapply(u, function(v) {
      if (v == 0) return(0.5 * (sum(df * w) - t))
      theta <- 0.5 * sum(df * atan(w * v)) - 0.5 * t * v
      rho <- exp(sum(df / 4 * log1p((w * v)^2)))
      sin(theta) / (v * rho)
    }, numeric(1))
  }
  # the integrand decays like u^(-1 - sum(df)/2); integrate on a finite range in pieces
  upper <- 1
  while (abs(f(upper)) * upper > 1e-14 && upper < 1e8) upper <- upper * 2
  br <- unique(c(0, upper * 2^(-(30:0))))
  br <- br[br <= upper]
  val <- 0
  for (i in seq_len(length(br) - 1)) {
    val <- val + stats::integrate(f, br[i], br[i + 1], subdivisions = 1000L,
                                  rel.tol = 1e-10, abs.tol = 1e-13)$value
  }
  min(max(0.5 + val / pi, 0), 1)
}

#' @rdname null_limit_sf
#' @export
null_limit_quantile <- function(alpha, m, d) {
  check_md(m, d)
  if (!(alpha > 0 && alpha < 1)) stop("alpha must be between 0 and 1", call. = FALSE)
  mom <- null_limit_moments(m, d)
  hi <- mom[["mean"]] + 20 * sqrt(mom[["variance"]])
  stats::uniroot(function(t) limit_sf1(t, m, d) - alpha, c(1e-9, hi), tol = 1e-10)$root
}

#' @rdname null_limit_sf
#' @export
null_limit_summary <- function(m, d) {
  check_md(m, d)
  lw <- limit_weights(m, d)
  mom <- null_limit_moments(m, d)
  crit <- vapply(c(0.10, 0.05, 0.01), null_limit_quantile, numeric(1), m = m, d = d)
  names(crit) <- c("0.10", "0.05", "0.01")
  structure(list(m = m, d = d, patterns = d^m,
                 components = data.frame(weight = lw$w, df = lw$df),
                 mean = mom[["mean"]], variance = mom[["variance"]], critical = crit),
            class = "ivserial_null")
}

#' @export
print.ivserial_null <- function(x, ...) {
  cat("Null limit of Q for m = ", x$m, ", d = ", x$d, " (", x$patterns, " patterns)\n", sep = "")
  cat("L = ", paste0(x$components$weight, " x chi2(", x$components$df, ")", collapse = " + "), "\n", sep = "")
  cat(sprintf("mean = %.2f, variance = %.2f\n", x$mean, x$variance))
  for (i in seq_along(x$critical))
    cat(sprintf("critical value at level %s: %.3f\n", names(x$critical)[i], x$critical[i]))
  invisible(x)
}
