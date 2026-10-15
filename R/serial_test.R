#' Default resolution of the block statistic
#'
#' Block length \eqn{m} and number of quantile classes \eqn{d} used by
#' \code{\link{serial_test}} for a series of length \code{n}: \eqn{(3, 3)} for
#' \eqn{n < 300}, \eqn{(3, \lfloor (n-300)/400 \rfloor + 4)} for
#' \eqn{300 \le n < 1500}, and \eqn{m = \lfloor \log n / (2\log 3) \rfloor},
#' \eqn{d = 3} beyond (Section 6 of the paper).
#'
#' @param n Number of observations.
#' @return A named integer vector \code{c(m = , d = )}.
#' @examples
#' choose_resolution(500)
#' choose_resolution(5000)
#' @export
choose_resolution <- function(n) {
  if (length(n) != 1 || !is.finite(n) || n < 1) stop("n must be a positive number", call. = FALSE)
  if (n < 300) return(c(m = 3L, d = 3L))
  if (n < 1500) return(c(m = 3L, d = as.integer((n - 300) %/% 400 + 4)))
  c(m = as.integer(floor(log(n) / (2 * log(3)))), d = 3L)
}

portmanteau_classes <- function(n) if (n < 300) 3L else if (n < 800) 4L else 5L

# block codes 0 .. d^m - 1 of the class sequence cls (0-based classes)
block_codes <- function(cls, m, d) {
  K <- length(cls) - m + 1
  code <- numeric(K)
  for (j in seq_len(m)) code <- code * d + cls[j:(j + K - 1)]
  code
}

pearson_blocks <- function(cls, m, d) {
  R <- d^m
  K <- length(cls) - m + 1
  cnt <- tabulate(block_codes(cls, m, d) + 1, R)
  R / K * sum(as.numeric(cnt)^2) - K
}

portmanteau_stat <- function(cls, d, lags) {
  n <- length(cls)
  out <- 0
  for (j in seq_len(lags)) {
    N <- n - j
    cnt <- tabulate(cls[1:N] * d + cls[(j + 1):n] + 1, d * d)
    out <- out + d * d / N * sum(as.numeric(cnt)^2) - N
  }
  out
}

tau_hat_fn <- function(codes, p, R, m) {
  g <- 2 * R * p[codes + 1]
  g <- g - mean(g)
  K <- length(g)
  L <- m - 1 + floor(4 * (K / 100)^(2 / 9))
  v <- sum(g * g) / K
  for (h in seq_len(min(L, K - 1))) v <- v + 2 * (1 - h / (L + 1)) * sum(g[1:(K - h)] * g[(1 + h):K]) / K
  sqrt(max(v, 0))
}

#' Serial Cramer coefficient
#'
#' The serial Cramer coefficient
#' \eqn{V_{m,d} = \{\Gamma / (d^{m-1} - 1)\}^{1/2}} measures the strength of
#' serial dependence on a scale from 0 to 1 (Proposition 4.7 of the paper):
#' \eqn{V = 0} if and only if blocks of \eqn{m} consecutive quantile classes
#' are uniformly distributed, and \eqn{V = 1} if and only if the class of any
#' one value determines its whole block. For \eqn{m = 2} it is Cramer's V of
#' consecutive classes. \code{gamma_from_cramer_v} is the inverse map.
#'
#' @param gamma Normalized information variance (approximate Bahadur slope).
#' @param v Serial Cramer coefficient, between 0 and 1.
#' @param m Block length.
#' @param d Number of quantile classes.
#' @return A number.
#' @examples
#' serial_cramer_v(0.13, m = 3, d = 4)
#' gamma_from_cramer_v(0.1, m = 3, d = 4)
#' @export
serial_cramer_v <- function(gamma, m, d) sqrt(pmax(gamma, 0) / (d^(m - 1) - 1))

#' @rdname serial_cramer_v
#' @export
gamma_from_cramer_v <- function(v, m, d) {
  if (any(v < 0 | v > 1)) stop("V must be between 0 and 1", call. = FALSE)
  (d^(m - 1) - 1) * v^2
}

with_seed <- function(seed, expr) {
  if (is.null(seed)) return(expr)
  genv <- globalenv()
  old <- if (exists(".Random.seed", envir = genv, inherits = FALSE)) get(".Random.seed", envir = genv) else NULL
  on.exit(if (is.null(old)) rm(".Random.seed", envir = genv) else assign(".Random.seed", old, envir = genv))
  set.seed(seed)
  expr
}

#' Exact symbolic test of serial independence
#'
#' Tests whether a time series is a sequence of independent and identically
#' distributed values, with the procedure of Ashtari Nezhad (Information
#' Variance and Exact Symbolic Tests of Serial Independence). The values are
#' replaced by their quantile classes (ranks); the block statistic \eqn{Q}
#' compares the frequencies of the \eqn{d^m} patterns of \eqn{m} consecutive
#' classes with their expected values, and the portmanteau \eqn{P_2} does the
#' same for pairs at lags 1 to \code{lags}. The proposed test is the combined
#' test \eqn{C = \min(p_Q, p_{P2})}, calibrated exactly from the same random
#' permutations.
#'
#' Under serial independence the permutation P-values are exact for every
#' sample size and every continuous distribution; ties are broken at random,
#' which keeps the test exact for discrete data. The asymptotic P-value of
#' \eqn{Q} uses its weighted chi-square limit (\code{\link{null_limit_sf}}),
#' that of \eqn{P_2} a chi-square limit with \code{lags*(d2-1)^2} degrees of
#' freedom.
#'
#' Besides the P-values the result contains the serial Cramer coefficient
#' \code{cramer_v} (strength of dependence, 0 to 1), the bias-corrected
#' normalized information variance \code{gamma_hat} (approximate Bahadur
#' slope; about 0 under independence), its asymptotic standard deviation
#' \code{tau_hat} (used for sample-size planning, see
#' \code{\link{required_sample_size}}), and the standardized residuals of the
#' patterns, which show which patterns of consecutive values are over- or
#' under-represented.
#'
#' For residuals of a fitted model the P-values are only approximately exact:
#' they are valid and slightly conservative after efficient (maximum
#' likelihood) estimation (Theorem 4.11 of the paper); after Gaussian
#' quasi-maximum likelihood with non-normal innovations use a model-based
#' bootstrap.
#'
#' @param x Numeric vector (or univariate time series), in time order.
#'   Missing and non-finite values are removed.
#' @param m,d Block length and number of quantile classes of \eqn{Q}.
#'   Default: \code{\link{choose_resolution}(n)}.
#' @param d2 Number of classes of the portmanteau \eqn{P_2} (default 3, 4 or
#'   5 for \eqn{n < 300}, \eqn{< 800}, \eqn{\ge 800}).
#' @param lags Lags of the portmanteau (default 2).
#' @param B Number of random permutations (default 9999). The P-values are
#'   exact for every \code{B}; a larger \code{B} makes them less variable.
#' @param seed Optional seed, for reproducible P-values. The random number
#'   state of the session is restored afterwards.
#' @param alpha Level of the reported decision.
#' @param transform \code{"none"}, \code{"diff"} (first differences) or
#'   \code{"logdiff"} (log returns in percent, for prices).
#' @param label Optional name of the series, shown in the report.
#' @param n_cells Number of patterns kept in the result (default 10).
#' @return An object of class \code{"ivserial"}: a list with elements
#'   \code{n}, \code{m}, \code{d}, \code{d2}, \code{lags}, \code{B},
#'   \code{Q}, \code{p_Q}, \code{p_Q_asymptotic}, \code{P2}, \code{p_P2},
#'   \code{p_P2_asymptotic}, \code{p_C}, \code{reject}, \code{alpha},
#'   \code{cramer_v}, \code{gamma_hat}, \code{tau_hat}, \code{cells} (data
#'   frame of patterns with observed and expected counts and residuals),
#'   \code{n_removed} and \code{label}. Use \code{print} for the report and
#'   \code{as.data.frame} for one row of results.
#' @seealso \code{\link{screen_series}} for many series,
#'   \code{\link{required_sample_size}} for planning,
#'   \code{\link{ivserial_app}} for the graphical interface.
#' @examples
#' set.seed(1)
#' x <- rnorm(300)
#' serial_test(x, B = 999, seed = 1)
#'
#' # ARCH(1): uncorrelated but dependent
#' z <- numeric(600); e <- rnorm(600)
#' for (t in 2:600) z[t] <- sqrt(1 + 0.8 * z[t - 1]^2) * e[t]
#' res <- serial_test(z[101:600], B = 999, seed = 1)
#' res$p_C
#' res$cramer_v
#' head(res$cells)
#'
#' # prices: test the log returns
#' fx <- read_series(system.file("extdata", "exchange_rates.csv", package = "ivserial"))
#' serial_test(fx$China, transform = "logdiff", label = "China", B = 999, seed = 1)
#' @export
serial_test <- function(x, m = NULL, d = NULL, d2 = NULL, lags = 2, B = 9999, seed = NULL,
                        alpha = 0.05, transform = c("none", "diff", "logdiff"), label = "",
                        n_cells = 10) {
  transform <- match.arg(transform)
  if (is.data.frame(x) || (is.matrix(x) && ncol(x) > 1))
    stop("x must be a single series; use screen_series() for several columns", call. = FALSE)
  x <- as.numeric(x)
  if (transform == "logdiff") {
    if (any(x[is.finite(x)] <= 0)) stop("log differences require positive values", call. = FALSE)
    x <- 100 * diff(log(x))
  } else if (transform == "diff") {
    x <- diff(x)
  }
  ok <- is.finite(x)
  n_removed <- sum(!ok)
  if (n_removed > 0) warning(n_removed, " missing or non-finite values removed", call. = FALSE)
  x <- x[ok]
  n <- length(x)
  if (n < 20) stop("at least 20 observations are needed (got ", n, ")", call. = FALSE)
  if (length(unique(x)) < 3) stop("the series is (almost) constant", call. = FALSE)

  res0 <- choose_resolution(n)
  m <- if (is.null(m)) res0[["m"]] else as.integer(m)
  d <- if (is.null(d)) res0[["d"]] else as.integer(d)
  d2 <- if (is.null(d2)) portmanteau_classes(n) else as.integer(d2)
  lags <- as.integer(lags)
  B <- as.integer(B)
  check_md(m, d)
  if (d2 < 2) stop("d2 must be at least 2", call. = FALSE)
  if (lags < 1 || lags >= n - 1) stop("lags must be between 1 and n - 2", call. = FALSE)
  if (B < 1) stop("B must be at least 1", call. = FALSE)
  R <- d^m
  K <- n - m + 1
  if (K / R < 1) warning(sprintf("only %.2f blocks per pattern on average; consider a smaller m or d", K / R),
                         call. = FALSE)

  sim <- with_seed(seed, {
    r <- order(order(x, stats::runif(n))) - 1          # 0-based ranks, ties broken at random
    Qs <- numeric(B)
    Ps <- numeric(B)
    for (b in seq_len(B)) {
      pr <- sample.int(n) - 1
      Qs[b] <- pearson_blocks((d * pr) %/% n, m, d)
      Ps[b] <- portmanteau_stat((d2 * pr) %/% n, d2, lags)
    }
    list(r = r, Qs = Qs, Ps = Ps)
  })
  r <- sim$r
  dq <- (d * r) %/% n
  dp <- (d2 * r) %/% n
  Q0 <- pearson_blocks(dq, m, d)
  P0 <- portmanteau_stat(dp, d2, lags)
  Qs <- sim$Qs
  Ps <- sim$Ps

  upper_p <- function(v, v0) (1 + sum(v >= v0 - 1e-9 * abs(v0))) / (B + 1)
  pQ <- upper_p(Qs, Q0)
  pP <- upper_p(Ps, P0)
  rank_p <- function(v) {
    s <- sort(v)
    (length(v) - findInterval(v - 1e-9 * abs(v), s, left.open = TRUE)) / length(v)
  }
  pm <- pmin(rank_p(c(Q0, Qs)), rank_p(c(P0, Ps)))
  pC <- (1 + sum(pm[-1] <= pm[1] + 1e-12)) / (B + 1)

  mu0 <- null_limit_moments(m, d)[["mean"]]
  gamma_hat <- (Q0 - mu0) / K
  pQa <- limit_sf1(Q0, m, d)
  pPa <- stats::pchisq(P0, lags * (d2 - 1)^2, lower.tail = FALSE)

  codes <- block_codes(dq, m, d)
  counts <- tabulate(codes + 1, R)
  tau_hat <- tau_hat_fn(codes, counts / K, R, m)
  e <- K / R
  resid <- (counts - e) / sqrt(e)
  top <- order(-abs(resid))[seq_len(min(n_cells, R))]
  pattern <- vapply(top - 1, function(cd) {
    dig <- integer(m)
    for (j in m:1) {
      dig[j] <- cd %% d + 1
      cd <- cd %/% d
    }
    paste(dig, collapse = "-")
  }, character(1))
  cells <- data.frame(pattern = pattern, observed = counts[top], expected = rep(e, length(top)),
                      residual = resid[top], stringsAsFactors = FALSE)

  structure(list(label = label, n = n, m = m, d = d, d2 = d2, lags = lags, B = B,
                 Q = Q0, p_Q = pQ, p_Q_asymptotic = pQa, P2 = P0, p_P2 = pP, p_P2_asymptotic = pPa,
                 p_C = pC, reject = pC <= alpha, alpha = alpha,
                 cramer_v = serial_cramer_v(gamma_hat, m, d), gamma_hat = gamma_hat, tau_hat = tau_hat,
                 cells = cells, n_removed = n_removed),
            class = "ivserial")
}

fmt_p <- function(p) if (p < 1e-4) "< 0.0001" else sprintf("%.4f", p)

#' @export
format.ivserial <- function(x, n_cells = 8, ...) {
  title <- "Exact symbolic test of serial independence"
  if (nzchar(x$label)) title <- paste0(title, "  [", x$label, "]")
  L <- c(title, strrep("=", nchar(title)),
         paste0("Observations n = ", x$n,
                if (x$n_removed > 0) paste0("  (", x$n_removed, " missing values removed)") else ""),
         sprintf("Block statistic: m = %d consecutive values, d = %d quantile classes (%d patterns)",
                 x$m, x$d, as.integer(x$d^x$m)),
         sprintf("Portmanteau: lags 1..%d, d = %d classes", x$lags, x$d2),
         sprintf("Random permutations B = %d", x$B), "",
         sprintf("%-34s%12s%12s", "Test", "Statistic", "P-value"),
         sprintf("%-34s%12s%12s", "C = min(p_Q, p_P2)  [proposed]", "", fmt_p(x$p_C)),
         sprintf("%-34s%12.3f%12s", "Q  (blocks, permutation)", x$Q, fmt_p(x$p_Q)),
         sprintf("%-34s%12s%12s", "Q  (blocks, asymptotic)", "", fmt_p(x$p_Q_asymptotic)),
         sprintf("%-34s%12.3f%12s", "P2 (lags, permutation)", x$P2, fmt_p(x$p_P2)),
         sprintf("%-34s%12s%12s", "P2 (lags, asymptotic)", "", fmt_p(x$p_P2_asymptotic)), "",
         sprintf("Serial independence is %s at the %g level (combined test C).",
                 if (x$reject) "REJECTED" else "not rejected", x$alpha),
         sprintf("Strength of dependence: V-hat = %.3f", x$cramer_v),
         "  (serial Cramer coefficient, 0 to 1: 0 = independent blocks, 1 = each value determines its block)",
         sprintf("Efficiency: Gamma-hat = %.4f", x$gamma_hat),
         "  (bias-corrected normalized information variance = approximate Bahadur slope; about 0 under independence)")
  if (x$reject && x$gamma_hat > 0) {
    n80 <- tryCatch(required_sample_size(x$gamma_hat, x$m, x$d, 0.05, 0.8, x$tau_hat), error = function(e) NULL)
    if (!is.null(n80))
      L <- c(L, sprintf("Planning: power 0.8 against dependence of this strength needs about %d observations", n80),
             sprintf("  (tau-hat = %.3f; Corollary 4.8 of the paper).", x$tau_hat))
  }
  cl <- utils::head(x$cells, n_cells)
  if (nrow(cl) > 0) {
    L <- c(L, "", sprintf("Most over- and under-represented patterns (classes 1 = lowest ... %d = highest):", x$d),
           sprintf("%-14s%10s%10s%10s", "pattern", "observed", "expected", "residual"),
           sprintf("%-14s%10d%10.1f%10.2f", cl$pattern, as.integer(cl$observed), cl$expected, cl$residual),
           "Residual = (observed - expected)/sqrt(expected); |residual| > 3 is notable.")
  }
  L
}

#' Print or convert the result of serial_test
#'
#' @param x Result of \code{\link{serial_test}}.
#' @param n_cells Number of patterns shown.
#' @param row.names,optional Ignored.
#' @param ... Ignored.
#' @return \code{print}: \code{x}, invisibly. \code{as.data.frame}: a data
#'   frame with one row of results.
#' @name print.ivserial
#' @export
print.ivserial <- function(x, n_cells = 8, ...) {
  cat(format(x, n_cells = n_cells), sep = "\n")
  invisible(x)
}

#' @rdname print.ivserial
#' @export
as.data.frame.ivserial <- function(x, row.names = NULL, optional = FALSE, ...) {
  keep <- c("label", "n", "m", "d", "d2", "lags", "B", "Q", "p_Q", "p_Q_asymptotic", "P2", "p_P2",
            "p_P2_asymptotic", "p_C", "reject", "alpha", "cramer_v", "gamma_hat", "tau_hat", "n_removed")
  as.data.frame(x[keep], stringsAsFactors = FALSE)
}
