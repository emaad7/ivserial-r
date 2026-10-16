#' Test many series (screening)
#'
#' Applies \code{\link{serial_test}} to every numeric column of a data frame
#' (or the columns named in \code{columns}) and adjusts the combined P-values
#' for multiple testing by the method of Benjamini and Hochberg.
#'
#' @param data A data frame, matrix or list of numeric series, for example
#'   the result of \code{\link{read_series}}.
#' @param columns Optional names of the columns to test (default: all numeric
#'   columns).
#' @param m,d,d2,lags,B,seed,alpha,transform,n_cells Options of
#'   \code{\link{serial_test}}, applied to every series; the same
#'   \code{seed} is used for each series.
#' @return An object of class \code{"ivserial_screen"}: a list with
#'   \code{table} (one row per series, with the adjusted P-value
#'   \code{p_adj} and the decision), \code{results} (the individual
#'   \code{"ivserial"} results) and \code{skipped}. \code{print} shows the
#'   table; \code{summary} prints every individual report as well.
#' @examples
#' fx <- read_series(system.file("extdata", "exchange_rates.csv", package = "ivserial"))
#' sc <- screen_series(fx[, 1:4], transform = "logdiff", B = 499, seed = 1)
#' sc
#' sc$results$China
#' @export
screen_series <- function(data, columns = NULL, m = NULL, d = NULL, d2 = NULL, lags = 2, B = 9999,
                          seed = NULL, alpha = 0.05, transform = c("none", "diff", "logdiff"), n_cells = 10) {
  transform <- match.arg(transform)
  if (is.matrix(data)) data <- as.data.frame(data)
  if (!is.list(data)) stop("data must be a data frame, matrix or list of series", call. = FALSE)
  if (is.null(names(data))) names(data) <- paste("column", seq_along(data))
  num <- names(data)[vapply(data, is.numeric, logical(1))]
  columns <- if (is.null(columns)) num else columns
  miss <- setdiff(columns, num)
  if (length(miss)) stop("column(s) not found or not numeric: ", paste(miss, collapse = ", "), call. = FALSE)
  args <- list(m = m, d = d, d2 = d2, lags = lags, B = B, seed = seed, alpha = alpha,
               transform = transform, n_cells = n_cells)
  results <- list()
  skipped <- character()
  for (nm in columns) {
    r <- tryCatch(suppressWarnings(do.call(serial_test, c(list(data[[nm]], label = nm), args))),
                  error = function(e) conditionMessage(e))
    if (is.character(r)) skipped[nm] <- r else results[[nm]] <- r
  }
  tab <- do.call(rbind, lapply(results, function(r)
    data.frame(series = r$label, n = r$n, p_C = r$p_C, cramer_v = r$cramer_v, gamma_hat = r$gamma_hat,
               stringsAsFactors = FALSE)))
  if (!is.null(tab)) {
    tab$p_adj <- stats::p.adjust(tab$p_C, "BH")
    tab$reject <- tab$p_adj <= alpha
    tab <- tab[, c("series", "n", "p_C", "p_adj", "cramer_v", "gamma_hat", "reject")]
    rownames(tab) <- NULL
  }
  structure(list(table = tab, results = results, skipped = skipped, alpha = alpha),
            class = "ivserial_screen")
}

#' @export
format.ivserial_screen <- function(x, ...) {
  L <- "Screening summary (Benjamini-Hochberg adjustment over the series tested)"
  t <- x$table
  if (!is.null(t)) {
    L <- c(L, sprintf("%-24s%7s%10s%10s%8s%11s  %s", "series", "n", "p_C", "adj. p", "V-hat", "Gamma-hat", "decision"),
           sprintf("%-24s%7d%10.4f%10.4f%8.3f%11.4f  %s", substr(t$series, 1, 23), as.integer(t$n), t$p_C, t$p_adj,
                   t$cramer_v, t$gamma_hat, ifelse(t$reject, "reject", "-")))
  }
  for (nm in names(x$skipped)) L <- c(L, sprintf("[%s] skipped: %s", nm, x$skipped[[nm]]))
  L
}

#' @export
print.ivserial_screen <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' @export
summary.ivserial_screen <- function(object, ...) {
  for (r in object$results) {
    print(r)
    cat("\n")
  }
  print(object)
  invisible(object)
}
