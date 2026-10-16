#' ivserial: exact symbolic tests of serial independence
#'
#' Rank-based tests of the hypothesis that a time series is a sequence of
#' independent and identically distributed values, with the procedure of
#' Ashtari Nezhad, \emph{Information Variance and Exact Symbolic Tests of
#' Serial Independence}.
#'
#' Main functions:
#' \describe{
#'   \item{\code{\link{serial_test}}}{the test for one series;}
#'   \item{\code{\link{screen_series}}}{many series, with Benjamini-Hochberg adjustment;}
#'   \item{\code{\link{required_sample_size}}, \code{\link{approximate_power}}}{planning;}
#'   \item{\code{\link{null_limit_sf}}, \code{\link{null_limit_summary}}}{the null limit of the block statistic;}
#'   \item{\code{\link{serial_cramer_v}}}{the strength of dependence between 0 and 1;}
#'   \item{\code{\link{read_series}}}{reading CSV, TXT and Excel files;}
#'   \item{\code{\link{ivserial_app}}}{the graphical interface;}
#'   \item{\code{\link{ivserial_guide}}}{the user guide in the browser.}
#' }
#' The user guide is \code{vignette("ivserial")}. A Python version with the
#' same functions is available from PyPI (\code{pip install ivserial}).
#'
#' @keywords internal
"_PACKAGE"

#' Daily exchange rates against the U.S. dollar
#'
#' Daily noon buying rates in New York of the U.S. dollar against 20
#' currencies, in units of currency per U.S. dollar, from 3 January 2023 to
#' 2 October 2026, on the 940 days on which all rates were published. These
#' are the data of the application in the paper; test their log returns
#' (\code{transform = "logdiff"}).
#'
#' The same data are in the file
#' \code{system.file("extdata", "exchange_rates.csv", package = "ivserial")}.
#'
#' @format A data frame with 940 rows and 21 columns: \code{Date} and one
#'   numeric column per currency (Australia, Brazil, Canada, China, Denmark,
#'   Euro, India, Japan, Malaysia, Mexico, New Zealand, Norway, Singapore,
#'   South Africa, South Korea, Sweden, Switzerland, Taiwan, Thailand,
#'   United Kingdom).
#' @source Board of Governors of the Federal Reserve System, statistical
#'   release H.10, obtained from \url{https://github.com/datasets/exchange-rates}.
#' @examples
#' head(exchange_rates[, 1:5])
#' serial_test(exchange_rates$China, transform = "logdiff", B = 999, seed = 1)
"exchange_rates"
