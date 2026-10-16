#' Read series from a CSV, TXT or Excel file
#'
#' Reads a file with one series per column, rows in time order. A first row
#' with column names is optional; the delimiter (comma, semicolon, tab or
#' spaces) and a decimal comma are detected. Columns that are not mostly
#' numeric, such as dates, are dropped. Excel files (\code{.xlsx},
#' \code{.xls}) need the package \pkg{readxl}.
#'
#' @param path Path of the file.
#' @return A data frame with the numeric columns.
#' @examples
#' fx <- read_series(system.file("extdata", "exchange_rates.csv", package = "ivserial"))
#' names(fx)
#' @export
read_series <- function(path) {
  if (!file.exists(path)) stop("file not found: ", path, call. = FALSE)
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("reading Excel files needs the package readxl: install.packages(\"readxl\")", call. = FALSE)
    df <- as.data.frame(readxl::read_excel(path))
    raw <- lapply(df, function(v) if (is.numeric(v)) v else to_num(as.character(v)))
  } else {
    con <- file(path, encoding = "UTF-8-BOM")
    lines <- tryCatch(readLines(con, warn = FALSE), finally = close(con))
    lines <- lines[nzchar(trimws(lines))]
    if (!length(lines)) stop("the file is empty", call. = FALSE)
    head_lines <- utils::head(lines, 50)
    cnt <- vapply(c(",", ";", "\t"), function(s) mean(lengths(regmatches(head_lines, gregexpr(s, head_lines, fixed = TRUE)))),
                  numeric(1))
    sep <- if (max(cnt) >= 1) names(cnt)[which.max(cnt)] else ""
    tab <- utils::read.table(text = lines, sep = sep, quote = "\"", colClasses = "character", fill = TRUE,
                             na.strings = character(), comment.char = "", strip.white = TRUE,
                             header = FALSE, check.names = FALSE)
    first <- as.character(unlist(tab[1, ]))
    has_header <- any(is.na(to_num(first[nzchar(first)])))
    header <- if (has_header) ifelse(nzchar(first), first, paste("column", seq_along(first))) else
      paste("column", seq_along(first))
    body <- if (has_header) tab[-1, , drop = FALSE] else tab
    raw <- lapply(body, to_num)
    names(raw) <- header
  }
  keep <- vapply(raw, function(v) length(v) > 0 && mean(is.finite(v)) > 0.5, logical(1))
  if (!any(keep)) stop("no numeric column found", call. = FALSE)
  as.data.frame(raw[keep], check.names = FALSE)
}

to_num <- function(s) {
  s <- trimws(s)
  dec_comma <- !is.na(s) & grepl("^[-+]?[0-9]*,[0-9]+([eE][-+]?[0-9]+)?$", s)
  s[dec_comma] <- sub(",", ".", s[dec_comma], fixed = TRUE)
  suppressWarnings(as.numeric(s))
}
