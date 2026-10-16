#' Open the user guide
#'
#' Opens the user guide of the package (the same text as
#' \code{vignette("ivserial")}) in the web browser. The guide is installed
#' with the package, also when it is installed from GitHub without building
#' the vignettes.
#'
#' @return The path of the guide, invisibly.
#' @examples
#' if (interactive()) ivserial_guide()
#' @export
ivserial_guide <- function() {
  path <- system.file("guide", "ivserial-guide.html", package = "ivserial")
  if (!nzchar(path)) stop("the guide is not installed; see vignette(\"ivserial\")", call. = FALSE)
  utils::browseURL(path)
  invisible(path)
}
