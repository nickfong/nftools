#' Load a vector of librarires, installing them if need be
#'
#' This function loads a list of librarires and installs them if necessary.
#'
#' @param packages a vector of package names to be loaded
#' @return
#' @export
nf_library <- function(packages) {
  # Set default repo options
  r <- getOption("repos")
  r["CRAN"] <- "https://cran.us.r-project.org"
  options(repos = r)

  load_or_install_library <- function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }

  lapply(packages, load_or_install_library)
}
