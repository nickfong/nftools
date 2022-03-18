#' Unset 'GITHUB_PAT'
#'
#' Unset 'GITHUB_PAT'
#'
#' @return Nothing
#' @export
nf_github_pat_unset <- function() {
    Sys.unsetenv("GITHUB_PAT")
}
