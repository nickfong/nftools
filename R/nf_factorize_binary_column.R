#' Change a column that is numeric 1/0 to 'Yes'/'No', respectively
#'
#' Change a column that is numeric 1/0 to 'Yes'/'No', respectively
#'
#' @param column name as a string
#' @param a df
#' @return a df
#' @export
nf_factorize_binary_column <- function(column, df) {
  df[[column]] <- factor(df[[column]], levels=c(0, 1), labels=c('No', 'Yes'))
  return(df)
}
