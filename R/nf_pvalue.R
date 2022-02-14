#' Calculate the p-value for a column stratified by an outcome
#'
#' Calculate a table 1-style p-value for both continuous and categorical
#' outcomes
#'
#' @param column Name of the column to calculate a p-value for
#' @param outcome Outcome to stratify column by
#' @return A cleanly formatted p-value
#' @export
nf_pvalue <- function(column, outcome, df) {
  if (is.numeric(df[[column]])) {
    print(paste0(column, ' numeric'))
    # ANOVA calculates the difference in means
    aov_result <- aov(as.formula(paste0('`', column, '` ~ ', outcome)), data=df)
    p <- format.pval(summary(aov_result)[[1]][["Pr(>F)"]][1], digits=3, eps=0.001)
    return(p)
  }
  else {
    print(paste0(column, ' categorical'))
    # Chi Square
    chi_table <- table(df[[column]], df[[outcome]])
    chi_result <- chisq.test(chi_table, simulate.p.value = T)
    p <- format.pval(chi_result$p.value, digits=3, eps=0.001)
    return(p)
  }
}
