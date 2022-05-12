#' Create a table1 and p-values
#'
#' Create a table using the table1 package while generating p-values
#'
#' @param columns  List of column names to include
#' @param outcome Outcome to stratify table by
#' @return A table1 and p-value df
#' @export
nf_table1 <- function(columns, outcome, df, minmax=FALSE, reset_p_value_df=FALSE, p_value_df_name='p_value_df') {
    if((reset_p_value_df) | (!exists('p_value_df'))) {
        p_value_df <- data.frame()
    }
    for (column in columns) {
        temp <- data.frame(
            column_name = column,
            p_value = nftools::nf_pvalue(column, outcome, df)
        )
        p_value_df <- rbind(p_value_df, temp)
    }
    # Hack to write p_value_df to global environment
    assign(p_value_df_name, p_value_df, envir=.GlobalEnv)

    if(minmax) {
        return(table=table1::table1(
                                    as.formula(paste0('~`', paste(columns, collapse='` + `'), '` | ', outcome)),
                                    data=df,
                                    render.continuous=c(
                                                        .="Mean (SD)",
                                                        .="Median [Q1, Q3]",
                                                        .="Min, Max"
                                    )))
    }
    else {
        return(table=table1::table1(
                                    as.formula(paste0('~`', paste(columns, collapse='` + `'), '` | ', outcome)),
                                    data=df,
                                    render.continuous=c(
                                                        .="Mean (SD)",
                                                        .="Median [Q1, Q3]"
                                    )))
    }
}
