#' Create a forest plot from the output of broom::tidy
#'
#' Creates a forest plot from the output of broom::tidy
#'
#' @param a df that broom::tidy returns
#' @return a forest plot
#' @export
nf_forest_plot_broom <- function(df) {
  df$lower <- df$estimate - 1.96*df$std.error
  df$upper <- df$estimate + 1.96*df$std.error

  df$exp_coef <- ifelse(df$estimate < 0,
                        1*exp(df$estimate),
                        exp(df$estimate))

  df$exp_std.error <- exp(df$std.error)
  df$exp_lower <- ifelse(df$lower< 0,
                         1*exp(df$lower),
                         exp(df$lower))
  df$exp_upper <- ifelse(df$upper< 0,
                         1*exp(df$upper),
                         exp(df$upper))

  df$term <- factor(df$term, levels=rev(df$term))

  # Force the order of the terms from least to greatest OR
  df <- dplyr::arrange(df, estimate)
  df$term <- factor(df$term, levels=rev(df$term))

  forest_plot_exp <- ggplot2::ggplot(data=df, ggplot2::aes(x=term, y=exp_coef, ymin=exp_lower, ymax=exp_upper)) +
    ggplot2::geom_pointrange() +
    ggplot2::geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
    ggplot2::coord_flip() +  # flip coordinates (puts labels on y axis)
    ggplot2::ylab("Odds Ratio (95% CI)") +
    ggplot2::xlab("") +
    ggplot2::theme_bw() + # use a white background
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 15))
    print(forest_plot_exp)

    df$exp_coef <- round(df$exp_coef, 2)
    df$exp_lower <- round(df$exp_lower, 2)
    df$exp_upper <- round(df$exp_upper, 2)
    df$p.value <- round(df$p.value, 3)

    # Print out stats to copy and paste
    cat(paste0(df$term, ' ', df$exp_coef, ' [', df$exp_lower, ', ', df$exp_upper, ']; p = ', df$p.value, '\n'))

  return(forest_plot_exp)
}
