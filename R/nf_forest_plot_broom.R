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
  df <- df %>% arrange(estimate)
  df$term <- factor(df$term, levels=rev(df$term))

  forest_plot_exp <- ggplot(data=df, aes(x= term, y=exp_coef, ymin=exp_lower, ymax=exp_upper)) +
    geom_pointrange() +
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    ylab("Odds Ratio (95% CI)") +
    xlab("") +
    theme_bw() + # use a white background
    theme(axis.text.y = element_text(size = 15))
  print(forest_plot_exp)
  return(forest_plot_exp)
}

