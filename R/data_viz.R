# QUANTILE BASED TARGET LABEL RATIO ----

#' Ratio of target categories in binned numeric predictor
#'
#' @description Plot the ratio of categories of target labels inside different bins created based on quantiles for numeric features
#'
#' @param data Name of the data frame object
#' @param x_feat Name of the predictor of the interest
#' @param y_feat Name of the target feature
#' @param quantile Number of quantiles to be used to bin the numeric feature
#'
#' @return
#'
#' Returns three outputs:
#' 1. Quantile boundaries,
#' 2. # in each quantiles based bin,
#' 3. A line plot showing ratio in different bins. The output of the function can be passed to plotly to generate interactive plot
#'
#' @examples
#'
#' library(tidyverse)
#' quant_plot(iris, Sepal.Length, Species, 4)
#'
#'
#' @export

quant_plot = function(data, x_feat, y_feat, quantile = 4){

  x_str <- rlang::enquo(x_feat) %>% rlang::get_expr()
  y_str <- rlang::enquo(y_feat) %>% rlang::get_expr()

  Q = dplyr::pull(data, {{x_feat}}) %>%
    stats::quantile(probs = seq(0, 1, 1/quantile))

  if(length(unique(Q)) < quantile+1){
    stop(paste0(x_feat,
                " doesn't have enough unique values. Lower the number of quantiles."))
  }

  cat("Quantile boundaries:\n")
  print(Q)

  TILES <- cut(dplyr::pull(data, {{x_feat}}), breaks = c(Q), labels = paste0("Q", seq(1, quantile, 1)),
               include.lowest = T)

  cat("\nQuantile wise total counts:\n")
  print(table(TILES))

  data$TILES <- TILES

  data %>% dplyr::select(TILES, {{y_feat}}) %>% table()

  data %>%
    dplyr::count({{y_feat}}, TILES) %>%
    dplyr::group_by(TILES) %>%
    dplyr::mutate(PERCENT = n/sum(n)) %>%
    ggplot2::ggplot(ggplot2::aes(x = TILES, y = PERCENT, group = {{y_feat}}, color = {{y_feat}})) +
    ggplot2::geom_point() +
    ggplot2::geom_line()  +
    ggplot2::labs(title = paste0("Ratio of ", y_str, " Categories in ", quantile, " Quantile Groups of ", x_str)) +
    ggplot2::theme_minimal()

}


