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
#' 2. Total records in each quantiles based bin,
#' 3. A line plot showing ratio in different bins. The output of the function can be passed to plotly to generate interactive plot
#'
#' @examples
#'
#' library(tidyverse)
#' qtile_plot(iris, Sepal.Length, Species, 4)
#'
#'
#' @export

qtile_plot = function(data, x_feat, y_feat, quantile = 4){

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

# BOX PLOTS FOR NUMERIC FEATURES SPLIT IN TARGET FEAT CATEGORIS ----

#' A function to display or save box plots created on all numeric features.
#'
#' @description Creates (or saves as png if asked) boxplots for all numeric features. Split the box plots based on target feature categories.
#'
#' @param dataset Name of the data frame object
#' @param classVar Name of the target feature
#' @param order A vector listing the target feature labels in the desired order. To use default order leave this parameter. Default value NULL.
#' @param colors A vector listing which color to use to represent which target feature label. To have the function pick color leave this parameter. Deafult value NULL.
#' @param loc A string with the directory where you want to save the plots. If no location is provided the plots will be created and displayed but not stored as image files.
#'
#' @return
#'
#' Returns three outputs:
#' 1. If loc = NULL, returns plots and displays in the RStudio Plots window,
#' 2. If loc has location provided, creates and saves the plots. Doesn't display in RStudio.
#'
#' @examples
#'
#' library(tidyverse)
#' boxPlot(dataset = iris,
#'      classVar = Species,
#'           order = c("virginica", "versicolor", "setosa"),
#'           colors = c("virginica" = "#32a897", "versicolor" = "#328bab", "setosa" = "#8031ad"))
#'
#'
#' @export

boxPlot <- function(dataset, classVar, order = NULL, colors = NULL, loc = NULL) {

  x <- rlang::enquo(classVar) %>% rlang::get_expr()
  nLevels <- dplyr::select(dataset, {{classVar}}) %>% unique() %>% nrow()
  # producing random color n case no color is provided
  if(is.null(colors)){
    colors = randomcoloR::randomColor(nLevels, luminosity = "bright")
  }

  if(is.null(loc)){
    for(i in names(dplyr::select(dataset, where(is.numeric)))) {
      print(dataset %>%
              ggplot2::ggplot(ggplot2::aes_string(x, {i}, color = x)) +
              ggplot2::geom_boxplot(show.legend = FALSE) +
              ggplot2::scale_x_discrete(limits = order) +
              ggplot2::scale_color_manual(values = colors) +
              ggplot2::labs(title = paste0("Distribution of ", i, " in Different ", x, " Categories")) +
              ggplot2::theme_minimal())
    }
  } else{
    for(i in names(select(dataset, where(is.numeric)))) {
      png(paste0(loc, "/boxplot_", i, ".PNG"), width = 627, height = 453)
      plot <- dataset %>%
        ggplot2::ggplot(ggplot2::aes_string(x, {i}, color = x)) +
        ggplot2::geom_boxplot(show.legend = FALSE) +
        ggplot2::scale_x_discrete(limits = order) +
        ggplot2::scale_color_manual(values = colors) +
        ggplot2::labs(title = paste0("Distribution of ", i, " in Different ", x, " Categories")) +
        ggplot2::theme_minimal()
      print(plot)
      dev.off()
    }
  }
}
