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
    # add_count(TILES, wt = n, name = 'TILE_TOTAL') %>%
    dplyr::group_by(TILES) %>%
    dplyr::mutate(PERCENT = n/sum(n)) %>%
    ggplot2::ggplot(ggplot2::aes(x = TILES, y = PERCENT, group = {{y_feat}}, color = {{y_feat}})) +
    ggplot2::geom_point() +
    ggplot2::geom_line()  +
    ggplot2::theme_minimal()

}


# BINARY PREDICTION PERFORMACNE WITH CHANGING ONE VARIABLE ----

#'
#'
#' @description A function that takes the inputs and spits out a plot showing changes in prediction based on changes in one predictor
#'
#' @param: data      Data frame object that contains predictors + ID
#' @param: model     Model object name that needs to be fit
#' @param: ID        ID (numeric) of the case that we want to use for experiment
#' @param: test_feat Numeric predictor name (String) that we want to experiment with
#' @param: breaks    Interval point at which the test features is to be broken into (e.g. breaks = 2, will split 0:6 series like this: 0, 2, 4, 6)
#'
#'  @return:          A plot that shows how the prediction changes when one predictor changes keeping all other predictors same

prediction_variation = function(data, model, ID, test_feat, ID_feat_name = "EMPLID", breaks = 100, isMatrix = TRUE){

  # taking the data from the corresponding ID
  sample = data %>%
    filter(!!rlang::sym(ID_feat_name) == ID)

  # creating a dataframe combining sample case and multiple values of the desired test feature
  message("Creating dataframe for the experiment")
  df = seq(min(data[,test_feat]),
           max(data[,test_feat]), breaks) %>%
    data.frame() %>%
    structure(names = c(test_feat)) %>%
    bind_rows(sample) %>%
    tidyr::fill(names(.), .direction = "up") %>%
    distinct()

  # organizing dataframe to match the training set that was used to train the model
  df = df[, c(names(sample))]
  df_mat = df %>%
    select(-EMPLID)  %>% data.matrix() %>% xgb.DMatrix()

  # if(isMatrix){
  #   df = df %>% data.matrix() %>% xgb.DMatrix()
  # }

  # calculating prediction on the experiment data
  message("Printing the plot")
  plot = predict(model, df_mat) %>%
    as.data.frame() %>%
    structure( names = c("pred_prob")) %>%
    # adding columns from the experient data to prediction
    bind_cols(df) %>%
    # creating visualization
    ggplot(aes_string(test_feat, "pred_prob")) +
    geom_line(color = "#000099", size = 0.1) +
    geom_point(color = "#CC0000", size = .5) +
    expand_limits(y = c(0,1)) +
    theme_light() +
    labs(title = paste0(test_feat, " curve")) +
    ylab("Predicted Probability") +
    ggthemes::theme_tufte()

  # printing as an interactive plotly plot
  plot %>%
    ggplotly()
}
