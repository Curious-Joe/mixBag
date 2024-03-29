# FIT AND COMPARE MULTIPLE CLASSIFIERS ----

#' Comparing Classifiers' Performance
#'
#' @description Fits five commonly used classifiers on data and compare their performance based on AROC.
#'
#' @param recipe A parsnip recipe object.
#' @param test_df Test data frame to test model performances.
#' @param target_lab Label used in the target feature to indicate positive outcome. Default value is Y.
#'
#' @details
#'  - Make sure the recipe object entered is at pre-prepped stage
#'  - Five models compared are: logistic regression, elastic net, random forest, support vector machine, xtreme gradient boosting.
#'
#' @return Returns following two things:
#'  - A line plot showing comparative area under the ROC curve score
#'  - A tibble containing the test data along with the predicted probability calculated by the five classifiers.
#'
#' @examples
#'   library(tidymodels)
#'   split <- rsample::initial_split(wine, strata = quality_bin)
#'   train <- rsample::training(split)
#'   test <- rsample::testing(split)
#'   recipe <- recipes::recipe(quality_bin ~ ., data = train) %>%
#'   update_role(ID, new_role = 'identification') %>%
#'   step_string2factor(all_nominal()) %>%
#'   step_knnimpute(all_predictors()) %>%
#'   step_normalize(all_numeric())
#'
#'   compare_classifiers(recipe = recipe, test_df = test, target_lab = 1)
#'
#' @export
compare_classifiers = function(recipe, test_df, target_lab = Y){
  rec_prep = recipes::prep({{recipe}})
  target <-  stats::formula(rec_prep)[2] %>% as.character()
  form = stats::formula(rec_prep)
  train = rec_prep %>% recipes::juice()
  test = recipes::bake(rec_prep, new_data = {{test_df}})
  target_lab <- rlang::enquo(target_lab) %>% rlang::get_expr()


  # fitting models
  message('fitting logit model...')
  log_reg <-  parsnip::logistic_reg() %>%  parsnip::set_engine("glm") %>%
    parsnip::fit(form , data = train)

  message('fitting elastic net...')
  glm_reg <- parsnip::logistic_reg(penalty = 0.05) %>% parsnip::set_engine("glmnet") %>%
    parsnip::fit(form , data = train)

  message('fitting random forest...')
  rforest <- parsnip::rand_forest() %>% parsnip::set_engine("ranger") %>% parsnip::set_mode("classification") %>%
    parsnip::fit(form , data = train)

  message('fitting xtreeme gradient boosting...')
  xgbModel <- parsnip::boost_tree() %>% parsnip::set_engine("xgboost") %>% parsnip::set_mode("classification") %>%
    parsnip::fit(form , data = train)

  # message('fitting support vector machine...')
  # svmRbf <- parsnip::svm_rbf() %>% parsnip::set_engine("kernlab") %>% parsnip::set_mode("classification") %>%
  #   parsnip::fit(form , data = train)

  # COMPARING PERFORMANCE
  message('calculating predictions')
  pred_col <- paste0(".pred_", target_lab)

  pred_test <<-  {{test_df}} %>%
    cbind(stats::predict(log_reg, new_data = test, type = "prob") %>%
            dplyr::select(pred_col) %>% dplyr::rename(log_reg_y = pred_col)) %>%
    cbind(stats::predict(glm_reg, new_data = test, type = "prob") %>%
            dplyr::select(pred_col) %>% dplyr::rename(glm_reg_y = pred_col)) %>%
    cbind(stats::predict(rforest, new_data = test, type = "prob") %>%
            dplyr::select(pred_col) %>% dplyr::rename(rforest_y = pred_col)) %>%
    cbind(stats::predict(xgbModel, new_data = test, type = "prob") %>%
            dplyr::select(pred_col) %>% dplyr::rename(xgbModel_y = pred_col))
    # cbind(stats::predict(svmRbf, new_data = test, type = "prob")
            # dplyr::select(pred_col) %>% dplyr::rename(svmRbf_y = pred_col))

  pred_test[, target] <- as.factor(pred_test[, target])

  message('plotting performance curves')
  yardstick::roc_auc(data = pred_test, truth = !!target, log_reg_y, event_level = "second") %>%
    rbind(yardstick::roc_auc(data = pred_test, truth = !!target, glm_reg_y, event_level = "second")) %>%
    rbind(yardstick::roc_auc(data = pred_test, truth = !!target, rforest_y, event_level = "second")) %>%
    rbind(yardstick::roc_auc(data = pred_test, truth = !!target, xgbModel_y, event_level = "second")) %>%
    # rbind(yardstick::roc_auc(data = pred_test, truth = !!target, svmRbf_y, event_level = "second")) %>%
    cbind(model = c('logit_model', 'elastic_net', 'random_forest', 'xgboost')) %>%
    ggplot2::ggplot(ggplot2::aes(.estimate, stats::reorder(model, .estimate))) +
    ggplot2::geom_segment(ggplot2::aes(xend = 0, yend = model),
                          show.legend = F) +
    ggplot2::geom_point(ggplot2::aes(size = .estimate),
                        show.legend = F) +
    ggplot2::geom_label(ggplot2::aes(label = round(.estimate, 2), size = 5),
                        fill = "white",
                        hjust = "inward",
                        show.legend = F) +
    ggplot2::labs(x = "Area under ROC curve", y = "Model Names") +
    ggplot2::theme_minimal()

}


# FIT AND COMPARE TUNED CLASSIFERS ----

#' Comparing Tuned Classifiers' Performance
#'
#' @description Fits and tune hyper-parameters of five commonly used classifiers on data and compare their performance based on AROC.
#'
#' @param recipe A parsnip recipe object.
#' @param train_df Train data frame to train the models on. If no train_df is provided the function will try to extract training data from recipe.
#' @param test_df Test data frame to test model performances.
#' @param tune_metric Name of the metric that needs to be used to tune models. Available metric names = "roc_auc", "f_meas", "bal_accuracy", "pr_auc". Default value = "f_meas".
#' @param target_lab Label used in the target feature to indicate positive outcome. Default value is Y.
#' @param cv_fold_n How many folds to be used for cross validation. Default value is 5.
#' @param tune_n How many total combination of the hyper-parameter values to be tried. Default value is 10.
#' @param parallel Want to run the function using parallel? Default mode is FALSE.
#'
#' @details
#'  - Make sure the recipe object entered is at pre-prepped stage
#'  - Five models compared are: logistic regression, elastic net, random forest, support vector machine, xtreme gradient boosting.
#'  - Hyper-parameter values are created using 'tune' package's random default parameter generator: grid inside the tune_grid() function.
#'  - Putting very large number of tune_n will slow the process and potentially might break. Try using managable numbers e.g. 10 or 20.
#'
#' @return Returns following outputs:
#'  - A line plot showing comparative area under the ROC curve score
#'  - A tibble containing the test data along with the predicted probability calculated by the five classifiers.
#'  - Four tibbles containing the hyper-parameter tuning outcome. As it's produced by the tune::tune_grid() function.
#'  - Final fitted models with the best tuned parameters.
#'
#' @examples
#'   library(tidymodels)
#'   split <- rsample::initial_split(wine, strata = quality_bin)
#'   train <- rsample::training(split)
#'   test <- rsample::testing(split)
#'   recipe <- recipes::recipe(quality_bin ~ ., data = train) %>%
#'   update_role(ID, new_role = 'identification') %>%
#'   step_string2factor(all_nominal()) %>%
#'   step_knnimpute(all_predictors()) %>%
#'   step_normalize(all_numeric())
#'
#'   compare_tuned_classifiers(recipe = recipe, test_df = test, tune_metric = "f_meas", target_lab = 1, parallel = FALSE)
#'
#' @export
compare_tuned_classifiers = function(recipe, train_df = NULL, test_df, tune_metric = "f_meas", target_lab = 1, cv_fold_n = 5, tune_n = 10, parallel = FALSE){

  rec_prep = recipes::prep({{recipe}})
  target <-  stats::formula(rec_prep)[2] %>% as.character()
  form = stats::formula(rec_prep)
  train <- if(is.null(train_df)){
    rec_prep %>% recipes::juice()
  } else{
    train_df
  }
  test = recipes::bake(rec_prep, new_data = {{test_df}})
  target_lab <- rlang::enquo(target_lab) %>% rlang::get_expr()

  # fitting models
  message('fitting logit model')
  log_reg <- parsnip::logistic_reg() %>%  parsnip::set_engine("glm") %>%
    parsnip::fit(form , data = train)

  glm_reg <- parsnip::logistic_reg(penalty = tune::tune(), mixture = tune::tune()) %>%
    parsnip::set_engine("glmnet")

  rforest <- parsnip::rand_forest(mtry = tune::tune(), min_n = tune::tune()) %>%
    parsnip::set_engine("ranger", importance = "permutation") %>% parsnip::set_mode("classification")

  xgbModel <- parsnip::boost_tree(min_n = tune::tune(), tree_depth = tune::tune(), learn_rate = tune::tune()) %>%
    parsnip::set_engine("xgboost") %>% parsnip::set_mode("classification")

  # svmRbf <- parsnip::svm_rbf(cost = tune::tune(), rbf_sigma = tune::tune()) %>%
  #   parsnip::set_engine("kernlab") %>% parsnip::set_mode("classification")

  # CROSS VALIDATION
  set.seed(3333)
  cv_folds = train %>%
    rsample::vfold_cv(cv_fold_n, strata = !!target)

  if(parallel){
    doParallel::registerDoParallel(cores = 2)
  }

  set.seed(999)

  message('tuning elastic net')
  glm_cv_results_tbl <<- tune::tune_grid(
    glm_reg,
    form,
    resamples = cv_folds,
    grid      = tune_n,
    metrics   = yardstick::metric_set(roc_auc, f_meas, bal_accuracy, pr_auc),
    control   = tune::control_grid(verbose = TRUE)
  )

  message('tuning random forest')
  rf_cv_results_tbl <<- tune::tune_grid(
    rforest,
    form,
    resamples = cv_folds,
    grid      = tune_n,
    metrics   = yardstick::metric_set(roc_auc, f_meas, bal_accuracy, pr_auc),
    control   = tune::control_grid(verbose = TRUE)
  )

  message('tuning xgboost')
  xgb_cv_results_tbl <<- tune::tune_grid(
    xgbModel,
    form,
    resamples = cv_folds,
    grid      = tune_n,
    metrics   = yardstick::metric_set(roc_auc, f_meas, bal_accuracy, pr_auc),
    control   = tune::control_grid(verbose = TRUE)
  )

  # message('tuning support vector machine')
  # svm_cv_results_tbl <<- tune::tune_grid(
  #   svmRbf,
  #   form,
  #   resamples = cv_folds,
  #   grid      = tune_n,
  #   metrics   = yardstick::metric_set(roc_auc, f_meas, bal_accuracy, pr_auc),
  #   control   = tune::control_grid(verbose = TRUE)
  # )


  message('fitting best performing models')
  glm_reg_final <<- glm_reg %>% tune::finalize_model(parameters =  tune::select_best(glm_cv_results_tbl, tune_metric)) %>%
    parsnip::fit(formula = form, data    = train)

  rforest_final <<- rforest %>% tune::finalize_model(parameters =  tune::select_best(rf_cv_results_tbl, tune_metric)) %>%
    parsnip::fit(formula = form, data    = train)

  xgbModel_final <<- xgbModel %>% tune::finalize_model(parameters =  tune::select_best(xgb_cv_results_tbl, tune_metric)) %>%
    parsnip::fit(formula = form, data    = train)

  # svmRbf_final <<- svmRbf %>% tune::finalize_model(parameters =  tune::select_best(svm_cv_results_tbl, tune_metric)) %>%
  #   parsnip::fit(formula = form, data    = train)


  # COMPARING PERFORMANCE
  message('calculating predictions')
  pred_col <- paste0(".pred_", target_lab)

  pred_test <<-  {{test_df}} %>%
    cbind(stats::predict(log_reg, new_data = test, type = "prob") %>%
            dplyr::select(dplyr::all_of(pred_col)) %>% dplyr::rename(log_reg_y = pred_col)) %>%
    cbind(stats::predict(glm_reg_final, new_data = test, type = "prob") %>%
            dplyr::select(dplyr::all_of(pred_col)) %>% dplyr::rename(glm_reg_y = pred_col)) %>%
    cbind(stats::predict(rforest_final, new_data = test, type = "prob") %>%
            dplyr::select(dplyr::all_of(pred_col)) %>% dplyr::rename(rforest_y = pred_col)) %>%
    cbind(stats::predict(xgbModel_final, new_data = test, type = "prob") %>%
            dplyr::select(dplyr::all_of(pred_col)) %>% dplyr::rename(xgbModel_y = pred_col))
    # cbind(stats::predict(svmRbf_final, new_data = test, type = "prob")
    #         dplyr::select(dplyr::all_of(pred_col)) %>% dplyr::rename(svmRbf_y = pred_col))

  pred_test[, target] <- as.factor(pred_test[, target])

  message('plotting performance curves')
  yardstick::roc_auc(data = pred_test, truth = !!target, log_reg_y, event_level = "second") %>%
    rbind(yardstick::roc_auc(data = pred_test, truth = !!target, glm_reg_y, event_level = "second")) %>%
    rbind(yardstick::roc_auc(data = pred_test, truth = !!target, rforest_y, event_level = "second")) %>%
    rbind(yardstick::roc_auc(data = pred_test, truth = !!target, xgbModel_y, event_level = "second")) %>%
    # rbind(yardstick::roc_auc(data = pred_test, truth = !!target, svmRbf_y, event_level = "second")) %>%
    cbind(model = c('logit_model', 'elastic_net', 'random_forest', 'xgboost')) %>%
    ggplot2::ggplot(ggplot2::aes(.estimate, stats::reorder(model, .estimate))) +
    ggplot2::geom_segment(ggplot2::aes(xend = 0, yend = model),
                          show.legend = F) +
    ggplot2::geom_point(ggplot2::aes(size = .estimate),
                        show.legend = F) +
    ggplot2::geom_label(ggplot2::aes(label = round(.estimate, 2), size = 5),
                        fill = "white",
                        hjust = "inward",
                        show.legend = F) +
    ggplot2::labs(x = "Area under ROC curve", y = "Model Names",
                  title = "Comparative performance of the tuned models",
                  subtitle = "Performance is measured on testing data") +
    ggplot2::theme_minimal()

}
