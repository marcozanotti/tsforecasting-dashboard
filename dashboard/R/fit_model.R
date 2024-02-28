# function to generate initial split
generate_initial_split <- function(data, n_assess, assess_type) {

  splits <- timetk::time_series_split(
    data, date_var = date,
    initial = nrow(data) - n_assess,
    assess = n_assess,
    cumulative = ifelse(assess_type == "Expanding", TRUE, FALSE)
  )
  return(splits)

}

# function to generato cross validation split
generate_cv_split <- function(
    data, n_assess, assess_type, validation_type = "TS CV", n_folds = 5,
    seed = 1992
) {

  set.seed(seed)
  if (validation_type == "TS CV") {
    cv_splits <- modeltime.resample::time_series_cv(
      data, date_var = date,
      initial = nrow(data) - n_assess,
      assess = trunc(n_assess / n_folds),
      slice_limit = n_folds,
      cumulative = ifelse(assess_type == "Expanding", TRUE, FALSE)
    )
  } else {
    cv_splits <- rsample::vfold_cv(data, v = n_folds)
  }
  return(cv_splits)

}

#function to generate the recipe specification
generate_recipe_spec <- function(data, method) {
	
	if (method == "default") {
		
		rcp_spec <- recipes::recipe(value ~ ., data = data) |> 
			recipes::step_zv(recipes::all_predictors()) |>
			recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE) |> 
			recipes::step_rm(dplyr::ends_with(".lbl_01"), dplyr::ends_with(".lbl_1"))

	} else {
		
		method_type <- parse_method(method)
		n_feats <- get_features(data, number_only = TRUE, remove_date = TRUE)
		
		if (n_feats == 0) {
			
			logging::loginfo("Using default recipe specification")
			if (method_type == "ts") {
				
				rcp_spec <- recipes::recipe(value ~ date, data = data) # regress on date only
				
			} else if (any(method_type %in% c("ml", "dl"))) {
				
				rcp_spec <- recipes::recipe(value ~ ., data = data) |>
					# recipes::step_mutate(time_trend = 1:dplyr::n()) |> # timetk::normalize_vec(as.numeric(date), silent = TRUE)) |>
					timetk::step_timeseries_signature(date) |>
					recipes::step_rm(dplyr::matches("(diff)|(iso)|(xts)|(index.num)")) |>
					recipes::step_zv(recipes::all_predictors()) |>
					recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE) |> 
					recipes::step_rm(dplyr::ends_with(".lbl_01"), dplyr::ends_with(".lbl_1")) |> 
					recipes::step_rm(date)
				
			} else if (any(method_type %in% c("mix", "aml"))) {
				
				rcp_spec <- recipes::recipe(value ~ ., data = data) |>
					# recipes::step_mutate(time_trend = 1:dplyr::n()) |> # timetk::normalize_vec(as.numeric(date), silent = TRUE)) |>
					timetk::step_timeseries_signature(date) |>
					recipes::step_rm(dplyr::matches("(diff)|(iso)|(xts)|(index.num)")) |>
					recipes::step_zv(recipes::all_predictors()) |>
					recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE) |> 
					recipes::step_rm(dplyr::ends_with(".lbl_01"), dplyr::ends_with(".lbl_1"))
				
			} else {
				stop(paste("Unknown method type", method_type))
			}
			
		} else { # at least one feature
			
			logging::loginfo("Using pre-defined features specification")
			if (method_type == "ts") {
				
				if (method == "SARIMA") {
					rcp_spec <- recipes::recipe(value ~ ., data = data) |>
						recipes::step_rm(dplyr::contains("lag"))
				} else if (method == "Prophet") {
					rcp_spec <- recipes::recipe(value ~ ., data = data)
				} else {
					rcp_spec <- recipes::recipe(value ~ date, data = data) # regress on date only
				}
				
			} else if (any(method_type %in% c("ml", "dl"))) {
				
				if (method == "MARS") {
					rcp_spec <- recipes::recipe(value ~ ., data = data) |>
						recipes::step_rm(dplyr::contains("spline")) |> 
						recipes::step_rm(date)
				} else {
					rcp_spec <- recipes::recipe(value ~ ., data = data) |>
						recipes::step_rm(date)
				}
				
			} else if (any(method_type %in% c("mix", "aml"))) {
				
				rcp_spec <- recipes::recipe(value ~ ., data = data)
				
			} else {
				stop(paste("Unknown method type", method_type))
			}
			
		}

	}

	return(rcp_spec)
	
}

# function to generate the model specification
generate_model_spec <- function(method, params) {

  if (method == "Naive") {

    model_spec <- modeltime::naive_reg() |>
      parsnip::set_engine("naive")

  } else if (method == "Seasonal Naive") {

    model_spec <- modeltime::naive_reg() |>
    	parsnip::set_engine("snaive")

  } else if (method == "Rolling Average") {

    model_spec <- modeltime::window_reg(
      window_size = !!params$window_size
    ) |>
    	parsnip::set_engine("window_function", window_function = mean, na.rm = TRUE)

  } else if (method == "ETS") {

    if (params$auto_ets) {
      model_spec <- modeltime::exp_smoothing() |>
      	parsnip::set_engine("ets") # smooth_es
    } else {
      model_spec <- modeltime::exp_smoothing(
        error = !!params$error,
        trend = !!params$trend,
        season = !!params$season,
        damping = !!params$damping,
        smooth_level = !!params$smooth_level,
        smooth_trend = !!params$smooth_trend,
        smooth_seasonal = !!params$smooth_seasonal
      ) |>
      	parsnip::set_engine("ets") # smooth_es
    }

  } else if (method == "Theta") {

    model_spec <- modeltime::exp_smoothing() |>
    	parsnip::set_engine("theta")

  } else if (method == "SARIMA") {

    if (params$auto_arima) {
      model_spec <- modeltime::arima_reg() |>
      	parsnip::set_engine("auto_arima")
    } else {
      model_spec <- modeltime::arima_reg(
        non_seasonal_ar = !!params$non_seasonal_ar,
        non_seasonal_differences = !!params$non_seasonal_differences,
        non_seasonal_ma = !!params$non_seasonal_ma,
        seasonal_ar = !!params$seasonal_ar,
        seasonal_differences = !!params$seasonal_differences,
        seasonal_ma = !!params$seasonal_ma
      ) |>
      	parsnip::set_engine("arima")
    }

  } else if (method == "TBATS") {

    if (params$auto_tbats) {
      model_spec <- modeltime::seasonal_reg() |>
      	parsnip::set_engine("tbats")
    } else {
      model_spec <- modeltime::seasonal_reg(
        seasonal_period_1 = !!params$tbats_seasonal_period_1,
        seasonal_period_2 = !!params$tbats_seasonal_period_2,
        seasonal_period_3 = !!params$tbats_seasonal_period_3
      ) |>
      	parsnip::set_engine("tbats")
    }

  } else if (method == "STLM") {

    if (params$trend_model == "ETS") {
      if (params$auto_stlm) {
        model_spec <- modeltime::seasonal_reg() |>
        	parsnip::set_engine("stlm_ets")
      } else {
        model_spec <- modeltime::seasonal_reg(
          seasonal_period_1 = !!params$stlm_seasonal_period_1,
          seasonal_period_2 = !!params$stlm_seasonal_period_2,
          seasonal_period_3 = !!params$stlm_seasonal_period_3
        ) |>
        	parsnip::set_engine("stlm_ets")
      }
    } else {
      if (params$auto_stlm) {
        model_spec <- modeltime::seasonal_reg() |>
        	parsnip::set_engine("stlm_arima")
      } else {
        model_spec <- modeltime::seasonal_reg(
          seasonal_period_1 = !!params$stlm_seasonal_period_1,
          seasonal_period_2 = !!params$stlm_seasonal_period_2,
          seasonal_period_3 = !!params$stlm_seasonal_period_3
        ) |>
        	parsnip::set_engine("stlm_arima")
      }
    }

  } else if (method == "Prophet") {

    if (params$auto_prophet) {
      model_spec <- modeltime::prophet_reg() |>
      	parsnip::set_engine("prophet")
    } else {
      model_spec <- modeltime::prophet_reg(
        growth = !!params$growth,
        changepoint_num = !!params$changepoint_num,
        changepoint_range = !!params$changepoint_range,
        season = !!params$prophet_season,
        seasonality_yearly = !!params$seasonality_yearly,
        seasonality_weekly = !!params$seasonality_weekly,
        seasonality_daily = !!params$seasonality_daily,
        prior_scale_changepoints = !!params$prior_scale_changepoints,
        prior_scale_seasonality = !!params$prior_scale_seasonality,
        prior_scale_holidays = !!params$prior_scale_holidays,
        logistic_cap = !!params$logistic_cap,
        logistic_floor = !!params$logistic_floor
      ) |>
      	parsnip::set_engine("prophet")
    }

  } else if (method == "Linear Regression") {

    model_spec <- parsnip::linear_reg(mode = "regression") |>
    	parsnip::set_engine(engine = "lm")

  } else if (method == "Elastic Net") {

    model_spec <- parsnip::linear_reg(
      mode = "regression",
      penalty = !!params$penalty,
      mixture = !!params$mixture
    ) |>
    	parsnip::set_engine(engine = "glmnet")

  } else if (method == "MARS") {

    model_spec <- parsnip::mars(
      mode = "regression",
      num_terms = !!params$num_terms,
      prod_degree = !!params$prod_degree,
      prune_method = !!params$prune_method
    ) |>
    	parsnip::set_engine("earth") # endspan = 100

  } else if (method == "KNN") {

    model_spec <- parsnip::nearest_neighbor(
      mode = "regression",
      neighbors = !!params$neighbors
    ) |>
    	parsnip::set_engine("kknn")

  } else if (method == "SVM") {

    if (params$boundary == "Linear") {
      model_spec <- parsnip::svm_linear(
        mode = "regression",
        cost = !!params$cost,
        margin = !!params$margin
      ) |>
      	parsnip::set_engine("kernlab")
    } else {
      model_spec <- parsnip::svm_rbf(
        mode = "regression",
        cost = !!params$cost,
        margin = !!params$margin,
        rbf_sigma = !!params$rbf_sigma
      ) |>
      	parsnip::set_engine("kernlab")
    }

  } else if (method == "Random Forest") {

    model_spec <- parsnip::rand_forest(
      mode = "regression",
      mtry = !!params$rf_mtry,
      trees = !!params$rf_trees,
      min_n = !!params$rf_min_n
    ) |>
    	parsnip::set_engine("ranger")

  } else if (method == "Boosted Trees") {

    if (params$boost_method == "XGBoost") {
      model_spec <- parsnip::boost_tree(
        mode = "regression",
        mtry = !!params$boost_mtry,
        trees = !!params$boost_trees,
        min_n = !!params$boost_min_n,
        tree_depth = !!params$boost_tree_depth,
        learn_rate = !!params$boost_learn_rate,
        loss_reduction = !!params$boost_loss_reduction,
        sample_size = !!params$boost_sample_size
      ) |>
      	parsnip::set_engine("xgboost")
    } else if (params$boost_method == "LightGBM") {
      model_spec <- parsnip::boost_tree(
        mode = "regression",
        mtry = !!params$boost_mtry,
        trees = !!params$boost_trees,
        min_n = !!params$boost_min_n,
        tree_depth = !!params$boost_tree_depth,
        learn_rate = !!params$boost_learn_rate,
        loss_reduction = !!params$boost_loss_reduction,
        sample_size = !!params$boost_sample_size
      ) |>
      	parsnip::set_engine("lightgbm")
    } else {
      stop(paste("Unknown Boosting method", params$boost_method))
    }

  } else if (method == "Cubist") {

    model_spec <- parsnip::cubist_rules(
      committees = !!params$committees,
      neighbors = !!params$cub_neighbors,
      max_rules = !!params$max_rules
    ) |>
    	parsnip::set_engine("Cubist")

  } else if (method == "Feed-Forward") {

    model_spec <- parsnip::mlp(
      mode = "regression",
      hidden_units = !!params$ff_hidden_units,
      penalty = !!params$ff_penalty,
      epochs = !!params$ff_epochs,
      dropout = !!params$ff_dropout,
      learn_rate = !!params$ff_learn_rate
    ) |>
    	parsnip::set_engine("nnet")

  } else if (method == "Feed-Forward AR") {

    model_spec <- modeltime::nnetar_reg(
      mode = "regression",
      non_seasonal_ar = !!params$ffar_non_seasonal_ar,
      seasonal_ar = !!params$ffar_seasonal_ar,
      hidden_units = !!params$ffar_hidden_units,
      penalty = !!params$ffar_penalty,
      epochs = !!params$ffar_epochs,
      num_networks = !!params$ffar_num_networks
    ) |>
    	parsnip::set_engine("nnetar")

  } else if (method == "ARIMA-Boost") {

    model_spec <- modeltime::arima_boost(
      mode = "regression",
      mtry = !!params$arima_boost_mtry,
      trees = !!params$arima_boost_trees,
      min_n = !!params$arima_boost_min_n,
      tree_depth = !!params$arima_boost_tree_depth,
      learn_rate = !!params$arima_boost_learn_rate,
      loss_reduction = !!params$arima_boost_loss_reduction,
      sample_size = !!params$arima_boost_sample_size
    ) |>
    	parsnip::set_engine("auto_arima_xgboost")

  } else if (method == "Prophet-Boost") {

    model_spec <- modeltime::prophet_boost(
      mode = "regression",
      mtry = !!params$prophet_boost_mtry,
      trees = !!params$prophet_boost_trees,
      min_n = !!params$prophet_boost_min_n,
      tree_depth = !!params$prophet_boost_tree_depth,
      learn_rate = !!params$prophet_boost_learn_rate,
      loss_reduction = !!params$prophet_boost_loss_reduction,
      sample_size = !!params$prophet_boost_sample_size
    ) |>
    	parsnip::set_engine("prophet_xgboost")

  } else if (method == "H2O AutoML") {

    model_spec <- modeltime.h2o::automl_reg(mode = "regression") |>
    	parsnip::set_engine(
        engine = "h2o",
        project_name = "h2o_tsf_dashboard",
        max_models = 50,
        max_runtime_secs = !!params$h2o_max_time,
        max_runtime_secs_per_model = !!params$h2o_max_time_model,
        nfolds = !!params$h2o_nfolds,
        sort_metric = !!params$h2o_metric,
        seed = 1992
        # include_algos = c("DRF"),
        # exclude_algos = c("DeepLearning"),
        # verbosity = NULL
      )

  } else {
    stop(paste("Unknown method", method))
  }

  return(model_spec)

}

# function to set the metric set
set_metric_set <- function(metric) {

  metric <- tolower(metric)
  if (metric == "mae") {
    mtr_set <- yardstick::metric_set(mae)
  } else if (metric == "mape") {
    mtr_set <- yardstick::metric_set(mape)
  } else if (metric == "mase") {
    mtr_set <- yardstick::metric_set(mase)
  } else if (metric == "smape") {
    mtr_set <- yardstick::metric_set(smape)
  } else if (metric == "mse") {
    mtr_set <- yardstick::metric_set(mse)
  } else if (metric == "rmse") {
    mtr_set <- yardstick::metric_set(rmse)
  } else if (metric == "rmspe") {
  	mtr_set <- yardstick::metric_set(rmspe)
  } else {
    stop(paste("Unknown metric", metric))
  }
  return(mtr_set)

}

# function to generate the model specification for tuning
set_tune_parameters <- function(method, params) {

  # function to set tuning parameters
  set_tune <- function(parameter, value) {
    if (value == FALSE) {
      get_default(parameter)
    } else {
      tune::tune()
    }
  }

  if (method == "Elastic Net") {
    prm_ui_name <- params$tune_elanet
  } else if (method == "MARS") {
    prm_ui_name <- params$tune_mars
  } else if (method == "KNN") {
    prm_ui_name <- params$tune_knn
  } else if (method == "SVM") {
    if (params$tune_boundary == "Linear") {
      prm_ui_name <- params$tune_svm_linear
    } else {
      prm_ui_name <- params$tune_svm_rbf
    }
  } else if (method == "Random Forest") {
    prm_ui_name <- params$tune_rf
  } else if (method == "Boosted Trees") {
    prm_ui_name <- params$tune_boost
  } else if (method == "Cubist") {
    prm_ui_name <- params$tune_cub
  } else if (method == "Feed-Forward") {
    prm_ui_name <- params$tune_ff
  } else if (method == "Feed-Forward AR") {
    prm_ui_name <- params$tune_ffar
  } else if (method == "ARIMA-Boost") {
    prm_ui_name <- params$tune_arima_boost
  } else if (method == "Prophet-Boost") {
    prm_ui_name <- params$tune_prophet_boost
  } else {
    stop(paste("Unknown method", method))
  }

  mtd_params <- getOption("tsf.dashboard.methods_params")[[method]] # get the parameters for the method
  tune_params <- mtd_params[names(mtd_params) %in% prm_ui_name] # get the parameters to tune
  is_to_tune <- mtd_params %in% tune_params
  new_params <- purrr::map2(mtd_params, is_to_tune, set_tune) |> purrr::set_names(mtd_params)

  if (method == "SVM") {
    new_params$boundary <- params$tune_boundary
  }
  if (method == "Boosted Trees") {
    new_params$boost_method <- params$tune_boost_method
  }

  return(new_params)

}

# function to perform grid specification
generate_grid_spec <- function(method, model_spec, recipe_spec, grid_size, seed = 1992) {

  set.seed(seed)
  feature_set <- get_features(recipe_spec, remove_date = TRUE)
  updated_parameter_set <- model_spec |>
    hardhat::extract_parameter_set_dials() |>
    dials::finalize(x = feature_set)
  grid_spec <- dials::grid_latin_hypercube(updated_parameter_set, size = grid_size)
  return(grid_spec)

}

# function to perform model estimation
# the argument assess_type is not effectively used with the split function
# because it creates train and test splits! To effectively implement expanding
# and rolling window evaluation one must rely on time_series_cv() function,
# setting assess = 1 and cumulative = TRUE or FALSE. The model fitting then
# has to be performed using the fit_resamples() function (as in tuning).
fit_model <- function(data, method, params, n_assess, assess_type, seed = 1992) {

  logging::loginfo("*** Fitting Algorithm ***")
  logging::loginfo(paste("Method(s):", paste0(method, collapse = ", ")))

  check_parameters(method, params)
  set.seed(seed)

  # initial split
  logging::loginfo("Initial Split")
  splits <- generate_initial_split(data, n_assess, assess_type)
  train_tbl <- rsample::training(splits) |> dplyr::select(-dplyr::any_of(c("id", "frequency")))

  # recipe specification
  logging::loginfo("Recipe Specification")
  rcp_spec <- generate_recipe_spec(train_tbl, method)

  # model specification
  logging::loginfo("Model Specification")
  model_spec <- generate_model_spec(method, params)

  # workflow specification
  logging::loginfo("Workflow Specification")
  wkfl_spec <- workflows::workflow() |> 
  	workflows::add_recipe(rcp_spec) |> 
  	workflows::add_model(model_spec)

  # fitting
  logging::loginfo("Fitting")
  if (method == "H2O AutoML") { h2o::h2o.init() }
  wkfl_fit <- wkfl_spec |> parsnip::fit(data = train_tbl)

  return(wkfl_fit)

}

# function to perform ensemble model estimation
# it is just adding the ensemble method to the modeltime table
# because no fitting is required for simple ensembles
fit_ensemble <- function(modeltime_table, ensemble_methods, weights, seed = 1992) {

  set.seed(seed)
  ensemble_tbl <- modeltime::modeltime_table()

  if ("Average" %in% ensemble_methods) {
    ens_tmp <- modeltime.ensemble::ensemble_average(modeltime_table, type = "mean") |>
      modeltime::modeltime_table() |>
      modeltime::update_modeltime_description(.model_id = 1, .new_model_desc = "Ens - Average")
    ensemble_tbl <- modeltime::combine_modeltime_tables(ensemble_tbl, ens_tmp)
  }

  if ("Weighted Average" %in% ensemble_methods) {
    ens_tmp <- modeltime.ensemble::ensemble_weighted(
      modeltime_table, loadings = weights, scale_loadings = TRUE
    ) |>
      modeltime::modeltime_table() |>
      modeltime::update_modeltime_description(.model_id = 1, .new_model_desc = "Ens - Weighted Average")
    ensemble_tbl <- modeltime::combine_modeltime_tables(ensemble_tbl, ens_tmp)
  }

  if ("Median" %in% ensemble_methods) {
    ens_tmp <- modeltime.ensemble::ensemble_average(modeltime_table, type = "median") |>
      modeltime::modeltime_table() |>
      modeltime::update_modeltime_description(.model_id = 1, .new_model_desc = " Ens - Median")
    ensemble_tbl <- modeltime::combine_modeltime_tables(ensemble_tbl, ens_tmp)
  }

  return(ensemble_tbl)

}

# function to perform stacking model estimation
fit_stack <- function(modeltime_table, stacking_methods, resamples, seed = 1992) {

  set.seed(seed)
  stack_fit_list <- list()
  stack_tbl <- modeltime::modeltime_table()

  if ("Linear Regression" %in% stacking_methods) {
    stk_model_spec <- modeltime.ensemble::ensemble_model_spec(
      resamples,
      model_spec = parsnip::linear_reg(mode = "regression") |> parsnip::set_engine("lm"),
      control = tune::control_grid(verbose = FALSE)
    )
    stk_tmp <- stk_model_spec |>
      modeltime::modeltime_table() |>
      modeltime::update_modeltime_description(.model_id = 1, .new_model_desc = "Stk - Linear Regression")
    stack_tbl <- modeltime::combine_modeltime_tables(stack_tbl, stk_tmp)
    stack_fit_list$lm <- stk_model_spec$fit$fit
  }

  if ("Elastic Net" %in% stacking_methods) {
    stk_model_spec <- modeltime.ensemble::ensemble_model_spec(
      resamples,
      model_spec = parsnip::linear_reg(
        mode = "regression",
        penalty = get_default("penalty"),
        mixture = get_default("mixture")
      ) |> parsnip::set_engine("glmnet"),
      control = tune::control_grid(verbose = FALSE)
    )
    stk_tmp <- stk_model_spec |>
      modeltime::modeltime_table() |>
      modeltime::update_modeltime_description(.model_id = 1, .new_model_desc = "Stk - Elastic Net")
    stack_tbl <- modeltime::combine_modeltime_tables(stack_tbl, stk_tmp)
    stack_fit_list$elanet <- stk_model_spec$fit$fit
  }

  if ("Boosted Trees" %in% stacking_methods) {
    stk_model_spec <- modeltime.ensemble::ensemble_model_spec(
      resamples,
      model_spec = parsnip::boost_tree(
        mode = "regression",
        mtry = get_default("boost_mtry"),
        trees = get_default("boost_trees"),
        min_n = get_default("boost_min_n"),
        tree_depth = get_default("boost_tree_depth"),
        learn_rate = get_default("boost_learn_rate"),
        loss_reduction = get_default("boost_loss_reduction"),
        sample_size = get_default("boost_sample_size")
      ) |>
      	parsnip::set_engine("xgboost"),
      control = tune::control_grid(verbose = FALSE)
    )
    stk_tmp <- stk_model_spec |>
      modeltime::modeltime_table() |>
      modeltime::update_modeltime_description(.model_id = 1, .new_model_desc = "Stk - Boosted Trees")
    stack_tbl <- modeltime::combine_modeltime_tables(stack_tbl, stk_tmp)
    stack_fit_list$xgboost <- stk_model_spec$fit$fit
  }

  stack_res <- list("fit" = unname(stack_fit_list), "tbl" = stack_tbl)
  return(stack_res)

}

# function to perform model optimization
fit_model_tuning <- function(
    data, method, params, n_assess, assess_type,
    validation_type = "TS CV",
    n_folds = 5, validation_metric = "rmse", grid_size = 10,
    bayesian_optimization = TRUE, seed = 1992
) {

  logging::loginfo("*** Tuning Algorithm ***")
  logging::loginfo(paste("Method(s):", paste0(method, collapse = ", ")))

  params_new <- set_tune_parameters(method, params)
  check_parameters(method, params_new)
  validation_metric <- tolower(validation_metric)
  valid_metric_set <- set_metric_set(validation_metric)

  # initial split
  logging::loginfo("Initial Split")
  splits <- generate_initial_split(data, n_assess, assess_type)
  train_tbl <- rsample::training(splits) |> dplyr::select(-dplyr::any_of(c("id", "frequency")))

  # validation split
  logging::loginfo("Validation Split")
  cv_splits <- generate_cv_split(
    train_tbl, n_assess, assess_type, validation_type, n_folds, seed
  )

  # recipe specification
  logging::loginfo("Recipe Specification")
  rcp_spec <- generate_recipe_spec(train_tbl, method)

  # model specification
  logging::loginfo("Model Specification")
  model_spec <- generate_model_spec(method, params_new)

  # workflow specification
  logging::loginfo("Workflow Specification")
  wkfl_spec <- workflows::workflow() |> 
  	workflows::add_recipe(rcp_spec) |> 
  	workflows::add_model(model_spec)

  # grid specification
  # logging::loginfo("Grid Specification")
  # grid_spec <- generate_grid_spec(method, model_spec, rcp_spec, grid_size, seed)

  # tuning
  logging::loginfo("Tuning")
  doFuture::registerDoFuture()
  future::plan(strategy = "multisession", workers = parallelly::availableCores() - 1)
  if (bayesian_optimization) {
    feat_set <- get_features(rcp_spec, remove_date = TRUE)
    updated_param_set <- model_spec |> 
    	hardhat::extract_parameter_set_dials() |>
      dials::finalize(x = feat_set)
    set.seed(seed)
    tune_fit <- wkfl_spec |>
      tune::tune_bayes(
        resamples = cv_splits,
        metrics = valid_metric_set,
        initial = as.integer(params$tune_grid_size), # tune_fit (result from tune_grid)
        objective = tune::conf_bound(kappa = 0.1),
        iter = 20L, # as.integer(length(params_new) * 20) good practice
        param_info = updated_param_set,
        control = tune::control_bayes(
          save_pred = FALSE, allow_par = TRUE, verbose = TRUE, no_improve = 5L
        )
      )
  } else {
    set.seed(seed)
    tune_fit <- wkfl_spec |>
      tune::tune_grid(
        preprocessor = rcp_spec,
        resamples = cv_splits,
        metrics = valid_metric_set,
        grid = as.integer(params$tune_grid_size), # as.integer(params$tune_grid_size)
        control = tune::control_grid(
          save_pred = FALSE, allow_par = TRUE, verbose = TRUE
        )
      )
  }
  future::plan(strategy = "sequential")

  # picking best model
  logging::loginfo("Extracting Best Model")
  best_fit <- tune::show_best(tune_fit, metric = validation_metric, n = 1)

  # fitting (fit to training with optimal values)
  logging::loginfo("Fitting")
  wkfl_fit <- wkfl_spec |> 
  	tune::finalize_workflow(best_fit) |> 
  	parsnip::fit(train_tbl)

  return(wkfl_fit)

}

# function to extract model fit
parse_model_fit <- function(fitted_model_list) {
	res <- purrr::map(fitted_model_list, parsnip::extract_fit_engine)
	return(res)
}