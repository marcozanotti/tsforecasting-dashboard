# set global options
set_options <- function() {

  op <- options()
  op.tsf.dashboard <- list(
  	tsf.dashboard.datasets = c(
  		"",
  		"Air Passengers", "EU Population", "Electricity Demand", 
  		"People Traffic", "Stock Price", "Tobacco Prod"
  	),
  	tsf.dashboard.frequencies = c(
  		"Auto", "Year (1)", "Semester (2)", "Quarter (4)", "Month (12)", "Week (52)", "Bus-day (5)", "Day (7)", 
  		"Hour (24)", "Half-hour (48)"
  	),
    tsf.dashboard.methods = list(
      "ts" = c("Naive", "Seasonal Naive", "Rolling Average", "ETS", "Theta", "SARIMA", "TBATS", "STLM", "Prophet"),
      "ml" = c("Linear Regression", "Elastic Net", "MARS", "KNN", "SVM", "Random Forest", "Boosted Trees", "Cubist"),
      "dl" = c("Feed-Forward", "COMING SOON!"),
      "mix" = c("Feed-Forward AR", "ARIMA-Boost", "Prophet-Boost"),
      "aml" = c("H2O AutoML", "COMING SOON!"),
      "ens" = c("Average", "Weighted Average", "Median"),
      "stk" = c("Linear Regression", "Elastic Net", "Boosted Trees"),
      "tune" = c(
        "Elastic Net", "MARS", "KNN", "SVM", "Random Forest", "Boosted Trees", "Cubist",
        "Feed-Forward", "Feed-Forward AR", "ARIMA-Boost", "Prophet-Boost"
      )
    ),
    tsf.dashboard.methods_params = list(
      "Naive" = NULL,
      "Seasonal Naive" = NULL,
      "Rolling Average" = c("window_size") |> purrr::set_names(c("Window Size")),
      "ETS" = c(
        "auto_ets", "error", "trend", "season", "damping",
        "smooth_level", "smooth_trend", "smooth_season"
      ) |> purrr::set_names(c("Auto-ETS", "Error", "Trend", "Seasonality", "Damped Trend", "Alpha", "Beta", "Gamma")),
      "Theta" = NULL,
      "SARIMA" = c(
        "auto_arima", "non_seasonal_ar", "non_seasonal_differences", "non_seasonal_ma",
        "seasonal_ar", "seasonal_differences", "seasonal_ma"
      ) |> purrr::set_names(c("Auto-ARIMA", "p", "d", "q", "P", "D", "Q")),
      "TBATS" = c("auto_tbats", "tbats_seasonal_period_1", "tbats_seasonal_period_2", "tbats_seasonal_period_3") |>
        purrr::set_names(c("Auto-TBATS", "Seasonal Period 1", "Seasonal Period 2", "Seasonal Period 3")),
      "STLM" = c("auto_stlm", "trend_model", "stlm_seasonal_period_1", "stlm_seasonal_period_2", "stlm_seasonal_period_3") |>
        purrr::set_names(c("Auto-STLM", "Trend Model", "Seasonal Period 1", "Seasonal Period 2", "Seasonal Period 3")),
      "Prophet" = c(
        "auto_prophet", "growth", "logistic_cap", "logistic_floor",
        "changepoint_num", "changepoint_range", "prophet_season",
        "seasonality_yearly", "seasonality_weekly", "seasonality_daily",
        "prior_scale_changepoints", "prior_scale_seasonality", "prior_scale_holidays"
      ) |>
        purrr::set_names(c(
          "Auto-Prophet", "Growth", "Logistic Cap", "Logistic Floor",
          "Changepoints Num", "Changepoints Range", "Seasonality",
          "Yearly Seasonality", "Weekly Seasonality", "Daily Seasonality",
          "Changepoint Flexibility", "Seasonality Stength", "Holidays Strength"
        )),
      "Linear Regression" = NULL,
      "Elastic Net" = c("penalty", "mixture") |> purrr::set_names(c("Penalty", "Mixture")),
      "MARS" = c("num_terms", "prod_degree", "prune_method") |>
        purrr::set_names(c("Num Terms", "Interactions Degree", "Prune Method")),
      "KNN" = c("neighbors") |> purrr::set_names(c("K-neighbors")),
      "SVM" = c("boundary", "cost", "margin", "rbf_sigma") |>
        purrr::set_names(c("Boundary Type", "Cost", "Margin", "Sigma")),
      "Random Forest" = c("rf_mtry", "rf_trees", "rf_min_n") |>
        purrr::set_names(c("Random Predictors", "Trees", "Min Node Size")),
      "Boosted Trees" = c(
        "boost_method",
        "boost_mtry", "boost_trees", "boost_min_n", "boost_tree_depth",
        "boost_learn_rate", "boost_loss_reduction", "boost_sample_size"
      ) |>
        purrr::set_names(c(
          "Boosting Method", "Random Predictors", "Trees", "Min Node Size",
          "Tree Depth", "Learning Rate", "Min Loss Reduction", "Sample"
        )),
      "Cubist" = c("committees", "cub_neighbors", "max_rules") |>
        purrr::set_names(c("Num Members", "Neighbors", "Max Rules")),
      "Feed-Forward" = c("ff_hidden_units", "ff_penalty", "ff_epochs") |> # "ff_dropout", "ff_learn_rate"
        purrr::set_names(c("Hidden Units", "Decay", "Epochs")), # "Dropout", "Learning Rate"
      "Feed-Forward AR" = c(
        "ffar_non_seasonal_ar", "ffar_seasonal_ar",
        "ffar_hidden_units", "ffar_penalty", "ffar_epochs", "ffar_num_networks"
      ) |> purrr::set_names(c("p", "P", "Hidden Units", "Decay", "Epochs", "Num Networks")),
      "ARIMA-Boost" = c(
        "arima_boost_mtry", "arima_boost_trees", "arima_boost_min_n",
        "arima_boost_tree_depth", "arima_boost_learn_rate", "arima_boost_loss_reduction",
        "arima_boost_sample_size"
      ) |> purrr::set_names(c(
        "Random Predictors", "Trees", "Min Node Size", "Tree Depth",
        "Learning Rate", "Min Loss Reduction", "Sample"
      )),
      "Prophet-Boost" = c(
        "prophet_boost_mtry", "prophet_boost_trees", "prophet_boost_min_n",
        "prophet_boost_tree_depth", "prophet_boost_learn_rate", "prophet_boost_loss_reduction",
        "prophet_boost_sample_size"
      ) |> purrr::set_names(c(
        "Random Predictors", "Trees", "Min Node Size", "Tree Depth",
        "Learning Rate", "Min Loss Reduction", "Sample"
      )),
      "H2O AutoML" = c("h2o_max_time", "h2o_max_time_model", "h2o_nfolds", "h2o_metric") |>
        purrr::set_names(c("Max Time (secs)", "Max Time per Model (secs)", "Folds", "Metric"))
    ),
    tsf.dashboard.transformations = c(
    	"Multiply by -1", "Add 1", "Log", "Box-Cox", "Log-Interval", "Min-Max", 
    	"Standardization", "Differencing", "Seasonal Differencing"
    ),
    tsf.dashboard.metrics = c("mae", "mape", "mase", "smape", "rmse", "rmspe"),
    tsf.dashboard.aggreg_funs = c("sum", "mean", "median")
  )
  toset <- !(names(op.tsf.dashboard) %in% names(op))
  if (any(toset)) options(op.tsf.dashboard[toset])

  return(invisible(NULL))

}

# function to convert frequency from character to numeric
parse_frequency <- function(frequency) {
  if (frequency == "Auto") {
    freq <- NULL
	} else if (frequency == "Year (1)") {
    freq <- 1
  } else if (frequency == "Semester (2)") {
    freq <- 2
  } else if (frequency == "Quarter (4)") {
    freq <- 4
  } else if (frequency == "Month (12)") {
    freq <- 12
  } else if (frequency == "Week (52)") {
    freq <- 52
  } else if (frequency == "Bus-day (5)") {
    freq <- 5
  } else if (frequency == "Day (7)") {
    freq <- 7
  } else if (frequency == "Hour (24)") {
    freq <- 24
  } else if (frequency == "Half-hour (48)") {
    freq <- 48
  } else {
    stop(paste("Unknown frequency", frequency))
  }
  return(freq)
}

# function to set the forecast horizon based on frequency
set_horizon <- function(frequency) {
	
	if (is.character(frequency)) {
		frequency <- parse_frequency(frequency)
	}
	
  if (is.null(frequency)) {
    h <- NULL
	} else if (frequency == 1) {
    h <- 6
  } else if (frequency == 2) {
    h <- 2 * 3
  } else if (frequency == 4) {
    h <- 8
  } else if (frequency == 12) {
    h <- 12
  } else if (frequency == 52 | frequency == 13) {
    h <- 13
  } else if (frequency == 5) {
    h <- 5 * 4 * 3
  } else if (frequency == 7) {
    h <- 7 * 4 * 3 + 6
  } else if (frequency == 24) {
    h <- 24 * 7
  } else if (frequency == 48) {
    h <- 48 * 7
  } else {
    stop(paste("Unknown frequency", frequency))
  }
  return(h)
}

# function to convert the frequency value (number) into the number of ACF lags
set_acf_lags <- function(frequency, horizon) {
	
	if (is.character(frequency)) {
		frequency <- parse_frequency(frequency)
	}
	
	if (is.null(frequency)) {
		lags <- NULL
	}	else if (frequency == 1) {
		lags <- horizon * 2
	} else if (frequency == 2) {
		lags <- horizon * 2
	} else if (frequency == 4) {
		lags <- horizon * 3
	} else if (frequency == 12) {
		lags <- horizon * 4
	} else if (frequency == 52 | frequency == 13) {
		lags <- horizon * 4
	} else if (frequency == 5) {
		lags <- horizon
	} else if (frequency == 7) {
		lags <- horizon
	} else if (frequency == 24) {
		lags <- horizon
	} else if (frequency == 48) {
		lags <- horizon
	} else {
		stop(paste("Unknown frequency", frequency))
	}
	return(lags)
}

# function to understand if the method is a time series or a machine learning one
parse_method <- function(method) {

  mtd <- getOption("tsf.dashboard.methods")
  if (method %in% mtd$ts) {
    res <- "ts"
  } else if (method %in% mtd$ml) {
    res <- "ml"
  } else if (method %in% mtd$dl) {
    res <- "dl"
  } else if (method %in% mtd$mix) {
    res <- "mix"
  } else if (method %in% mtd$aml) {
    res <- "aml"
  } else if (method %in% mtd$ens) {
    res <- "ens"
  } else if (method %in% mtd$stk) {
    res <- "stk"
  } else if (method %in% mtd$tune) {
    res <- "tune"
  } else {
    stop(paste("Unknown method", method))
  }
  return(res)

}

# check the parameters for ts and ml methods
check_parameters <- function(method, params) {

  mtd_prm <- getOption("tsf.dashboard.methods_params")[[method]]
  if (!all(mtd_prm %in% names(params))) {
    stop(paste("Parameters for", method, "are not correct!"))
  }

}

# parse model informations
parse_model <- function(fit, method) {
  # for ETS
  # wkfl_fit$fit$desc
  # wkfl_fit$fit$models$model_1$par
}

# function to get default parameters' values
get_default <- function(parameter, return_value = TRUE) {

  def <- list(
    "window_size" = 12, # Rolling Average
    "auto_ets" = TRUE, "error" = "additive", "trend" = "none", "season" = "none", # ETS
    "damping" = "none", "smooth_level" = 0.1, "smooth_trend" = 0, "smooth_season" = 0,
    "auto_arima" = TRUE, "non_seasonal_ar" = 0, "non_seasonal_differences" = 0, # SARIMA
    "non_seasonal_ma" = 0, "seasonal_ar" = 0, "seasonal_differences" = 0, "seasonal_ma" = 0,
    "auto_tbats" = TRUE, "tbats_seasonal_period_1" = 12, # TBATS
    "tbats_seasonal_period_2" = 0, "tbats_seasonal_period_3" = 0,
    "auto_stlm" = TRUE, "trend_model" = "ETS", "stlm_seasonal_period_1" = 12, # STLM
    "stlm_seasonal_period_2" = 0, "stlm_seasonal_period_3" = 0,
    "auto_prophet" = TRUE, "growth" = "linear", "logistic_cap" = 0, "logistic_floor" = 0, # Prophet
    "changepoint_num" = 25, "changepoint_range" = 0.8, "prophet_season" = "additive",
    "seasonality_yearly" = TRUE, "seasonality_weekly" = FALSE, "seasonality_daily" = FALSE,
    "prior_scale_changepoints" = 0.5, "prior_scale_seasonality" = 10, "prior_scale_holidays" = 10,
    "penalty" = 1, "mixture" = 0.5, # Elastic Net
    "num_terms" = 20, "prod_degree" = 1, "prune_method" = "backward", # MARS
    "neighbors" = 5, # KNN
    "boundary" = "Linear", "cost" = 1, "margin" = 0.1, "rbf_sigma" = 0.02, # SVM
    "rf_mtry" = 5, "rf_trees" = 1000, "rf_min_n" = 5, # Random Forest
    "boost_method" = "XGBoost", # Boosted Trees
    "boost_mtry" = 5, "boost_trees" = 1000, "boost_min_n" = 1, "boost_tree_depth" = 6,
    "boost_learn_rate" = 0.3, "boost_loss_reduction" = 0, "boost_sample_size" = 1,
    "committees" = 1, "cub_neighbors" = 0, "max_rules" = 20, # Cubist
    "ff_hidden_units" = 10, "ff_penalty" = 0, "ff_epochs" = 100, # Feed-Forward "ff_dropout" = 0.1, "ff_learn_rate" = 0.3,
    "ffar_non_seasonal_ar" = 1, "ffar_seasonal_ar" = 0, # Feed-Forward AR
    "ffar_hidden_units" = 10, "ffar_penalty" = 0, "ffar_epochs" = 100, "ffar_num_networks" = 20,
    "arima_boost_mtry" = 5, "arima_boost_trees" = 100, "arima_boost_min_n" = 1, "arima_boost_tree_depth" = 6, # ARIMA-Boost
    "arima_boost_learn_rate" = 0.3, "arima_boost_loss_reduction" = 0, "arima_boost_sample_size" = 1,
    "prophet_boost_mtry" = 5, "prophet_boost_trees" = 100, "prophet_boost_min_n" = 1, "prophet_boost_tree_depth" = 6, #  Prophet-Boost
    "prophet_boost_learn_rate" = 0.3, "prophet_boost_loss_reduction" = 0, "prophet_boost_sample_size" = 1,
    "h2o_max_time" = 30, "h2o_max_time_model" = 15, "h2o_nfolds" = 5, "h2o_metric" = "RMSE"
  )

  if (return_value) {
    return(def[[parameter]])
  } else {
    return(def[parameter])
  }

}

# function to clean strings
clean_chr <- function(x) {
  stringr::str_replace_all(x, "_", " ") |> stringr::str_to_title()
}

# function to clean back strings
clean_chr_inv <- function(x) {
  stringr::str_replace_all(x, " ", "_") |> stringr::str_to_lower()
}

# function to format accuracy table
format_accuracy <- function(accuracy_table, single_method = TRUE, digits = 2) {

  if (single_method == TRUE) {
    res <- accuracy_table |>
      dplyr::select(-1, -2) |>
    	dplyr::rename("Type" = ".type") |>
    	dplyr::rename_with(.fn = toupper, .cols = -c("Type")) |>
    	dplyr::relocate("ME", .after = "Type") |>
    	dplyr::relocate("RMSPE", .after = "RMSE") |>
    	dplyr::mutate(dplyr::across(where(is.numeric), ~round(., digits))) |>
      tidyr::pivot_longer(cols = -1, names_to = "Metric", values_to = "Value") |>
    	tidyr::pivot_wider(names_from = "Type", values_from = "Value")
  } else {
    res <- accuracy_table |>
      dplyr::select(-1) |>
    	dplyr::rename("Algorithm" = ".model_desc", "Type" = ".type") |>
    	dplyr::rename_with(.fn = toupper, .cols = -c("Algorithm", "Type")) |>
    	dplyr::relocate("ME", .after = "Type") |>
    	dplyr::relocate("RMSPE", .after = "RMSE") |>
    	dplyr::mutate(dplyr::across(where(is.numeric), ~round(., digits)))
  }
  return(res)

}

# function to set confidence levels
set_confidence_levels <- function(confidence_level, by = 0.05) {
  min_lvl <- min(confidence_level)
  max_lvl <- max(confidence_level)
  lvls <- seq(min_lvl, max_lvl, by = by)
  lvls <- unique(c(min_lvl, lvls, max_lvl))
  return(lvls)
}

# function to generate intitial values for back-transform differencing operations
generate_initial_values <- function(initial_values) {
	res <- tibble::tibble(
		".model_id" = NA_integer_,
		".model_desc" = "ACTUAL",
		".key" = "actual",
		".index" = initial_values$date,
		".value" = initial_values$value,
		".conf_lo" = NA_real_,
		".conf_hi" = NA_real_,
		".conf_lvl" = NA_real_
	)
	return(res)
}

# function to pare text input to numeric
parse_textinput <- function(x, format_to = "numeric") {
	x_res <- x |> 
		stringr::str_remove_all(" ") |> 
		stringr::str_split(",") |> 
		unlist()
	if (format_to == "numeric") {
		x_res <- as.numeric(x_res)
	} else if (format_to == "character") {
		x_res <- as.character(x_res)
	} else {
		stop("Unknown format_to argument.")
	}
	return(x_res)
}

# function to reset shiny inputs
reset_ids <- function(ids) {
	lapply(ids, function(id) shinyjs::reset(id))
}
