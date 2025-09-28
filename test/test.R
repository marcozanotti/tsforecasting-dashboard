# TEST & EVALUATE ---------------------------------------------------------
data <- data_selected <- get_data(datasets[2])
ts_freq <- data_selected$frequency |> unique() |> parse_frequency()
input <- list(
  n_future = 12,
  n_assess = 24,
  assess_type = "Rolling",
  method = "ETS",
  auto_ets = TRUE,
  error = "auto",
  trend = "auto",
  season = "auto",
  damping = "auto",
  smooth_level = 0.1,
  smooth_trend = 0.1,
  smooth_season = 0.1
)
input <- list(
  n_future = 12,
  n_assess = 24,
  assess_type = "Rolling",
  method = "Elastic Net",
  penalty = 1,
  mixture = 0.5
)
input <- list(
  n_future = 12,
  n_assess = 24,
  assess_type = "Rolling",
  method = "Rolling Average",
  window_size = 12
)

generate_initial_split(data_selected, input$n_assess, "Rolling")


fitted_model <- fit_model(
  data = data_selected, method = input$method, params = input,
  n_assess = input$n_assess, assess_type = input$assess_type, seed = 1992
)
forecast_results <- generate_forecast(
  fitted_model_list = list(fitted_model), data = data_selected,
  method = input$method, n_future = input$n_future,
  n_assess = input$n_assess, assess_type = input$assess_type
)

forecast_results$splits |>
  tk_time_series_cv_plan() |>
  plot_time_series_cv_plan(date, value)
forecast_results$fit
forecast_results$residuals
forecast_results$accuracy
forecast_results$test_forecast |> plot_modeltime_forecast()
forecast_results$oos_forecast |> plot_modeltime_forecast()

forecast_results$fit |> parse_model_fit()



# COMPARE -----------------------------------------------------------------
data_selected <- get_data(datasets[1])
ts_freq <- data_selected$frequency |> unique() |> parse_frequency()
input <- list(
  n_future = 12,
  n_assess = 24,
  assess_type = "Rolling",
  method = c("ETS", "SARIMA", "H2O AutoML"),
  auto_ets = get_default("auto_ets"),
  error = get_default("error"),
  trend = get_default("trend"),
  season = get_default("season"),
  damping = get_default("damping"),
  smooth_level = get_default("smooth_level"),
  smooth_trend = get_default("smooth_trend"),
  smooth_season = get_default("smooth_season"),
  auto_arima = get_default("auto_arima"),
  non_seasonal_ar = get_default("non_seasonal_ar"),
  non_seasonal_differences = get_default("non_seasonal_differences"),
  non_seasonal_ma = get_default("non_seasonal_ma"),
  seasonal_ar = get_default("seasonal_ar"),
  seasonal_differences = get_default("seasonal_differences"),
  seasonal_ma = get_default("seasonal_ma"),
  h2o_max_time = get_default("h2o_max_time"),
  h2o_max_time_model = get_default("h2o_max_time_model"),
  h2o_nfolds = get_default("h2o_nfolds"),
  h2o_metric = get_default("h2o_metric")
)

fitted_model_list <- map(
  input$method,
  ~ fit_model(
    data = data_selected, method = ., params = input,
    n_assess = input$n_assess, assess_type = input$assess_type, seed = 1992
  )
)
forecast_results <- generate_forecast(
  fitted_model_list = fitted_model_list, data = data_selected,
  method = input$method, n_future = input$n_future,
  n_assess = input$n_assess, assess_type = input$assess_type
)

res <- map(
  input$method,
  ~ fit_model(
    data = data_selected,
    method = .,
    params = input,
    n_assess = input$n_assess,
    assess_type = input$assess_type,
    seed = 1992
  )
) |>
  generate_forecast(
    data = data_selected,
    method = input$method,
    n_future = input$n_future,
    n_assess = input$n_assess,
    assess_type = input$assess_type
  )
res$accuracy |> format_accuracy(single_method = FALSE) |> filter(Type == "Test")



# COMBINE -----------------------------------------------------------------
data_selected <- get_data(datasets[1])
ts_freq <- data_selected$frequency |> unique() |> parse_frequency()
ens_methods <- getOption("tsf.dashboard.methods")[["ens"]][1]
stk_methods <- getOption("tsf.dashboard.methods")[["stk"]]
input <- list(
  n_future = 12,
  n_assess = 24,
  assess_type = "Rolling",
  method = c("ETS", "SARIMA", "Elastic Net", "H2O AutoML"),
  ens_type = ens_methods,
  auto_ets = get_default("auto_ets"),
  error = get_default("error"),
  trend = get_default("trend"),
  season = get_default("season"),
  damping = get_default("damping"),
  smooth_level = get_default("smooth_level"),
  smooth_trend = get_default("smooth_trend"),
  smooth_season = get_default("smooth_season"),
  auto_arima = get_default("auto_arima"),
  non_seasonal_ar = get_default("non_seasonal_ar"),
  non_seasonal_differences = get_default("non_seasonal_differences"),
  non_seasonal_ma = get_default("non_seasonal_ma"),
  seasonal_ar = get_default("seasonal_ar"),
  seasonal_differences = get_default("seasonal_differences"),
  seasonal_ma = get_default("seasonal_ma"),
  penalty = get_default("penalty"),
  mixture = get_default("mixture"),
  h2o_max_time = get_default("h2o_max_time"),
  h2o_max_time_model = get_default("h2o_max_time_model"),
  h2o_nfolds = get_default("h2o_nfolds"),
  h2o_metric = get_default("h2o_metric")
)
input <- list(
  n_future = 12,
  n_assess = 24,
  assess_type = "Rolling",
  method = c("ETS", "SARIMA"),
  ens_type = ens_methods,
  auto_ets = get_default("auto_ets"),
  error = get_default("error"),
  trend = get_default("trend"),
  season = get_default("season"),
  damping = get_default("damping"),
  smooth_level = get_default("smooth_level"),
  smooth_trend = get_default("smooth_trend"),
  smooth_season = get_default("smooth_season"),
  auto_arima = get_default("auto_arima"),
  non_seasonal_ar = get_default("non_seasonal_ar"),
  non_seasonal_differences = get_default("non_seasonal_differences"),
  non_seasonal_ma = get_default("non_seasonal_ma"),
  seasonal_ar = get_default("seasonal_ar"),
  seasonal_differences = get_default("seasonal_differences"),
  seasonal_ma = get_default("seasonal_ma")
)

fitted_model_list <- map(
  input$method,
  ~ fit_model(
    data = data_selected, method = ., params = input,
    n_assess = input$n_assess, assess_type = input$assess_type, seed = 1992
  )
)

# ensemble simple
forecast_results <- generate_forecast(
  fitted_model_list = fitted_model_list, data = data_selected,
  method = input$method, n_future = input$n_future,
  n_assess = input$n_assess, assess_type = input$assess_type,
  ensemble_methods = input$ens_type, stacking_methods = NULL
)

# stacking
forecast_results <- generate_forecast(
  fitted_model_list = fitted_model_list, data = data_selected,
  method = input$method, n_future = input$n_future,
  n_assess = input$n_assess, assess_type = input$assess_type,
  ensemble_methods = NULL, stacking_methods = input$stk_type
)

fitted_model_list = fitted_model_list
data = data_selected
method = input$method
n_future = input$n_future
n_assess = input$n_assess
assess_type = input$assess_type
ensemble_methods = NULL
stacking_methods = input$stk_type


res <- map(
  input$method,
  ~ fit_model(
    data = data_selected,
    method = .,
    params = input,
    n_assess = input$n_assess,
    assess_type = input$assess_type,
    seed = 1992
  )
) |>
  generate_forecast(
    data = data_selected,
    method = input$method,
    n_future = input$n_future,
    n_assess = input$n_assess,
    assess_type = input$assess_type
  )
res$accuracy |> format_accuracy(single_method = FALSE) |> filter(Type == "Test")


forecast_results <- generate_forecast(
	fitted_model_list = fitted_model_list, data = data_selected,
	method = input$method, n_future = input$n_future,
	n_assess = input$n_assess, assess_type = input$assess_type,
	ensemble_methods = input$ens_type, stacking_methods = NULL, 
	confidence_level = c(0.05, 0.99)
)

forecast_results |> 
	back_transform_forecast(
		transform = TRUE, 
		transformations = c("Add 1"), 
		transform_params = transform_params
	)

forecast_results |> extract_ensemble_results(input$ens_type)



# OPTIMIZE ----------------------------------------------------------------

data_selected <- get_data(datasets[1])
ts_freq <- data_selected$frequency |> unique() |> parse_frequency()
input <- list(
  tune_n_future = 12,
  tune_n_assess = 24,
  tune_assess_type = "Rolling",
  tune_method = "Elastic Net",
  tune_valid_type = "TS CV",
  tune_n_folds = 5,
  tune_valid_metric = "RMSE",
  tune_grid_size = 10,
  tune_bayes = TRUE,
  tune_elanet = c("Penalty", "Mixture")
)
input <- list(
  tune_n_future = 12,
  tune_n_assess = 24,
  tune_assess_type = "Rolling",
  tune_method = "Random Forest",
  tune_valid_type = "K-Fold CV",
  tune_n_folds = 5,
  tune_valid_metric = "RMSE",
  tune_grid_size = 10,
  tune_bayes = TRUE,
  tune_rf = c("Random Predictors", "Trees")
)

data = data_selected
params = input
n_assess = input$tune_n_assess
assess_type = input$tune_assess_type
method = input$tune_method
validation_type = input$tune_valid_type
n_folds = input$tune_n_folds
validation_metric = input$tune_valid_metric
bayesian_optimization = input$tune_bayes
grid_size = input$tune_grid_size
n_future = input$tune_n_future
seed = 1992

fitted_model_list <- map(
  input$tune_method,
  ~ fit_model_tuning(
    data = data_selected, method = ., params = input,
    n_assess = input$tune_n_assess, assess_type = input$tune_assess_type,
    validation_type = input$tune_valid_type, n_folds = input$tune_n_folds,
    validation_metric = input$tune_valid_metric, grid_size = input$tune_grid_size,
    bayesian_optimization = input$tune_bayes, seed = 1992
  )
)
forecast_results <- generate_forecast(
  fitted_model_list = fitted_model_list, data = data_selected,
  method = input$tune_method, n_future = input$tune_n_future,
  n_assess = input$tune_n_assess, assess_type = input$tune_assess_type
)
forecast_results$accuracy |> format_accuracy(single_method = TRUE)

res <- map(
  input$method,
  ~ fit_model_tuning(
    data = data_selected,
    method = .,
    params = input,
    n_assess = input$n_assess,
    assess_type = input$assess_type,
    validation_type = input$valid_type,
    n_folds = input$n_folds,
    validation_metric = input$valid_metric,
    grid_size = input$grid_size,
    seed = 1992
  )
) |>
  generate_forecast(
    data = data_selected,
    method = input$method,
    n_future = input$n_future,
    n_assess = input$n_assess,
    assess_type = input$assess_type
  )
res$accuracy |> format_accuracy(single_method = TRUE)



# SCENARIO -----------------------------------------------------------------
data_selected <- get_data(datasets[1])
ts_freq <- data_selected$frequency |> unique() |> parse_frequency()
input <- list(
  n_future = 120,
  n_assess = 24,
  assess_type = "Rolling",
  method = "ETS",
  auto_ets = TRUE,
  error = "additive",
  trend = "additive",
  season = get_default("season"),
  damping = get_default("damping"),
  smooth_level = get_default("smooth_level"),
  smooth_trend = 0.1,
  smooth_season = get_default("smooth_season"),
  conf_lvl = 0.9,
  aggreg_fun = "sum",
  adjust = 5
)

input <- list(
  n_future = 120,
  n_assess = 24,
  assess_type = "Rolling",
  method = "Naive",
  conf_lvl = 0.9,
  aggreg_fun = "sum",
  adjust = 5
)

data = data_selected
method = input$method
params = input
n_assess = input$n_assess
assess_type = input$assess_type
seed = 1992
n_future = input$n_future
confidence_level = input$conf_lvl
aggregate_fun = input$aggreg_fun
adjustment = input$adjust

fitted_model_list <- map(
  input$method,
  ~ fit_model(
    data = data_selected, method = ., params = input,
    n_assess = input$n_assess, assess_type = input$assess_type, seed = 1992
  )
)
forecast_results <- generate_forecast(
  fitted_model_list = fitted_model_list, data = data_selected,
  method = input$method, n_future = input$n_future,
  n_assess = input$n_assess, assess_type = input$assess_type,
  confidence_level = c(0.05, 0.99)
)

forecast_results$oos_forecast |> adjust_forecast(adjustment)
forecast_results$oos_forecast |> aggregate_forecast(n_future, confidence_level, aggregate_fun)

forecast_results$oos_forecast |>
  adjust_forecast(adjustment) |>
  aggregate_forecast(n_future, confidence_level, aggregate_fun)


forecast_results$oos_forecast |>
  dplyr::filter(.model_desc == "ACTUAL" | .conf_lvl == "0.9") |>
  plot_modeltime_forecast()

forecast_results$oos_forecast |>
  adjust_prediction_interval(type = "linear", beta = 1) |>
  dplyr::filter(.model_desc == "ACTUAL" | .conf_lvl == "0.9") |>
  plot_modeltime_forecast()

forecast_results$oos_forecast |>
  adjust_prediction_interval(type = "exponential", beta = 1.05) |>
  dplyr::filter(.model_desc == "ACTUAL" | .conf_lvl == "0.9") |>
  plot_modeltime_forecast()


f <- forecast_results$oos_forecast

forecast_tbl <- f |>
  dplyr::select(-dplyr::contains(".conf")) |>
  dplyr::distinct(.keep_all = TRUE)

conf_lo_tbl <- f |>
  dplyr::select(.index, .conf_lo, .conf_lvl) |>
  tidyr::drop_na() |>
  tidyr::pivot_longer(cols = -c(.index, .conf_lvl), names_to = ".interval", values_to = ".value") |>
  # tidyr::unite(".interval_full", c(.interval, .conf_lvl), sep = "_") |>
  dplyr::arrange(.index)

conf_hi_tbl <- f |>
  dplyr::select(.index, .conf_hi, .conf_lvl) |>
  tidyr::drop_na() |>
  tidyr::pivot_longer(cols = -c(.index, .conf_lvl), names_to = ".interval", values_to = ".value") |>
  # tidyr::unite(".interval_full", c(.interval, .conf_lvl), sep = "_") |>
  dplyr::arrange(.index)

g <- f |>
  dplyr::filter(.key == "actual" | .conf_lvl == "0.9") |>
  modeltime::plot_modeltime_forecast(
    .interactive = FALSE, .conf_interval_show = FALSE, .legend_show = FALSE
  ) +
  ggplot2::geom_line(aes(x = .index, y = .conf_lo), col = "darkred", linetype = 2) +
  ggplot2::geom_line(aes(x = .index, y = .conf_hi), col = "darkgreen", linetype = 2)


  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = .conf_lo, ymax = .conf_hi, color = .model_desc), fill = .conf_interval_fill,
                       alpha = .conf_interval_alpha, linetype = 0)
g <- plotly::ggplotly(g, dynamicTicks = TRUE)

ggplot() +
  geom_line(aes(x = .index, y = .value, group = .key, colour = .key), data = forecast_tbl) +
  geom_line(aes(x = .index, y = .value, group = .conf_lvl), data = conf_lo_tbl) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = .conf_lo, ymax = .conf_hi, color = .model_desc), fill = .conf_interval_fill,
    alpha = .conf_interval_alpha, linetype = 0)



# XAI ---------------------------------------------------------------------
data_selected <- get_data(datasets[1])
ts_freq <- data_selected$frequency |> unique() |> parse_frequency()
input <- list(
	n_future = 120,
	n_assess = 24,
	assess_type = "Rolling",
	method = "ETS",
	auto_ets = TRUE,
	error = "additive",
	trend = "additive",
	season = get_default("season"),
	damping = get_default("damping"),
	smooth_level = get_default("smooth_level"),
	smooth_trend = 0.1,
	smooth_season = get_default("smooth_season")
)

input <- list(
	n_future = 120,
	n_assess = 24,
	assess_type = "Rolling",
	method = "H2O AutoML",
	h2o_max_time = 10,
	h2o_max_time_model = 10, 
	h2o_nfolds = 2,
	h2o_metric = "RMSE"
)

data = data_selected
method = input$method
params = input
n_assess = input$n_assess
assess_type = input$assess_type
seed = 1992
n_future = input$n_future

fitted_model_list <- map(
	input$method,
	~ fit_model(
		data = data_selected, method = ., params = input,
		n_assess = input$n_assess, assess_type = input$assess_type, seed = 1992
	)
)

forecast_results <- generate_forecast(
	fitted_model_list = fitted_model_list, data = data_selected,
	method = input$method, n_future = input$n_future,
	n_assess = input$n_assess, assess_type = input$assess_type
)


get_features(forecast_results$fit[[1]], names_only = TRUE)

explainer <- generate_model_explainer(data, method, params, n_assess, assess_type)

date = ymd("1959-02-01")
observation <- get_observation(data, date, method, n_assess, assess_type)
features = "date_week4"

res1 <- explain_model(explainer, type = "feature_importance")
res2 <- explain_model(explainer, type = "variable_response", features = features)
res3 <- explain_model(explainer, type = "break_down", observation = observation)
res4 <- explain_model(explainer, type = "local_stability", features = features, observation = observation)

plot(res4)



# Graphics ----------------------------------------------------------------

data_selected <- get_data(datasets[1])
ts_freq <- data_selected$frequency |> unique() |> parse_frequency()

# Vizualize
data_selected |> 
	plot_time_series(date, value, .color_var = lubridate::year(date), .smooth = FALSE)

# Feature Eng
data_selected |>
	plot_time_series_regression(
		.date_var     = date,
		.formula      = log(value) ~ as.numeric(date) + lubridate::month(date, label = TRUE),
		.interactive  = TRUE,
		.show_summary = FALSE
	)

# distribution plot of value
g <- data_selected |> 
  ggplot2::ggplot(ggplot2::aes(x = value)) +
  ggplot2::geom_density(fill = "lightblue", alpha = 0.5) +
  ggplot2::labs(title = NULL, x = NULL, y = NULL) +
  timetk:::theme_tq()
plotly::ggplotly(g, dynamicTicks = TRUE)

	

# Transformations ---------------------------------------------------------

data_selected <- get_data(datasets[1])
ts_freq <- data_selected$frequency |> unique() |> parse_frequency()
input <- list(
	"transf" = c(transf) # NULL
)
res <- transform_data(data_selected, input$transf, ts_freq) 
res |> purrr::pluck("data_transformed") |> 
	back_transform_data(input$transf, res$transform_params, ts_freq)

# test on forecasts 
input <- list(
	transf = transf[2:7],
	n_future = 12,
	n_assess = 24,
	assess_type = "Rolling",
	method = "ETS",
	auto_ets = TRUE,
	error = "auto",
	trend = "auto",
	season = "auto",
	damping = "auto",
	smooth_level = 0.1,
	smooth_trend = 0.1,
	smooth_season = 0.1
)

input <- list(
	transf = transf[1:4],
	n_future = 12,
	n_assess = 24,
	assess_type = "Rolling",
	method = "Naive"
)

data_transformed <- transform_data(data_selected, input$transf, ts_freq)$data_transformed
transform_params <- transform_data(data_selected, input$transf, ts_freq)$transform_params

fitted_model_list <- map(
	input$method,
	~ fit_model(
		data = data_transformed, method = ., params = input,
		n_assess = input$n_assess, assess_type = input$assess_type, seed = 1992
	)
)

forecast_results <- generate_forecast(
	fitted_model_list = fitted_model_list, data = data_transformed,
	method = input$method, n_future = input$n_future,
	n_assess = input$n_assess, assess_type = input$assess_type, 
	confidence_level = c(0.8, 0.95)
)

data = data_selected
method = input$method
params = input
n_assess = input$n_assess
assess_type = input$assess_type
seed = 1992
data_splits = fitted_model$splits
fitted_model = fitted_model$fit
n_future = input$n_future

forecast_results$splits |>
	tk_time_series_cv_plan() |>
	plot_time_series_cv_plan(date, value)
forecast_results$fit
forecast_results$residuals
forecast_results$accuracy

forecast_results$test_forecast |> plot_modeltime_forecast()
forecast_results$test_forecast |> 
	back_transform_data(
		cols_to_transform = c(".value", ".conf_lo", ".conf_hi"),
		transform = TRUE,
		transformations = input$transf, transform_params = transform_params
	) |> 
	plot_modeltime_forecast()

forecast_results$oos_forecast |> plot_modeltime_forecast()
forecast_results$oos_forecast |> 
	back_transform_data(
		cols_to_transform = c(".value", ".conf_lo", ".conf_hi"),
		transform = TRUE,
		transformations = input$transf, transform_params = transform_params
	) |> 
	plot_modeltime_forecast()

forecast_results$accuracy
back_transform_accuracy(
	calibration_table = forecast_results$calibration, 
	splits = forecast_results$splits,
	transform = TRUE,
	transformations = input$transf, transform_params = transform_params
)

forecast_results$residuals
forecast_results$residuals |> 
	back_transform_data(
		cols_to_transform = c(".actual", ".prediction"),
		transform = TRUE,
		transformations = input$transf, transform_params = transform_params
	) |> 
	dplyr::mutate(.residuals = .actual - .prediction)

forecast_results |> 
	back_transform_forecast(
		transform = TRUE, transformations = input$transf,	transform_params = transform_params
	)



# External Features -------------------------------------------------------

data <- data_selected <- get_data(datasets[1])
ts_freq <- data_selected$frequency |> unique() |> parse_frequency()
n_future <- 12

data_selected |> 
	add_future_frame(n_future + 2) |> 
	fill(value, .direction = "down") |>
	rename("xfeat1" = "value") |> 
	write_csv("dashboard/data/xfeatures.csv")

xfeatures <- read_csv("dashboard/data/xfeatures.csv")

data_selected |> 
	add_future_frame(n_future) |> 
	dplyr::left_join(xfeatures, by = c("date", "id", "frequency")) |> 
	tidyr::pivot_longer(-c(date, id, frequency), names_to = "name", values_to = "value") |>
	timetk::plot_time_series(
		.date_var = date, .value = value, .color_var = name,
		.smooth = FALSE, .interactive = TRUE
	)


xfeat_names <- get_features(xfeatures, names_only = TRUE)[-1]

data_selected |> 
	add_future_frame(n_future) |> 
	dplyr::left_join(xfeatures, by = c("date", "id", "frequency")) |>
	timetk::plot_acf_diagnostics(
		.date_var = date, .value = value, .ccf_vars = xfeat1,
		.lags = 50, 
		.show_white_noise_bars = TRUE, .show_ccf_vars_only = TRUE,
		.interactive = TRUE, .title = NULL, .y_lab = NULL,
		.facet_ncol = ifelse(length(xfeat_names) > 3, 2, 1)
	)



# Internal Features -------------------------------------------------------
data <- data_selected <- get_data(datasets[1])
ts_freq <- data_selected$frequency |> unique() |> parse_frequency()

input <- list(
	feat_n_future = 12,
	feat_calendar = TRUE,
	feat_holiday = FALSE,
	feat_fourier_p = "6, 12",
	feat_fourier_k = 1,
	feat_spline_deg = "3, 6",
	feat_lag = "12, 24",
	feat_roll = "3, 6",
	feat_inter = "week2 * wday.lbl,week3 * wday.lbl"
)
params = input

data_feat <- data |> 
	generate_features(params = input, n_future = input$feat_n_future)

data_feat |> 
	dplyr::select(-dplyr::any_of(c("id", "frequency"))) |> 
	plot_time_series_regression(
		.date_var = date,
		value ~ .,
		.show_summary = FALSE
	)

data_feat |> 
	dplyr::mutate(dplyr::across(5:ncol(data_feat), as.numeric)) |> 
	tidyr::pivot_longer(-c(date, id, frequency), names_to = "name", values_to = "value") |>
	timetk::plot_time_series(
		.date_var = date, .value = value, .color_var = name, 
		.smooth = FALSE, .interactive = TRUE, .title = NULL
	)



# Feature Selection -------------------------------------------------------
data <- data_selected <- get_data(datasets[1])
ts_freq <- data_selected$frequency |> unique() |> parse_frequency()
input <- list(
	feat_n_future = 12,
	feat_calendar = TRUE,
	feat_holiday = FALSE,
	feat_fourier_p = "6, 12",
	feat_fourier_k = 1,
	feat_spline_deg = "3, 6",
	feat_lag = "12",
	feat_roll = "3, 6",
	feat_inter = "week2 * wday.lbl,week3 * wday.lbl",
	featsel_reg_formula = "value ~ ."
)
input <- list(
	feat_n_future = 12,
	feat_calendar = FALSE,
	feat_holiday = FALSE,
	feat_fourier_p = "",
	feat_fourier_k = 0,
	feat_spline_deg = "",
	feat_lag = "1,2,12",
	feat_roll = "",
	feat_inter = "",
	featsel_reg_formula = "value ~ ."
)
data_feat <- data |> 
	generate_features(params = input, n_future = input$feat_n_future)

# CORR
data_feat |> 
	generate_correlations() |> 
	plot_feature_importance()

data_feat |> 
	dplyr::mutate(dplyr::across(5:ncol(data_feat), as.numeric)) |> 
	dplyr::select(where(is.numeric)) |> 
	tidyr::drop_na() |> 
	stats::cor() |> 
	ggcorrplot::ggcorrplot(
		type = "upper", lab = FALSE, colors = c( "darkred", "white", "darkgreen")
	) |> 
	plotly::ggplotly()

# PPS
data_feat |> 
	generate_pps() |> 
	plot_feature_importance()

# LASSO & Random Forest
data_feat |> 
	generate_model_importance(
		method = c("LASSO", "Random Forest")
	) |> 
	purrr::pluck("LASSO") |> 
	plot_feature_importance()

res <- purrr::map(
	c("LASSO", "Random Forest"), 
	~ fit_feature_model(data = data_feat, method = .x)
) |> 
	purrr::set_names(c("LASSO", "Random Forest"))

# ALL
feat_names <- get_features(data_feat, names_only = TRUE)

corr_values <- data_feat |>	generate_correlations() 
pps_values <- data_feat |> generate_pps()
imp_values <- data_feat |> 
	generate_model_importance(
		method = c("LASSO", "Random Forest"), importance_type = "absolute"
	)

input <- list(
	featsel_cor_thresh = 0,
	featsel_pps_thresh = 0,
	featsel_lasso_thresh = 0,
	featsel_rf_thresh = 0
)

data_importance <- c(list("Correlation" = corr_values, "PPS" = pps_values), imp_values) |> 
	dplyr::bind_rows()

data_importance |>
	dplyr::filter(type == "Random Forest") |> 
	plot_feature_importance()

data_importance |> 
	select_features(params = input, data_features = data_feat, n_future = 12)





# Forecast con Features ---------------------------------------------------
data_selected <- get_data(datasets[1])
ts_freq <- data_selected$frequency |> unique() |> parse_frequency()

input <- list(
	feat_n_future = 12,
	feat_calendar = TRUE,
	feat_holiday = FALSE,
	feat_fourier_p = "6, 12",
	feat_fourier_k = 1,
	feat_spline_deg = "3, 6",
	feat_lag = "12, 24",
	feat_roll = "3, 6",
	feat_inter = "week2 * wday.lbl,week3 * wday.lbl"
)

input <- list(
	feat_n_future = 12,
	feat_calendar = TRUE,
	feat_holiday = FALSE,
	feat_fourier_p = "",
	feat_fourier_k = 1,
	feat_spline_deg = "",
	feat_lag = "",
	feat_roll = "",
	feat_inter = ""
)

data = data_selected
params = input
n_future = input$feat_n_future

data_feat <- data_selected |> 
	generate_features(params = input, n_future = input$feat_n_future)

generate_recipe_spec(data_selected, "Linear Regression") |> prep() |> juice()
data_feat_rcp <- generate_recipe_spec(data_selected, "Linear Regression") |> get_features()
View(data_feat, "data_feat")
View(data_feat_rcp, "data_feat_rcp")

xfeatures <- read_csv("dashboard/data/xfeatures.csv")
data_feature_selected <- data_feat |> 
	dplyr::left_join(xfeatures, by = c("date", "id", "frequency"))
str(data_feature_selected)

generate_recipe_spec(data_feat, "SARIMA") |> get_features() |> View()

input <- list(
	n_future = 12,
	n_assess = 24,
	assess_type = "Rolling",
	use_feat_set = TRUE,
	method = "ETS",
	error = "auto",
	trend = "auto",
	season = "auto",
	damping = "auto",
	smooth_level = 0.1,
	smooth_trend = 0.1,
	smooth_season = 0.1
)
input <- list(
	n_future = 12,
	n_assess = 24,
	assess_type = "Rolling",
	use_feat_set = TRUE,
	method = "Linear Regression"
)

data_fit <- data_feature_selected |> 
	dplyr::slice_head(n = nrow(data_feature_selected) - input$n_future) |> 
	tidyr::drop_na()
future_tbl <- data_feature_selected |> dplyr::slice_tail(n = input$n_future)

data = data_fit
method = input$method
params = input
n_assess = input$n_assess
assess_type = input$assess_type
seed = 1992

fitted_model <- fit_model(
	data = data_fit, method = input$method, params = input,
	n_assess = input$n_assess, assess_type = input$assess_type, seed = 1992
)

forecast_results <- generate_forecast(
	fitted_model_list = list(fitted_model), data = data_fit,
	method = input$method, n_future = input$n_future,
	n_assess = input$n_assess, assess_type = input$assess_type, 
	future_data = future_tbl
)

forecast_results$splits |>
	tk_time_series_cv_plan() |>
	plot_time_series_cv_plan(date, value)
forecast_results$fit
forecast_results$residuals
forecast_results$accuracy
forecast_results$test_forecast |> plot_modeltime_forecast()
forecast_results$oos_forecast |> plot_modeltime_forecast()


# compare manual feature with default feature
data_selected <- get_data(datasets[1])
ts_freq <- data_selected$frequency |> unique() |> parse_frequency()

input <- list(
	feat_n_future = 12,
	feat_calendar = TRUE,
	feat_holiday = FALSE,
	feat_fourier_p = "",
	feat_fourier_k = 1,
	feat_spline_deg = "",
	feat_lag = "",
	feat_roll = "",
	feat_inter = "",
	n_future = 12,
	n_assess = 24,
	assess_type = "Rolling",
	use_feat_set = TRUE,
	method = "Linear Regression"
)

data_feat <- data_selected |> 
	generate_features(params = input, n_future = input$feat_n_future)

data = data_selected
data = data_feat
params = input
n_future = input$feat_n_future
n_assess = input$n_assess
assess_type = input$assess_type
method = input$method

fitted_model <- fit_model(
	data = data_fit, method = input$method, params = input,
	n_assess = input$n_assess, assess_type = input$assess_type, seed = 1992
)

forecast_results1 <- generate_forecast(
	fitted_model_list = list(wkfl_fit1), data = data_selected,
	method = input$method, n_future = input$n_future,
	n_assess = input$n_assess, assess_type = input$assess_type, 
	future_data = NULL
)	

future_tbl <- data_feat |> dplyr::slice_tail(n = n_future)
forecast_results2 <- generate_forecast(
	fitted_model_list = list(wkfl_fit2), data = data_feat,
	method = input$method, n_future = input$n_future,
	n_assess = input$n_assess, assess_type = input$assess_type, 
	future_data = future_tbl
)	

forecast_results1$test_forecast |> plot_modeltime_forecast()
forecast_results2$test_forecast |> plot_modeltime_forecast()
forecast_results1$oos_forecast |> plot_modeltime_forecast()
forecast_results2$oos_forecast |> plot_modeltime_forecast()


# ANOMALY DETECTION & CLEANING -----------------------------------------
data <- data_selected <- get_data(datasets[1])
ts_freq <- data_selected$frequency |> unique() |> parse_frequency()
params <- input <- list(
	anom_alpha = 0.05,
	anom_max_anomalies = 0.2
)
data |> anomaly_detection_and_cleaning(params)

data |> anomaly_detection_and_cleaning(params) |> 
	timetk::plot_anomalies(.date_var = date)

data |> anomaly_detection_and_cleaning(params) |> 
	timetk::plot_anomalies_cleaned(.date_var = date)



# Spinners -------------------------------------------------------------
waiter::autoWaiter(
	id = "waiter", 
	html = tagList(
		waiter::spin_google(), br(), br(), 
		tagAppendAttributes(style = "color:black;", p("Please wait..."))
	), 
	color = "#2c3e50"
)
shinybusy::add_busy_spinner(
	spin = "cube-grid", margins = c(10, 20), timeout = 100, color = "#2c3e50"
)


