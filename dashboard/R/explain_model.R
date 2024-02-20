# function to generate model explainer
generate_model_explainer <- function(
	fitted_model, 
	data, method, params, n_assess, assess_type, 
	features = NULL
) {
	
	logging::loginfo("*** Explaining Algorithm ***")
	logging::loginfo(paste("Method(s):", paste0(method, collapse = ", ")))
	
	# initial split
	logging::loginfo("Initial Split")
	splits <- generate_initial_split(data, n_assess, assess_type)
	train_tbl <- rsample::training(splits) |> dplyr::select(-dplyr::any_of(c("id", "frequency")))
	test_tbl <- rsample::testing(splits) |> dplyr::select(-dplyr::any_of(c("id", "frequency")))
	
	# recipe specification
	logging::loginfo("Recipe Specification")
	rcp_spec_tmp <- generate_recipe_spec(train_tbl, method)
	exp_train_tbl <- rcp_spec_tmp |> 
		recipes::prep() |> 
		recipes::bake(new_data = train_tbl)
	exp_test_tbl <- rcp_spec_tmp |> 
		recipes::prep() |> 
		recipes::bake(new_data = test_tbl)
	features <- exp_train_tbl |> dplyr::select(-value) |> names()
	rcp_spec <- recipes::recipe(value ~ ., data = exp_train_tbl)
	
	if (rlang::is_missing(fitted_model)) {
		
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
		wkfl_fit <- wkfl_spec |> parsnip::fit(data = exp_train_tbl)
		
	} else {
		if (method == "H2O AutoML") { h2o::h2o.init() }
		wkfl_fit <- fitted_model
	}
	
	# explainer
	logging::loginfo("Creating Explainer")
	if (method == "H2O AutoML") {
		explainer <- DALEXtra::explain_h2o(
			model = wkfl_fit$fit$fit,
			data = exp_test_tbl |> dplyr::select(-value),
			y = exp_test_tbl$value,
			label = method,
			type = "regression",
			colorize = FALSE
		)
		h2o::h2o.shutdown(prompt = FALSE)
	} else {
		explainer <- DALEXtra::explain_tidymodels(
			model = wkfl_fit,
			data = exp_test_tbl |> dplyr::select(-value),
			y = exp_test_tbl$value,
			label = method,
			type = "regression",
			colorize = FALSE
		)
	}
	
	exp_res <- list("explainer" = explainer, "features" = features)
	return(exp_res)
	
}

# function to get observation with features from data
get_observation <- function(data, date, method, n_assess, assess_type) {
	
	# attention: just from test data, adapt in case it is needed for train 
	splits <- generate_initial_split(data, n_assess, assess_type)
	test_tbl <- rsample::testing(splits) |> dplyr::select(-dplyr::any_of(c("id", "frequency")))
	exp_test_tbl <- test_tbl |>   
		generate_recipe_spec(method) |> 
		recipes::prep() |> 
		recipes::bake(new_data = test_tbl)
	observation <- exp_test_tbl[which(test_tbl$date == date), ]
	return(observation)
	
}

# function to explain model
explain_model <- function(explainer, type, features = NULL, observation = NULL) {
	
	if (type == "feature_importance") {
		logging::loginfo(paste("Explaining Algorithm:", type))
		res_explain <- DALEX::model_parts(explainer)
	} else if (type == "variable_response") {
		logging::loginfo(paste("Explaining Algorithm:", type))
		res_explain <- DALEX::model_profile(explainer, variable = features, type = "partial")
	} else if (type == "break_down") {
		logging::loginfo(paste("Explaining Algorithm:", type))
		res_explain <- DALEX::predict_parts(explainer, new_observation = observation, type = "break_down")
	} else if (type == "local_stability") {
		logging::loginfo(paste("Explaining Algorithm:", type))
		res_explain <- DALEX::predict_diagnostics(explainer, new_observation = observation,	variables = features)
	} else {
		stop(paste("Unknown type:", type))
	}
	return(res_explain)
	
}
