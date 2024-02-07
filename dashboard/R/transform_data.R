# function to impute missing values
impute_data <- function(data, impute = FALSE, freq) {

  if (impute == FALSE) {
    return(data)
  } else {
    logging::loginfo("Imputing missing values...")
    n2f <- trunc(nrow(data) / freq)
    p <- ifelse(n2f < 1, 1, 2)
    data_impute <- data |> 
    	dplyr::mutate(value = timetk::ts_impute_vec(value, period = p, lambda = "auto"))
    return(data_impute)
  }

}

# function to transform data
transform_data <- function(data, transformations, frequency) {

	trf_prm <- getOption("tsf.dashboard.transformations")
	if (any(!transformations %in% trf_prm)) {
		invalid_trf <- transformations[!transformations %in% trf_prm]
		stop(paste("Invalid transformation", invalid_trf))
	}
	init_params <- vector("list", length = length(trf_prm)) |> purrr::set_names(trf_prm)
	
	if (all(!trf_prm %in% transformations)) {
		data_transf <- data
	} else {
		
		logging::loginfo("Transforming data...")
		data_transf <- data
		
		# add 1 by default if any transformation is applied to avoid numerical problems
		data_transf <- data_transf |> dplyr::mutate(value = value + 1)
		
		if ("Log" %in% transformations) { # Log
			logging::loginfo("Log")
			data_transf <- data_transf |> 
				dplyr::mutate(value = log(value))
		}
		
		if ("Box-Cox" %in% transformations) { # Box-Cox
			logging::loginfo("Box-Cox")
			lambda_val <- timetk::auto_lambda(data_transf$value, method = c("guerrero")) 
			data_transf <- data_transf |> 
				dplyr::mutate(value = timetk::box_cox_vec(value, lambda = lambda_val))
			init_params[["Box-Cox"]] <- lambda_val
		}
		
		if ("Min-Max" %in% transformations) { # Min-Max Scaling
			logging::loginfo("Normalization")
			min_val <- min(data_transf$value)
			max_val <- max(data_transf$value)
			data_transf <- data_transf |> 
				dplyr::mutate(value = timetk::normalize_vec(value, min = min_val, max = max_val))
			init_params[["Min-Max"]] <- c(min_val, max_val)
		}
		
		if ("Standardization" %in% transformations) { # Standardization
			logging::loginfo("Standardization")
			mean_val <- mean(data_transf$value)
			sd_val <- sd(data_transf$value)
			data_transf <- data_transf |> 
				dplyr::mutate(value = timetk::standardize_vec(value, mean = mean_val, sd = sd_val))
			init_params[["Standardization"]] <- c(mean_val, sd_val)
		}
		
		if ("Differencing" %in% transformations) { # Differencing
			logging::loginfo("Differencing")
			init_val <- data_transf[1, ]
			data_transf <- data_transf |> 
				dplyr::mutate(value = timetk::diff_vec(value, difference = 1, silent = TRUE)) |> 
				tidyr::drop_na()
			init_params[["Differencing"]] <- init_val
		}
		
		if ("Seasonal Differencing" %in% transformations) { # Seasonal differencing
			logging::loginfo("Seasonal Differencing")
			init_seas_val <- data_transf[1:frequency, ]
			data_transf <- data_transf |> 
				dplyr::mutate(value = timetk::diff_vec(value, difference = 1, lag = !!frequency, silent = TRUE)) |>  # !! because frequency is also a column of data_transf
		    tidyr::drop_na()
			init_params[["Seasonal Differencing"]] <- init_seas_val
		}
		
	}
	
	res <- list("data_transformed" = data_transf, "transform_params" = init_params)
	return(res)
	
}

# function to transform data back
back_transform_data <- function(
	data, transform = FALSE, cols_to_transform, 
	transformations, transform_params, frequency
) {
	
	trf_prm <- getOption("tsf.dashboard.transformations")
	if (any(!transformations %in% trf_prm)) {
		invalid_trf <- transformations[!transformations %in% trf_prm]
		stop(paste("Invalid transformation", invalid_trf))
	}
	
	if (all(!trf_prm %in% transformations) | transform == FALSE) {
		data_back_transf <- data
	} else {
		
		logging::loginfo("Back transforming data...")
		is_frc_data <- all(
			names(data) %in% c(
				".model_id", ".model_desc", ".key", ".index", ".value", ".conf_lo", ".conf_hi", ".conf_lvl"
			)
		)
		if (is_frc_data) {
			data_back_transf <- data |> dplyr::group_by(.model_id)
		} else {
			data_back_transf <- data
		}
		
		if ("Seasonal Differencing" %in% transformations) { # Seasonal differencing
			logging::loginfo("Seasonal Differencing")
			if (is_frc_data) {
				init_seas_val <- generate_initial_values(transform_params[["Seasonal Differencing"]])
				init_seas_val_empty <- init_seas_val |> dplyr::mutate(.value = NA_real_)
			} else {
				init_seas_val <- transform_params[["Seasonal Differencing"]]
				init_seas_val_empty <- init_seas_val |> dplyr::mutate(value = NA_real_)
			}
			data_back_transf <- data_back_transf |> 
				tibble::add_row(init_seas_val_empty, .before = 1) |>
				dplyr::mutate(
					dplyr::across(
						.cols = cols_to_transform, 
						.fns = ~ timetk::diff_inv_vec(., difference = 1, lag = !!frequency, initial_values = init_seas_val$value)
					)
				)
		}
		
		if ("Differencing" %in% transformations) { # Differencing
			logging::loginfo("Differencing")
			if (is_frc_data) {
				init_val <- generate_initial_values(transform_params[["Differencing"]])
				init_val_empty <- init_seas_val |> dplyr::mutate(.value = NA_real_)
			} else {
				init_val <- transform_params[["Differencing"]]
				init_val_empty <- init_val |> dplyr::mutate(value = NA_real_)
			}
			data_back_transf <- data_back_transf |> 
				tibble::add_row(init_val_empty, .before = 1) |> 
				dplyr::mutate(
					dplyr::across(
						.cols = cols_to_transform, 
						.fns = ~ timetk::diff_inv_vec(., difference = 1, initial_values = init_val$value)
					)
				)
		}
		
		if ("Standardization" %in% transformations) { # Standardization
			logging::loginfo("Standardization")
			mean_val <- transform_params[["Standardization"]][1]
			sd_val <- transform_params[["Standardization"]][2]
			data_back_transf <- data_back_transf |> 
				dplyr::mutate(
					dplyr::across(
						.cols = cols_to_transform, 
						.fns = ~ timetk::standardize_inv_vec(., mean = mean_val, sd = sd_val)
					)
				)
		}
		
		if ("Min-Max" %in% transformations) { # Min-Max Scaling
			logging::loginfo("Normalization")
			min_val <- transform_params[["Min-Max"]][1]
			max_val <- transform_params[["Min-Max"]][2]
			data_back_transf <- data_back_transf |> 
				dplyr::mutate(
					dplyr::across(
						.cols = cols_to_transform, 
						.fns = ~ timetk::normalize_inv_vec(., min = min_val, max = max_val)
					)
				)
		}
		
		if ("Box-Cox" %in% transformations) { # Box-Cox
			logging::loginfo("Box-Cox")
			lambda_val <- transform_params[["Box-Cox"]] 
			data_back_transf <- data_back_transf |> 
				dplyr::mutate(
					dplyr::across(
						.cols = cols_to_transform, 
						.fns = ~ timetk::box_cox_inv_vec(., lambda = lambda_val)
					)
				)
		}
		
		if ("Log" %in% transformations) { # Log
			logging::loginfo("Log")
			data_back_transf <- data_back_transf |> 
				dplyr::mutate(
					dplyr::across(
						.cols = cols_to_transform, 
						.fns = ~ exp(.)
					)
				)
		}
		
		# subtract 1 by default if any transformation is applied to avoid numerical problems
		data_back_transf <- data_back_transf |> 
			dplyr::mutate(
				dplyr::across(
					.cols = cols_to_transform, 
					.fns = ~ (. - 1)
				)
			)
		
		data_back_transf <- data_back_transf |> dplyr::ungroup()
		
	}
	
	return(data_back_transf)
	
}

# function to back-transform the accuracy metrics
back_transform_accuracy <- function(
		data, calibration_table, n_assess, assess_type,
		transform = FALSE, transformations, transform_params, frequency
) {
	
	trf_prm <- getOption("tsf.dashboard.transformations")
	if (any(!transformations %in% trf_prm)) {
		invalid_trf <- transformations[!transformations %in% trf_prm]
		stop(paste("Invalid transformation", invalid_trf))
	}
	
	splits <- generate_initial_split(data, n_assess, assess_type)
	train_tbl <- rsample::training(splits) |> dplyr::select(-id, -frequency)
	test_tbl <- rsample::testing(splits) |> dplyr::select(-id, -frequency)
	
	# add to the default metric set
	new_mset <- modeltime::default_forecast_accuracy_metric_set(me, rmspe) 
	
	if (transform == TRUE) {
		logging::loginfo("Back transforming accuracy table...")
		for (i in 1:nrow(calibration_table)) {
			calibration_table$.calibration_data[[i]] <-	back_transform_data(
				data = calibration_table$.calibration_data[[i]],
				transform = transform, 
				cols_to_transform = c(".actual", ".prediction"), 
				transformations = transformations, 
				transform_params = transform_params, 
				frequency = frequency
			) |> 
				dplyr::mutate(.residuals = .actual - .prediction)
		}
	} 

	accuracy_tbl <- dplyr::bind_rows(
		calibration_table |>
			modeltime::modeltime_accuracy(new_data = train_tbl, metric_set = new_mset) |>
			dplyr::mutate(.type = "Train"),
		calibration_table |>
			modeltime::modeltime_accuracy(metric_set = new_mset) |>
			dplyr::mutate(.type = "Test")
	)
	
	return(accuracy_tbl)
	
}


# function to clean data from anomalies
clean_data <- function(data, clean = FALSE) {

  if (clean == FALSE) {
    return(data)
  } else {
    logging::loginfo("Cleaning data from anomalies...")
    data_clean <- data |> 
    	dplyr::mutate(value = timetk::ts_clean_vec(value))
    return(data_clean)
  }

}

