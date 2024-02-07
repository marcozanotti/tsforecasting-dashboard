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
		# add 1 by default if any transformation is applied to avoid numerical problems
		data_transf <- data |> dplyr::mutate(value = value + 1)
		
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
			init_params[['Box-Cox']] <- lambda_val
		}
		
		if ("Min-Max" %in% transformations) { # Min-Max Scaling
			logging::loginfo("Normalization")
			min_val <- min(data_transf$value)
			max_val <- max(data_transf$value)
			data_transf <- data_transf |> 
				dplyr::mutate(value = timetk::normalize_vec(value, min = min_val, max = max_val))
			init_params[['Min-Max']] <- c(min_val, max_val)
		}
		
		if ("Standardization" %in% transformations) { # Standardization
			logging::loginfo("Standardization")
			mean_val <- mean(data_transf$value)
			sd_val <- sd(data_transf$value)
			data_transf <- data_transf |> 
				dplyr::mutate(value = timetk::standardize_vec(value, mean = mean_val, sd = sd_val))
			init_params[['Standardization']] <- c(mean_val, sd_val)
		}
		
		if ("Differencing" %in% transformations) { # Differencing
			logging::loginfo("Differencing")
			init_val <- data_transf[1, ]
			data_transf <- data_transf |> 
				dplyr::mutate(value = timetk::diff_vec(value, difference = 1, silent = TRUE)) |> 
				tidyr::drop_na()
			init_params[['Differencing']] <- init_val
		}
		
		if ("Seasonal Differencing" %in% transformations) { # Seasonal differencing
			logging::loginfo("Seasonal Differencing")
			init_seas_val <- data_transf[1:frequency, ]
			data_transf <- data_transf |> 
				dplyr::mutate(value = timetk::diff_vec(value, difference = 1, lag = !!frequency, silent = TRUE)) |>  # !! because frequency is also a column of data_transf
		    tidyr::drop_na()
			init_params[['Seasonal Differencing']] <- init_seas_val
		}
		
	}
	
	res <- list("data_transformed" = data_transf, "transform_params" = init_params)
	return(res)
	
}

back_transform_data <- function(data, transformations, init_params) {

	trf_prm <- getOption("tsf.dashboard.transformations")
	if (any(!transformations %in% trf_prm)) {
		invalid_trf <- transformations[!transformations %in% trf_prm]
		stop(paste("Invalid transformation", invalid_trf))
	}
	
	if (all(!trf_prm %in% transformations)) {
		data_back_transf <- data
	} else {
		
	}
	
	init_val_empty <- init_val |> dplyr::mutate(value = NA_real_)
	init_seas_empty <- init_seas_val |> dplyr::mutate(value = NA_real_)
	
	data_back_transf <- data_transf |> 
		tibble::add_row(init_seas_empty, .before = 1) |>
		dplyr::mutate(value = timetk::diff_inv_vec(value, difference = 1, lag = !!frequency, initial_values = init_seas_val$value)) |> 
		tibble::add_row(init_val_empty, .before = 1) |> 
		dplyr::mutate(value = timetk::diff_inv_vec(value, difference = 1, initial_values = init_val$value)) |> 
		dplyr::mutate(value = timetk::standardize_inv_vec(value, mean = mean_val, sd = sd_val)) |>
		dplyr::mutate(value = timetk::normalize_inv_vec(value, min = min_val, max = max_val)) |>
		dplyr::mutate(value = timetk::box_cox_inv_vec(value, lambda = lambda_val)) |>
		dplyr::mutate(value = exp(value)) |>
		dplyr::mutate(value = value - 1)
	
	return(data_back_transf)
		
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

