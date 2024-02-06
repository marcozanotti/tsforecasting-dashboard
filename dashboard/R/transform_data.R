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
transform_data <- function(data, transformations, freq) {

	trf_prm <- getOption("tsf.dashboard.transfs")
	if (any(!names(transformations) %in% trf_prm)) {
		stop("Invalid transformation.")
	}
	transformations <- purrr::map(transformations, as.logical)

	if (all(!transformations)) {
		return(data)
	} else {
		logging::loginfo("Transforming data...")
		data_transf <- data
		if (transformations$log) { # Log
			logging::loginfo("Log")
			data_transf <- data_transf |> 
				dplyr::mutate(value = log1p(value))
		}
		if (transformations$boxcox) { # Box-Cox
			logging::loginfo("Box-Cox")
			data_transf <- data_transf |> 
				dplyr::mutate(value = timetk::box_cox_vec(value + 1, lambda = "auto"))
		}
		if (transformations$norm) { # Normalization
			logging::loginfo("Normalization")
			data_transf <- data_transf |> 
				dplyr::mutate(value = timetk::normalize_vec(value))
		}
		if (transformations$stand) { # Standardization
			logging::loginfo("Standardization")
			data_transf <- data_transf |> 
				dplyr::mutate(value = timetk::standardize_vec(value))
		}
		if (transformations$diff) { # Differencing
			logging::loginfo("Differencing")
			data_transf <- data_transf |> 
				dplyr::mutate(value = timetk::diff_vec(value, difference = 1)) |> 
				tidyr::drop_na()
		}
		if (transformations$sdiff) { # Seasonal differencing
			logging::loginfo("Seasonal Differencing")
			data_transf <- data_transf |> 
				dplyr::mutate(value = timetk::diff_vec(value, difference = 1, lag = freq)) |> 
				tidyr::drop_na()
		}
		return(data_transf)
	}

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

