# function to add future frame to the original data
# to do this operation in group_by fashion, simply map to groups !!!!!!!!!!!!!
add_future_frame <- function(data, n_future) {
	id <- data$id[1]
	freq <- data$frequency[1]
	data_future <- data |> 
		timetk::future_frame(.date_var = date, .length_out = n_future) |> 
		dplyr::mutate(id = id, frequency = freq)
	data_full <- dplyr::bind_rows(data, data_future)
	return(data_full)
}

# function to get the features from a recipe or workflow
get_features <- function(object, names_only = FALSE) {
	
	logging::loginfo("Extracting Features...")
	if (inherits(object, "workflow")) {
		feats <- object |> 
			workflows::extract_preprocessor() |> 
			recipes::prep() |> 
			recipes::bake(new_data = NULL) |> 
			dplyr::select(-value)
	} else if (inherits(object, "recipe")) {
		feats <- object |> 
			recipes::prep() |> 
			recipes::bake(new_data = NULL) |> 
			dplyr::select(-value)
	} else if (inherits(object, "data.frame")) {
		feats <- object |> 
			dplyr::select(-dplyr::any_of(c("id", "frequency", "value")))
	} else {
		stop("object must be a workflow, recipe or data.frame")
	}
	
	if (names_only) { feats <- names(feats)	}
	
	return(feats)
	
}

# function to generate the feature set
generate_feature_set <- function(recipe_spec) {
	feature_set <- recipe_spec |>
		recipes::prep() |>
		recipes::bake(new_data = NULL) |>
		dplyr::select(-date, -value)
	return(feature_set)
}

# function to get the near zero variance features
get_nzv_features <- function(
	x, freqCut = 95/5, uniqueCut = 5, saveMetrics = FALSE,	names = FALSE
) {
	if (is.null(dim(x))) 
		x <- matrix(x, ncol = 1)
	freqRatio <- apply(x, 2, function(data) {
		t <- table(data[!is.na(data)])
		if (length(t) <= 1) {
			return(0)
		}
		w <- which.max(t)
		return(max(t, na.rm = TRUE)/max(t[-w], na.rm = TRUE))
	})
	lunique <- apply(x, 2, function(data) length(unique(data[!is.na(data)])))
	percentUnique <- 100 * lunique/apply(x, 2, length)
	zeroVar <- (lunique == 1) | apply(x, 2, function(data) all(is.na(data)))
	if (saveMetrics) {
		out <- data.frame(
			freqRatio = freqRatio, percentUnique = percentUnique, zeroVar = zeroVar, 
			nzv = (freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar)
	}
	else {
		out <- which((freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar)
		names(out) <- NULL
		if (names) {
			out <- colnames(x)[out]
		}
	}
	out
}

# function to test if a vector is a zero variance vector
is_zv <- function(x) { 
	return(length(unique(x)) == 1)
}

# function to generate basis spline features assuming df = degree
# does not work by id !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
generate_spline_features <- function(.data, .degree) {
	res <- splines::bs(as.numeric(.data$date), df = .degree, degree = .degree) |> 
		tibble::as_tibble() |> 
		purrr::set_names(paste0("spline_", 1:.degree, "_", .degree)) |> 
		dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
	return(res)
}

# function to augment data with spline features
augment_spline <- function(.data, .degree) {
	spline_tbl <- .degree |> 
		purrr::map(~ generate_spline_features(.data, .x)) |> 
		dplyr::bind_cols()
	res <- dplyr::bind_cols(.data, spline_tbl)
	return(res)
}

# function to generate interaction features
augment_interaction <- function(.data, .expr) {
	res <- .data |> dplyr::mutate(!!!.expr)
	cols_to_rename <- as.character(.expr) |> 
		purrr::map_dbl(~ which(grepl(.x, names(res), fixed = TRUE)))
	colnames(res)[cols_to_rename] <- paste0("inter_", seq_along(.expr))
	res <- res |> 
		dplyr::mutate(dplyr::across(dplyr::contains("inter_"), as.numeric))
	return(res)
}

# function to generate new features
generate_features <- function(data, params, n_future) {
	
	logging::loginfo("Generating new features...")
	
	# add future frame
	data_full <- data |> 
		add_future_frame(n_future) |> 
		dplyr::group_by(id, frequency)
	data_feat <- data_full  
	
	if (params$feat_calendar) {
		logging::loginfo("Generating calendar features...")
		data_feat <- data_feat |> 
			dplyr::mutate("trend" = timetk::normalize_vec(as.numeric(date), silent = TRUE)) |> 
			timetk::tk_augment_timeseries_signature(.date_var = date) |> 
			dplyr::mutate(year = timetk::normalize_vec(year, silent = TRUE)) |> 
			dplyr::select(-dplyr::matches("(diff)|(iso)|(xts)|(index.num)"))
	}
	
	if (params$feat_holiday) {
		logging::loginfo("Generating holiday features...")
		data_feat <- data_feat |>
			timetk::tk_augment_holiday_signature(.date_var = date)
	}
	
	if (!params$feat_fourier_p == "") {
		logging::loginfo("Generating Fourier features...")
		f_p <- params$feat_fourier_p |> parse_textinput()
		data_feat <- data_feat |>
			timetk::tk_augment_fourier(
				.date_var = date, .periods = f_p,	.K = params$feat_fourier_k
			) |> 
			dplyr::rename_with(~ stringr::str_remove_all(., "date_"))
	}
	
	if (!params$feat_spline_deg == "") {
		logging::loginfo("Generating splines features...")
		s_deg <- params$feat_spline_deg |> parse_textinput()
		data_feat <- data_feat |>	augment_spline(.degree = s_deg)
	}
	
	if (!params$feat_lag == "") {
		logging::loginfo("Generating lag features...")
		lag_order <- params$feat_lag |> parse_textinput()
		data_feat <- data_feat |>	
			timetk::tk_augment_lags(.value = value, .lags = lag_order) |> 
			dplyr::rename_with(~ stringr::str_remove_all(., "value_"))
		
		if (!params$feat_roll == "") {
			logging::loginfo("Generating rolling features...")
			max_lag_order <- max(lag_order) # roll average features are computed only on the max lag order
			roll_period <- params$feat_roll |> parse_textinput()
			# rolling averages
			data_roll_mean <- data_feat |> 
				timetk::tk_augment_slidify(
					.value = paste0("lag", max_lag_order), .f = mean,
					.period = roll_period, .align = "right", .partial = FALSE
				) |> 
				dplyr::ungroup() |> 
				dplyr::select(dplyr::contains("_roll_")) |> 
				dplyr::rename_with(~ stringr::str_c("roll_mean", stringr::str_remove_all(., ".*_roll")))
			data_feat <- dplyr::bind_cols(data_feat, data_roll_mean)	
			# rolling standard deviations
			data_roll_sd <- data_feat |> 
				timetk::tk_augment_slidify(
					.value = paste0("lag", max_lag_order), .f = sd,
					.period = roll_period, .align = "right", .partial = FALSE
				) |> 
				dplyr::ungroup() |> 
				dplyr::select(dplyr::contains("_roll_")) |> 
				dplyr::rename_with(~ stringr::str_c("roll_sd", stringr::str_remove_all(., ".*_roll")))
			data_feat <- dplyr::bind_cols(data_feat, data_roll_sd)	
		}
		
	}
	
	if (!params$feat_inter == "") {
		logging::loginfo("Generating interaction features...")
		inter_expr <- params$feat_inter |> 
			parse_textinput(format_to = "character") |> 
			stringr::str_split("\\*") |> 
			purrr::map(~ stringr::str_c("as.numeric(", .x, ")")) |> # add as.numeric to each variable by default
			purrr::map(~ stringr::str_flatten(.x, collapse = "*", )) |> 
			purrr::map(rlang::parse_expr) 
		data_feat <- data_feat |> augment_interaction(.expr = inter_expr)
	}
	
	data_feat <- data_feat |>	dplyr::select(-where(is_zv)) |>	dplyr::ungroup()
	return(data_feat)

}

#function to generate the recipe specification
generate_recipe_spec <- function(data, method) {
	
	method_type <- parse_method(method)
	
	if (method_type == "ts") {
		
		rcp_spec <- recipes::recipe(value ~ ., data = data)
		
	} else if (any(method_type %in% c("ml", "dl"))) {
		
		rcp_spec <- recipes::recipe(value ~ ., data = data) |>
			timetk::step_timeseries_signature(date) |>
			recipes::step_mutate(date = as.numeric(date)) |>
			recipes::step_zv(recipes::all_predictors()) |>
			recipes::step_rm(dplyr::matches("(iso)|(xts)|(index.num)")) |>
			recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
		
	} else if (any(method_type %in% c("mix", "aml"))) {
		
		rcp_spec <- recipes::recipe(value ~ ., data = data) |>
			timetk::step_timeseries_signature(date) |>
			recipes::step_mutate(trend = as.numeric(date)) |>
			recipes::step_zv(recipes::all_predictors()) |>
			recipes::step_rm(matches("(iso)|(xts)|(index.num)")) |>
			recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
		
	} else {
		stop(paste("Unknown method type", method_type))
	}
	
	return(rcp_spec)
	
}
