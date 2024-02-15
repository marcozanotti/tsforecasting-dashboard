# function to add future frame to the original data
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


# function to generate new features
generate_features <- function(data, params, n_future) {
	
	logging::loginfo("Generating new features...")
	
	# add future frame
	data_full <- add_future_frame(data, n_future)

	data_feat <- data_full |> dplyr::group_by(id, frequency) 
	
	if (params$feat_calendar) {
		logging::loginfo("Generating calendar features...")
		data_feat <- data_feat |> 
			timetk::tk_augment_timeseries_signature(.date_var = date) |> 
			dplyr::select(-dplyr::matches("(iso)|(xts)|(index.num)"))
	}
	
	if (params$feat_holiday) {
		logging::loginfo("Generating holiday features...")
		data_feat <- data_feat |>
			timetk::tk_augment_holiday_signature(.date_var = date)
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
