# function to add future frame to the original data
# to do this operation in group_by fashion, simply map to groups !!!!!!!!!!!!!
add_future_frame <- function(data, n_future) {
	id <- data$id[1]
	data_future <- data |> 
		timetk::future_frame(.date_var = date, .length_out = n_future) |> 
		dplyr::mutate(id = id)
	data_full <- dplyr::bind_rows(data, data_future)
	return(data_full)
}

# function to get the features from a recipe or workflow
get_features <- function(object, remove_date = FALSE, names_only = FALSE, number_only = FALSE) {
	
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
		feats <- object |> dplyr::select(-dplyr::any_of(c("id", "value")))
	} else {
		stop("object must be a workflow, recipe or data.frame")
	}
	
	if (remove_date) { feats <- dplyr::select(feats, -dplyr::any_of(c("date"))) }
	if (names_only) { feats <- names(feats)	}
	if (number_only) { feats <- ncol(feats) }
	
	return(feats)
	
}

# function to generate the feature set
generate_feature_set <- function(recipe_spec) {
	feature_set <- recipe_spec |>
		recipes::prep() |>
		recipes::bake(new_data = NULL) |>
		dplyr::select(-dplyr::any_of(c("date", "value")))
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
generate_features <- function(data, params, n_future, verbose = 1) {
	
	if (verbose > 0) logging::loginfo("*** Generating New Features ***")
	
	# add future frame
	data_full <- data |> add_future_frame(n_future) # |> dplyr::group_by(id)
	data_feat <- data_full  
	
	if (params$feat_calendar) {
		if (verbose > 0) logging::loginfo("Generating calendar features...")
		data_feat <- data_feat |> 
			dplyr::mutate("time_trend" = 1:dplyr::n()) |> 
			timetk::tk_augment_timeseries_signature(.date_var = date) |> 
			dplyr::select(-dplyr::matches("(diff)|(iso)|(xts)|(index.num)")) 
	}
	
	if (params$feat_holiday) {
		if (verbose > 0) logging::loginfo("Generating holiday features...")
		data_feat <- data_feat |>
			timetk::tk_augment_holiday_signature(.date_var = date)
	}
	
	if (!params$feat_fourier_p == "") {
		if (verbose > 0) logging::loginfo("Generating Fourier features...")
		f_p <- params$feat_fourier_p |> parse_textinput()
		data_feat <- data_feat |>
			timetk::tk_augment_fourier(
				.date_var = date, .periods = f_p,	.K = params$feat_fourier_k
			) |> 
			dplyr::rename_with(~ stringr::str_remove_all(., "date_"))
	}
	
	if (!params$feat_spline_deg == "") {
		if (verbose > 0) logging::loginfo("Generating splines features...")
		s_deg <- params$feat_spline_deg |> parse_textinput()
		data_feat <- data_feat |>	augment_spline(.degree = s_deg)
	}
	
	if (!params$feat_lag == "") {
		if (verbose > 0) logging::loginfo("Generating lag features...")
		lag_order <- params$feat_lag |> parse_textinput()
		data_feat <- data_feat |>	
			timetk::tk_augment_lags(.value = value, .lags = lag_order) |> 
			dplyr::rename_with(~ stringr::str_remove_all(., "value_"))
		
		if (!params$feat_roll == "") {
			if (verbose > 0) logging::loginfo("Generating rolling features...")
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
		if (verbose > 0) logging::loginfo("Generating interaction features...")
		inter_expr <- params$feat_inter |> 
			parse_textinput(format_to = "character") |> 
			stringr::str_split("\\*") |> 
			purrr::map(~ stringr::str_c("as.numeric(", .x, ")")) |> # add as.numeric to each variable by default
			purrr::map(~ stringr::str_flatten(.x, collapse = "*", )) |> 
			purrr::map(rlang::parse_expr) 
		data_feat <- data_feat |> augment_interaction(.expr = inter_expr)
	}
	
	data_feat <- data_feat |>	
		generate_recipe_spec(method = "default") |>  
		get_features(remove_date = TRUE)
	data_feat_full <- data_full |> dplyr::bind_cols(data_feat)
	return(data_feat_full)

}

# function to generate correlation matrix
generate_correlations <- function(data, cor_method = "spearman", full_matrix = FALSE) {
	
	logging::loginfo("*** Generating Correlation Matrix ***")
	data_cor <- data |> 
		dplyr::select(-dplyr::any_of(c("id", "date"))) |> 
		tidyr::drop_na()
	
	rcp_spec <- recipes::recipe(value ~ ., data = data_cor) |> 
		recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
	
	cor_mat <- rcp_spec |> 
		recipes::prep() |> 
		recipes::bake(new_data = NULL) |> 
		dplyr::relocate(value, .before = 1) |> 
		stats::cor(method = cor_method)
	
	if (full_matrix == FALSE) {
		cor_mat <- cor_mat |> 
			as.data.frame() |> 
			tibble::rownames_to_column() |> 
			dplyr::select(1:2) |> 
			dplyr::slice(-1) |>
			purrr::set_names(c("variable", "importance")) |> 
			dplyr::arrange(desc(importance)) |> 
			dplyr::mutate("type" = "Correlation") |> 
			dplyr::filter(abs(importance) > 0)
	}

	return(cor_mat)
	
}

# function to generate predictive power score
generate_pps <- function(data) {
	
	logging::loginfo("*** Generating Predictive Power Score ***")
	data_pps <- data |> 
		dplyr::select(-dplyr::any_of(c("id", "date"))) |> 
		tidyr::drop_na()
	
	rcp_spec <- recipes::recipe(value ~ ., data = data_pps) |> 
		recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
	
	pps_df <- rcp_spec |> 
		recipes::prep() |> 
		recipes::bake(new_data = NULL) |> 
		dplyr::relocate(value, .before = 1) |> 
		ppsr::score_predictors(
			y = "value", algorithm = "tree", cv_folds = 1, do_parallel = FALSE # on train because cv_folds = 1
		) |> 
		dplyr::select("x", "pps") |>
		dplyr::slice(-1) |>
		purrr::set_names(c("variable", "importance")) |> 
		dplyr::filter(importance > 0) |>
		dplyr::arrange(desc(importance)) |> 
		dplyr::mutate("type" = "PPS")

	return(pps_df)
	
}

# function to fit a feature selection model
fit_feature_model <- function(data, method, seed = 1992) {
	
	logging::loginfo("*** Fitting Feature Algorithm ***")
	logging::loginfo(paste("Method(s):", paste0(method, collapse = ", ")))
	
	set.seed(seed)
	
	if (method == "LASSO") {
		model_spec <- parsnip::linear_reg(
			mode = "regression", penalty = 1, mixture = 1
		) |>
			parsnip::set_engine(engine = "glmnet")
	} else if (method == "Random Forest") {
		model_spec <- parsnip::rand_forest(mode = "regression") |>
			parsnip::set_engine(engine = "ranger", importance = "permutation")
	} else {
		stop(paste("Unknown method:", method))
	}
	
	data_featsel <- data |> 
		dplyr::select(-dplyr::any_of(c("id", "date"))) |> 
		tidyr::drop_na()
	
	# rcp_spec <- recipes::recipe(value ~ ., data = data_featsel) |> 
	# 	recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
	rcp_spec <- generate_recipe_spec(data = data_featsel, method = "default")
	
	wkfl_spec <- workflows::workflow() |> 
		workflows::add_model(model_spec) |> 
		workflows::add_recipe(rcp_spec)
	
	wkfl_fit <- wkfl_spec |> parsnip::fit(data = data_featsel)
	
	return(wkfl_fit)
	
}

# function to extract feature importance
extract_feature_importance <- function(
		feature_fit, method
) {
	
	logging::loginfo("*** Extracting Feature Importance ***")
	if (method == "LASSO") {
		res_imp <- feature_fit |> 
			workflows::extract_fit_parsnip() |> 
			broom::tidy() |> 
			dplyr::select(term, estimate) |> 
			purrr::set_names(c("variable", "importance")) |> 
			dplyr::mutate("type" = "LASSO") |> 
			dplyr::filter(abs(importance) > 0)
	} else if (method == "Random Forest") {
		res_imp <- feature_fit |> 
			workflows::extract_fit_parsnip() |> 
			vip::vip(num_features = 1000) |> 
			purrr::pluck("data") |> 
			purrr::set_names(c("variable", "importance")) |> 
			dplyr::mutate("type" = "Random Forest") |> 
			dplyr::filter(importance > 0)
	} else {
		stop(paste("Unknown method:", method))
	}
	
	res_imp <- res_imp |> 
		dplyr::filter(variable != "(Intercept)") |> 
		dplyr::arrange(desc(importance))
	
	return(res_imp)
	
}

# wrapper to generate model importance
generate_model_importance <- function(data, method) {
	res <- purrr::map(
		method, 
		~ fit_feature_model(data = data, method = .x)
	) |>  
		purrr::set_names(method) |> 
		purrr::map2(
			method,
			~ extract_feature_importance(.x, method = .y)
		)
	return(res)
}

# wrapper to combine importance from different methods
generate_importance <- function(data, methods) {
	
	logging::loginfo("*** Generating Feature Importance ***")
	
	importance_list <- list()
	
	if ("Correlation" %in% methods) {
		cor_mat <- generate_correlations(data = data, cor_method = "spearman", full_matrix = FALSE)
		importance_list <- c(importance_list, list("Correlation" = cor_mat))
	}
	
	if ("PPS" %in% methods) {
		pps_mat <- generate_pps(data = data)
		importance_list <- c(importance_list, list("PPS" = pps_mat))
	}
	
	model_methods <- methods[methods %in% c("LASSO", "Random Forest")]
	if (length(model_methods) > 0) {
		model_imp <- generate_model_importance(data = data, method = model_methods)
		importance_list <- c(importance_list, model_imp)
	}
	
	data_importance <- importance_list |> dplyr::bind_rows()
	
	return(data_importance)
	
}

# function to normalize importance scores
normalize_importance <- function(data_importance) {
	
	logging::loginfo("Normalizing importance values...")
	methods <- unique(data_importance$type)
	data_res <- NULL
	
	if ("Correlation" %in% methods) {
		corr_norm <- data_importance |> 
			dplyr::filter(type == "Correlation") |>
			dplyr::mutate(importance = abs(importance)) # take absolute value of correlation
		data_res <- dplyr::bind_rows(data_res, corr_norm)
	}
	if ("PPS" %in% methods) {
		pps_norm <- data_importance |> 
			dplyr::filter(type == "PPS") |>
			dplyr::mutate(importance = importance) # already between 0 and 1
		data_res <- dplyr::bind_rows(data_res, pps_norm)
	}
	if ("LASSO" %in% methods) {
		lasso_norm <- data_importance |> 
			dplyr::filter(type == "LASSO") |> 
			dplyr::mutate(importance = timetk::normalize_vec(abs(importance), silent = TRUE, min = 0))
		data_res <- dplyr::bind_rows(data_res, lasso_norm)
	}
	if ("Random Forest" %in% methods) {
		rf_norm <- data_importance |> 
			dplyr::filter(type == "Random Forest") |>
			dplyr::mutate(importance = timetk::normalize_vec(importance, silent = TRUE, min = 0))
		data_res <- dplyr::bind_rows(data_res, rf_norm)
	}
	
	return(data_res)
	
}

# function to plot feature importance
plot_feature_importance <- function(importance_data, normalized = FALSE) {
	
	if (normalized) {
		importance_data <- importance_data |> normalize_importance()
	}
	g <- importance_data |> 
		ggplot2::ggplot(
			ggplot2::aes(
				y = reorder(variable, importance), 
				x = importance, 
				fill = importance
			)
		) +
		ggplot2::geom_col() + 
		ggplot2::scale_fill_gradient(low = "#deebf7", high = "#08306B") +
		timetk:::theme_tq() +
		ggplot2::theme(legend.position = "right") +
		ggplot2::labs(y = "", x = "")
	if (normalized) {
		g <- g + ggplot2::expand_limits(x = c(0, 1))
	}
	return(g)
	
}

# function to filter importance values 
filter_importance <- function(data_importance, params) {
	
	logging::loginfo("Filtering importance values...")
	sel_feat <- data_importance |>
		dplyr::filter(
			(type == "Correlation" & importance >= params$featsel_cor_thresh) |
				(type == "PPS" & importance >= params$featsel_pps_thresh) |
				(type == "LASSO" & importance >= params$featsel_lasso_thresh) |
				(type == "Random Forest" & importance >= params$featsel_rf_thresh)
		)
	return(sel_feat)
	
}

# function to select features based on importance thresholds
select_features <- function(data_importance, params, data_features, n_future) {
	
	logging::loginfo("Selecting features...")
	n_feats <- get_features(data_features, number_only = TRUE, remove_date = TRUE)
	sum_tbl <- tibble::tibble(
		"Variable" = NA_character_, "N. Methods" = NA_real_, 
		"Average Importance" = NA_real_, "Methods" = NA_character_
	)
	data_sel_importance <- data_importance
	data_final <- data_features |> 
		dplyr::slice_head(n = nrow(data_features) - n_future) |> 
		tidyr::drop_na()
	data_future <- data_features |> dplyr::slice_tail(n = n_future)
	
	if (n_feats > 0) {

		data_sel_importance <- data_importance |> filter_importance(params = params)
		sel_feat_names <- unique(data_sel_importance$variable)

		if (length(sel_feat_names) > 0) {
			
			mtd_n <- vector("numeric", length(sel_feat_names))
			mtd_ave <- vector("numeric", length(sel_feat_names))
			mtd_type <- vector("character", length(sel_feat_names))
			for (i in seq_along(sel_feat_names)) {
				sel_data_tmp <- data_sel_importance |> dplyr::filter(variable == sel_feat_names[i])
				mtd_n[i] <- nrow(sel_data_tmp)
				mtd_ave[i] <- mean(sel_data_tmp$importance, na.rm = TRUE)
				mtd_type[i] <- sel_data_tmp |> dplyr::pull("type") |>	unique() |> stringr::str_flatten(", ")
			}
			sum_tbl <- tibble::tibble(
				"Variable" = sel_feat_names, "N. Methods" = mtd_n, 
				"Average Importance" = mtd_ave, "Methods" = mtd_type
			)
			
		}
		
		sum_tbl <- sum_tbl |> 
			dplyr::arrange(dplyr::desc(`N. Methods`), dplyr::desc(`Average Importance`)) |> 
			dplyr::mutate(`Average Importance` = round(`Average Importance`, 3))
		data_selected <- data_features |> 
			dplyr::select(
				dplyr::any_of(c("id", "date", "value")), 
				dplyr::all_of(sel_feat_names)
			)
		data_final <- data_selected |> 
			dplyr::slice_head(n = nrow(data_selected) - n_future) |> 
			tidyr::drop_na()
		data_future <- data_selected |> dplyr::slice_tail(n = n_future)
	
	}
	
	res <- list(
		"data" = data_final,
		"future_data" = data_future,
		"summary_table" = sum_tbl,
		"data_importance" = data_sel_importance
	)
	return(res)
	
}
