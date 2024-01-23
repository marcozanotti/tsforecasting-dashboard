# function to compute the mean error
me_impl <- function(truth, estimate, case_weights = NULL) {
  mean((estimate - truth)) # pay attention to this formulation
}

me_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {

  yardstick::check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick::yardstick_remove_missing(truth, estimate, case_weights)
    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick::yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  me_impl(truth, estimate, case_weights = case_weights)

}

me <- function(data, ...) {
  UseMethod("me")
}

me <- yardstick::new_numeric_metric(me, direction = "minimize")

me.data.frame <- function(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {

  yardstick::numeric_metric_summarizer(
    name = "me",
    fn = me_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )

}

# function to compute the root mean squared percentage error
rmspe_impl <- function(truth, estimate, case_weights = NULL) {
  eps <- 1e-10
  sqrt(mean(((estimate - (truth + eps)) / (truth + eps)) ^ 2)) * 100 # pay attention to this formulation
}

rmspe_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {

  yardstick::check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick::yardstick_remove_missing(truth, estimate, case_weights)
    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick::yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  rmspe_impl(truth, estimate, case_weights = case_weights)

}

rmspe <- function(data, ...) {
  UseMethod("rmspe")
}

rmspe <- yardstick::new_numeric_metric(rmspe, direction = "minimize")

rmspe.data.frame <- function(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {

  yardstick::numeric_metric_summarizer(
    name = "rmspe",
    fn = me_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )

}