source("dashboard/R/utils-packages.R")

pkgs <- c(
	"devtools",
	"tidyverse", "tsibbledata", "janitor", "skimr", "vroom", "logging",
	"forecast", "prophet", "glmnet", "earth", "kernlab", "kknn",
	"randomForest", "ranger", "xgboost", "bonsai", "lightgbm", 
	"Cubist", "rules",
	"tidymodels", "vip",
	"modeltime", "modeltime.ensemble", "modeltime.resample", "modeltime.h2o",
	"plotly", "DT", "timetk", "ggcorrplot", "ppsr",
	"DALEX", "DALEXtra",
	"rmarkdown", "flexdashboard",
	"shiny", "shinyWidgets", "shinyjs"
)
install_and_load(pkgs)

if (!require(modeltime.h20)) {
	devtools::install_github("business-science/modeltime.h2o")
	library(modeltime.h2o)
}