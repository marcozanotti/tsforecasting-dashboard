# function to get the data
get_data <- function(dataset_name, path = NULL) {

  logging::loginfo(paste("Getting", dataset_name, "data..."))

  if (dataset_name == "Air Passengers") { # Monthly
    data <- tibble::tibble(
      "date" = seq.Date(as.Date("1949-01-01"), as.Date("1960-12-01"), by = "month"),
      "id" = "Air Passengers",
      "frequency" = "month",
      "value" = datasets::AirPassengers |> as.numeric()
    )
  } else if (dataset_name == "Electricity Demand") { # Half-Hourly
    data <- tibble::tibble(
      "date" = tsibbledata::vic_elec$Time,
      "id" = "Electricity Demand",
      "frequency" = "half-hour",
      "value" = tsibbledata::vic_elec$Demand
    )
  } else if (dataset_name == "Stock Price") { # Daily
    data <- tibble::tibble(
      "date" = tsibbledata::gafa_stock |> dplyr::filter(Symbol == "AAPL") |> dplyr::pull(Date),
      "id" = "Apple Stock Price",
      "frequency" = "bus-day",
      "value" = tsibbledata::gafa_stock |> dplyr::filter(Symbol == "AAPL") |> dplyr::pull(Adj_Close)
    )
  } else if (dataset_name == "Tobacco Prod") { # Quarterly
    data <- tibble::tibble(
      "date" = seq.Date(as.Date("1950-01-01"), as.Date("1998-04-01"), by = "quarter"),
      "id" = "Tobacco Prod",
      "frequency" = "quarter",
      "value" = tsibbledata::aus_production |> tidyr::drop_na() |> dplyr::pull(Tobacco)
    )
  } else if (dataset_name == "EU Population") { # Yearly
    data <- tibble::tibble(
      "date" = seq.Date(as.Date("1960-01-01"), as.Date("2017-01-01"), by = "year"),
      "id" = "EU Population",
      "frequency" = "year",
      "value" = tsibbledata::global_economy |> dplyr::filter(Country == "European Union") |> dplyr::pull(Population)
    )
  } else if (dataset_name == "People Traffic") { # Weekly
    data <- tibble::tibble(
      "date" = seq.Date(as.Date("2000-01-01"), as.Date("2005-06-01"), by = "week"),
      "id" = "People Traffic",
      "frequency" = "week",
      "value" = tsibbledata::ansett |> dplyr::group_by(Week) |> dplyr::summarise(value = sum(Passengers)) |> dplyr::pull(value)
    )
  } else if (dataset_name == "custom") {
    data <- vroom::vroom(path, delim = ",", na = c("", "NA"), show_col_types = FALSE) |>
      janitor::clean_names()
  } else {
    stop(paste("Unknown dataset", dataset_name))
  }

  return(data)

}
