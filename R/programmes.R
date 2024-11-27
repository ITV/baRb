#' Get a tibble of spots from the BARB API
#'
#' @param min_transmission_date Start date of the spot query
#' @param max_transmission_date End date of the spot query
#' @param macro_regions whether to return macro or standard region areas
#' @param consolidated whether to return consolidated or only live viewing. Defaults to TRUE (consolidated).
#' @param station_code A station code obtained with barb_get_stations()
#' @param use_reporting_days whether to use a standard 24 hour clock or the BARB reporting clock. Defaults to FALSE (standard 24 hour clock).
#' @param async should the async API be used?
#'
#' @return A tibble of TV spots
#' @export
#'
#' @examples
#'
barb_get_programmes <- function(min_transmission_date = NULL,
                           max_transmission_date = NULL,
                           station_code = "",
                           macro_regions = FALSE,
                           consolidated = TRUE,
                           use_reporting_days = FALSE,
                           async = TRUE){

  message(glue::glue("Running programmes from {min_transmission_date} to {max_transmission_date}..."))

  programmes <- barb_manage_query(
    query_url = barb_url_programmes(async = async),
    query_params = list(
        "min_transmission_date" = min_transmission_date,
        "max_transmission_date" = max_transmission_date,
        "station_code" = station_code,
        "limit" = "5000",
        "consolidated" = consolidated,
        "use_reporting_days" = use_reporting_days
    ),
    metric = "audience_size_hundreds",
    retry_on_initial_no_response = retry_on_initial_no_response,
    fail_on_unsuccessful_pagination = fail_on_unsuccessful_pagination,
    retries = retries,
    pause_before_retry = pause_before_retry,
    async = async,
    json_processor = process_programme_json
  )

  programmes |>
    dplyr::filter(is_macro_region==macro_regions)
}

process_programme_json <- function(spot_json, metric = "audience_size_hundreds"){

  #Extract spot list from json
  spots_parsed <- spot_json$json$events |>
    tidyjson::as_tbl_json() |>
    tidyjson::spread_values(panel_region = tidyjson::jstring('panel', 'panel_region')) |>
    tidyjson::spread_values(is_macro_region = tidyjson::jlogical('panel', 'is_macro_region')) |>
    tidyjson::spread_values(station_name = tidyjson::jstring('station', 'station_name')) |>
    tidyjson::spread_values(programme_name = tidyjson::jstring('transmission_log_programme_name')) |>
    tidyjson::spread_values(programme_type = tidyjson::jstring('programme_type')) |>
    tidyjson::spread_values(standard_datetime = tidyjson::jstring('programme_start_datetime', 'standard_datetime')) |>
    tidyjson::spread_values(programme_duration = tidyjson::jdouble('programme_duration'))

  #Get audience data for non-zero spots
  audiences_parsed <- spots_parsed |>
    tidyjson::enter_object('audience_views') |>
    tidyjson::gather_array() |>
    tidyjson::spread_values(audience_code = tidyjson::jstring('audience_code')) |>
    tidyjson::spread_values(audience_description = tidyjson::jstring('description')) |>
    tidyjson::spread_values(audience_size_hundreds = tidyjson::jdouble('audience_size_hundreds')) |>
    tidyjson::spread_values(universe_size_hundreds = tidyjson::jdouble('target_size_in_hundreds')) |>
    tibble::as_tibble() |>
    dplyr::mutate(tvrs = audience_size_hundreds / universe_size_hundreds * 100)

  #Pivot audiences to columns and append zero rated spots again
  spots_audiences <- audiences_parsed |>
    dplyr::mutate(kpi_var = audience_size_hundreds) |>
    dplyr::select(document.id,
                  panel_region,
                  is_macro_region,
                  station_name,
                  programme_name,
                  programme_type,
                  standard_datetime,
                  programme_duration,
                  audience_description,
                  kpi_var) |>
    tidyr::pivot_wider(names_from = audience_description, values_from = kpi_var) |>
    janitor::clean_names()

  spots_audiences[is.na(spots_audiences) & is.numeric(spots_audiences)] <- 0
  spots_audiences[is.na(spots_audiences) & is.character(spots_audiences)] <- ""

  spots_audiences
}
