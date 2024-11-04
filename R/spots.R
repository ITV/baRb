#' Get a tibble of spots from the BARB API
#'
#' @param min_transmission_date Start date of the spot query
#' @param max_transmission_date End date of the spot query
#' @param advertiser_name Advertiser name. Get names from barb_get_advertisers()
#' @param consolidated whether to return consolidated or only live viewing. Defaults to TRUE (consolidated).
#' @param use_reporting_days whether to use a standard 24 hour clock or the BARB reporting clock. Defaults to FALSE (standard 24 hour clock).
#' @param standardise_audiences whether to standardise impacts by spot time length. Options are the default of no standardisation (""), "using_duration" or "using_rate_factors".
#' @param metric Either "audience_size_hundreds" to return impacts, or "tvrs" to return TVR's
#' @param retry_on_initial_no_response If the API responds with no data for the first page of results, should baRb retry? Pagination will always automatically retry to avoid incomplete datasets.
#' @param fail_on_unsuccessful_pagination If the API has still not responded with data for a results page after all retries, should the function fail? FALSE will generate a warning but return results anyway.
#' @param pause_before_retry Time in seconds to pause before retrying. Helps to avoid a quick succession of consecutive failed queries that can trigger rate limiting.
#' @param retries Number of times to retry a page request that has responded with no data
#'
#' @return A tibble of TV spots
#' @export
#'
#' @examples
#' barb_get_spots(min_transmission_date = "2023-01-01", max_transmission_date = "2023-01-31", advertiser_name = "hays_travel", metric = "audience_size_hundreds")
barb_get_spots <- function(min_transmission_date = NULL,
                           max_transmission_date = NULL,
                           advertiser_name = NULL,
                           consolidated = TRUE,
                           use_reporting_days = FALSE,
                           standardise_audiences = "",
                           metric = "audience_size_hundreds",
                           retry_on_initial_no_response = FALSE,
                           fail_on_unsuccessful_pagination = FALSE,
                           retries = 5,
                           pause_before_retry = 90){

  success <- FALSE
  i <- 0

  message(glue::glue("Running {advertiser_name} from {min_transmission_date} to {max_transmission_date}..."))

  while(!success & i < retries){

    api_result <- barb_query_api(
      barb_url_spots(),
      list(
        "min_transmission_date" = min_transmission_date,
        "max_transmission_date" = max_transmission_date,
        "advertiser_name" = advertiser_name,
        "limit" = "5000",
        "consolidated" = consolidated,
        "use_reporting_days" = use_reporting_days,
        "standardise_audiences" = standardise_audiences
      )
    )

    if(length(api_result$json$events)>0 | !retry_on_initial_no_response){
      success <- TRUE
    } else {
      message("BARB API responded with no data on first contact. Trying again.")
      Sys.sleep(pause_before_retry)
    }

    i <- i+1
  }

  if(length(api_result$json$events)==0){
    if(!retry_on_initial_no_response){
      warning("No spots returned OR THE API FAILED TO RESPOND PROPERLY. Try setting `retry_on_initial_no_response = TRUE` to make sure.")
    } else {
      message("No spots returned by the API for the selected dates")
    }
    return(NULL)
  }

  spots <- process_spot_json(api_result, metric = metric)

  #Paginate if necessary
  while(!is.null(api_result$next_url)){
    message("Paginating")

    retry_url <- api_result$next_url

    api_result <- barb_query_api(api_result$next_url)

    # if(length(api_result$json$events)!=5000) browser()
    if(is.null(api_result$json$events) | length(api_result$json$events)==0){  #Needed in case a page contains no data. Queried with BARB why this happens.

      i <- 0

      # try again
      while((is.null(api_result$json$events) | length(api_result$json$events)==0) & i < retries){

        message("BARB API responded with no data while paginating. Trying again.")
        Sys.sleep(pause_before_retry)

        api_result <- barb_query_api(retry_url)
        i <- i+1
      }

      # Fail if unsuccessful
      if(is.null(api_result$json$events) | length(api_result$json$events)==0){  #Needed in case a page contains no data. Queried with BARB why this happens.
        if(fail_on_unsuccessful_pagination){
          stop(glue::glue("BARB API returned no data from {api_result$next_url}. Data request failed."))
        } else {
          warning("Pagination returned no results after retries but fail_on_unsuccessful_pagination = FALSE so returning results anyway.")
        }

      }
    }

      api_page <- process_spot_json(api_result, metric = metric)

      # API pages sometimes return fewer audiences than initial calls. Add a col of NA's when this happens.
      if(ncol(api_page) < ncol(spots)){
        api_page[, names(spots)[!names(spots) %in% names(api_page)]] <- NA
      }

      if(nrow(api_page) > 0){ #needed in case of failed pagination
        spots <- spots %>%
          dplyr::union_all(api_page)
      }

      message(glue::glue("Total spots: {nrow(spots)}"))
  }

  spots_macro_true <- spots |>
    dplyr::filter(is_macro_region==TRUE)

  spots_macro_false <- spots |>
    dplyr::filter(is_macro_region==FALSE)

  message(glue::glue("Removing duplicated spots..."))

  # Remove all universes except Online Multiple Screens Network for spots that have multiple universes
  spots_all <- spots_macro_false |>
    dplyr::mutate(panel_region_online_multi = ifelse(panel_region=="Online Multiple Screen Network",1,0)) |>
    dplyr::group_by(station_name, standard_datetime) |>
    dplyr::mutate(duplicate = dplyr::n() > 1) |>
    dplyr::ungroup() |>
    dplyr::filter(!duplicate | panel_region=="Online Multiple Screen Network") |>
    dplyr::select(-panel_region_online_multi, -duplicate) |>
    dplyr::union_all(spots_macro_true)

  spots_all

}

process_spot_json <- function(spot_json, metric = "audience_size_hundreds"){

  #Extract spot list from json
  spots_parsed <- spot_json$json$events %>%
    tidyjson::as_tbl_json() %>%
    tidyjson::spread_values(panel_region = tidyjson::jstring('panel', 'panel_region')) %>%
    tidyjson::spread_values(is_macro_region = tidyjson::jlogical('panel', 'is_macro_region'))  %>%
    tidyjson::spread_values(station_name = tidyjson::jstring('station', 'station_name')) %>%
    tidyjson::spread_values(sales_house_name = tidyjson::jstring('sales_house', 'sales_house_name')) %>%
    tidyjson::spread_values(standard_datetime = tidyjson::jstring('spot_start_datetime', 'standard_datetime')) %>%
    tidyjson::spread_values(clearcast_commercial_title = tidyjson::jstring('clearcast_information', 'clearcast_commercial_title')) %>%
    tidyjson::spread_values(preceding_programme_name = tidyjson::jstring('preceding_programme_name')) %>%
    tidyjson::spread_values(spot_duration = tidyjson::jinteger('spot_duration')) %>%
    tidyjson::spread_values(break_type = tidyjson::jstring('break_type')) %>%
    tidyjson::spread_values(commercial_number = tidyjson::jstring('commercial_number')) %>%
    tidyjson::spread_values(advertiser_name = tidyjson::jstring('clearcast_information', 'advertiser_name')) %>%
    tidyjson::spread_values(product_name = tidyjson::jstring('clearcast_information', 'product_name')) %>%
    tidyjson::spread_values(clearcast_web_address = tidyjson::jstring('clearcast_information', 'clearcast_web_address'))

  #Get audience data for non-zero spots
  audiences_parsed <- spots_parsed %>%
    tidyjson::enter_object('audience_views') %>%
    tidyjson::gather_array() %>%
    tidyjson::spread_values(audience_code = tidyjson::jstring('audience_code')) %>%
    tidyjson::spread_values(audience_description = tidyjson::jstring('description')) %>%
    tidyjson::spread_values(audience_size_hundreds = tidyjson::jdouble('audience_size_hundreds')) %>%
    tidyjson::spread_values(universe_size_hundreds = tidyjson::jdouble('target_size_in_hundreds')) %>%
    tibble::as_tibble() |>
    dplyr::mutate(tvrs = audience_size_hundreds / universe_size_hundreds * 100)

  #If all spots were zero rated, return result
  if(nrow(audiences_parsed)==0){
    spots_parsed <- spots_parsed %>%
      dplyr::select(
        panel_region,
        station_name,
        clearcast_commercial_title,
        standard_datetime
      )
    return(spots_parsed)
  }

  #Pivot audiences to columns and append zero rated spots again
  spots_audiences <- audiences_parsed %>%
    dplyr::mutate(kpi_var = !!rlang::sym(metric)) %>%
    dplyr::select(document.id,
                  panel_region,
                  is_macro_region,
                  station_name,
                  sales_house_name,
                  clearcast_commercial_title,
                  preceding_programme_name,
                  spot_duration,
                  break_type,
                  commercial_number,
                  advertiser_name,
                  product_name,
                  clearcast_web_address,
                  standard_datetime,
                  audience_description,
                  kpi_var) %>%
    tidyr::pivot_wider(names_from = audience_description, values_from = kpi_var)

  spots_parsed_wider <- spots_parsed

  spots_parsed_wider[setdiff(names(spots_audiences), names(spots_parsed))] <- NA
  spots_parsed_wider <- tibble::as_tibble(spots_parsed_wider)

  spots_all <- spots_audiences %>%
    dplyr::union_all(
      dplyr::filter(spots_parsed_wider, !document.id %in% spots_audiences$document.id)
    ) %>%
    janitor::clean_names()

  spots_all[is.na(spots_all) & is.numeric(spots_all)] <- 0
  spots_all[is.na(spots_all) & is.character(spots_all)] <- ""

  spots_all
}


#' Roll up raw spot data by broadcast time for spot to web modelling
#'
#' @param spots A raw spot file
#' @param plus_one Roll up +1 channels? (T/F)
#' @param hd Roll up HD channels? (T/F)
#'
#' @return A tibble of rolled up spots
#' @export
#'
#' @examples
barb_rollup_spots <- function(spots, plus_one = TRUE, hd = TRUE){
  spots_rollup <- spots %>%
    dplyr::mutate(date = as.Date(standard_datetime)) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~replace(., is.na(.), 0)))

  spots_rollup <- spots_rollup %>%
    dplyr::mutate(parent_station_name = station_name)

  if(plus_one){
    spots_rollup <- spots_rollup %>%
      dplyr::mutate(parent_station_name = ifelse(parent_station_name=="C4+1", "Channel 4", parent_station_name)) %>%
      dplyr::mutate(parent_station_name = ifelse(parent_station_name=="Dave ja vu", "Dave", parent_station_name)) %>%
      dplyr::mutate(parent_station_name = stringr::str_trim(stringr::str_replace(parent_station_name, "\\+1$", "")))
  }

  if(hd){
    spots_rollup <- spots_rollup %>%
      dplyr::mutate(parent_station_name = stringr::str_trim(stringr::str_replace(parent_station_name, "HD$", "")))
  }

  spots_rollup %>%
    dplyr::group_by(
             parent_station_name,
             sales_house_name,
             clearcast_commercial_title,
             preceding_programme_name,
             spot_duration,
             commercial_number,
             advertiser_name,
             product_name,
             clearcast_web_address,
             standard_datetime,
             date) %>%
    dplyr::summarise(impacts = sum(all_adults, na.rm = TRUE)) %>%
    dplyr::ungroup()
}
