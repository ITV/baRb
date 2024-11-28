#' Get a tibble of panel names
#'
#' @return A tibble of available panels
#' @export
#'
#' @examples
#' barb_get_panels()
barb_get_panels <- function(){

  api_result <- barb_query_api(
    barb_url_meta_panels()
  )

  if(is.null(api_result$json)) return(NULL)

  result <- api_result$json %>%
    tidyjson::as_tbl_json() %>%
    tidyjson::spread_values(panel_code = tidyjson::jinteger('panel_code')) %>%
    tidyjson::spread_values(panel_region = tidyjson::jstring('panel_region')) %>%
    tidyjson::spread_values(is_macro_region = tidyjson::jlogical('is_macro_region')) %>%
    dplyr::select(panel_code, panel_region, is_macro_region) %>%
    tibble::as_tibble()

  result
}

#' Get a tibble of station names
#'
#' @return A tibble of available stations
#' @export
#'
#' @examples
#' barb_get_stations()
barb_get_stations <- function(){

  api_result <- barb_query_api(
    barb_url_meta_stations()
  )

  if(is.null(api_result$json)) return(NULL)

  result <- api_result$json %>%
    tidyjson::as_tbl_json() %>%
    tidyjson::spread_values(station_code = tidyjson::jinteger('station_code')) %>%
    tidyjson::spread_values(station_name = tidyjson::jstring('station_name')) %>%
    dplyr::select(station_code, station_name) %>%
    tibble::as_tibble()

  result
}
