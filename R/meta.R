#' Get the list of panels
#'
#' Returns a complete list of panel codes and panel names.
#' This is useful for finding the correct panel code when using panel
#' code as a query parameter for other endpoints.
#'
#' @param as_tibble if TRUE cast result as a tibble object, otherwise list
#'   representation of the raw json response
#' @inheritParams barb
#' @include rexport-tibble.R
#' @export
#' @examples \dontrun{
#'   barb_get_panels()
#' }
barb_get_panels = function(as_tibble = TRUE, token = NA_character_) {
  res = barb("/panels", token = token) %>%
    with_class("barb_panels")
  if (as_tibble) {
    res = as_tibble(res)
  }
  res
}

#' @s3method as_tibble barb_panels
#' @rdname as_tibble
#' @export
as_tibble.barb_panels = function(
    x, ..., .rows = NULL,
    .name_repair = c("check_unique", "unique", "universal", "minimal"),
    rownames = pkgconfig::get_config("tibble::rownames", NULL)
) {
  x %>%
    tidyjson::as_tbl_json() %>%
    tidyjson::spread_values(panel_code = tidyjson::jstring('panel_code')) %>%
    tidyjson::spread_values(panel_region = tidyjson::jstring('panel_region')) %>%
    tidyjson::spread_values(is_macro_region = tidyjson::jstring('is_macro_region')) %>%
    dplyr::select(panel_code, panel_region, is_macro_region) %>%
    tibble::as_tibble()
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
    tidyjson::spread_values(station_code = tidyjson::jstring('station_code')) %>%
    tidyjson::spread_values(station_name = tidyjson::jstring('station_name')) %>%
    dplyr::select(station_code, station_name) %>%
    tibble::as_tibble()

  result
}
