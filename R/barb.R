#' convenience function to generate the auth header
#' 
#' Creates the request header for the authorization to the API
#' from a token.
#' 
#' @param token character(1) a barb API token
#' @return httr::request object
#' @keywords internal
barb_auth_header = function(token) {
  httr::add_headers(
    Authorization = glue::glue("Bearer {token}")
  )
}

#' Get next url from the response
#' 
#' @param response httr::response object
#' @return character(1) a url for the next set of results
#' @keywords internal
barb_get_next = function(response) {
  response$all_headers[[length(response$all_headers)]][["headers"]][["x-next"]]
}

#' Does the response indicate furhter pages
#'
#' @inheritParams barb_get_next
#' @return logical(1) TRUE if there is a URL indicating next page
#' @keywords internal
barb_has_next = function(response) {
  !is.null(barb_get_next(response))
}

#' Call the followup request to get the next page of results
#' 
#' @param response httr::response object
#' @param token barb API token
#' @keywords internal
barb_next = function(response, token) {
  httr::GET(
    barb_get_next(response),
    barb_auth_header(get_token(token))
  )
}

#' Make a request to the barb API
#'
#' Send a request to the barb API. This is a lightweight general purpose
#' request generating function for requests to the barb API and can be used as
#' a workhorse to query any of the API endpoints.
#'
#' This function will return a list of the raw response json, iterating through pages if necessary.
#'
#' @examples
#' \dontrun{
#'  barb(
#'    "/advertising_spots",
#'    min_transmission_date = "2023-01-01",
#'    max_transmission_date = "2023-01-31",
#'    advertiser_name = "hays_travel"
#'  )
#'
#'  barb("/panels")
#' }
#' @param path route to the request endpoint. Is appended to the API base URL.
#' @param ... arguments used to build query parameters
#' @param token character(1) barb API token. if NA try to find the session default token
#' @return list from raw response json
#' @export
barb = function(path, ..., token = NA_character_) {
  token = get_token(token)
  query = list(...)
  response = httr::GET(
    url = glue::glue("{.root}{path}"),
    barb_auth_header(token),
    query = query
  )
  res = httr::content(response)
  counter = 1
  # pagination
  while (barb_has_next(response)) {
    counter = counter + 1
    cli::cli_alert_info("Downloading page {counter} of results")
    response = barb_next(response)
    res = c(res, httr::content(response))
  }
  res
}
