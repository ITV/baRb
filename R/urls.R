barb_url_root <- function(){
  "https://barb-api.co.uk/api/v1"
}

barb_url_token <- function(){
  glue::glue("{barb_url_root()}/auth/token/")
}

barb_url_spots <- function(){
  glue::glue("{barb_url_root()}/advertising_spots")
}

barb_url_programmes <- function(){
  glue::glue("{barb_url_root()}/programme_ratings")
}

barb_url_meta_panels <- function(){
  glue::glue("{barb_url_root()}/panels")
}

barb_url_meta_stations <- function(){
  glue::glue("{barb_url_root()}/stations")
}

barb_url_advertisers <- function(){
  glue::glue("{barb_url_root()}/advertisers")
}

barb_login <- function(username = NULL, password = NULL) {
  if(is.null(username)) username <- Sys.getenv('BARB_API_USERNAME')
  if(is.null(password)) password <- Sys.getenv('BARB_API_PASSWORD')

  if(username=="" | password==""){
    stop("Username or password not set")
  }

  creds <- jsonlite::toJSON(list(email = username, password = password), auto_unbox = TRUE)

  token <- httr::POST(
    url = barb_url_token(),
    body = creds,
    httr::content_type("application/json")
  )

  token
}

#' Submit a query to the BARB API (usually not called directly)
#'
#' @param url Endpoint URL
#' @param query List of query parameters
#'
#' @return API results json
#' @export
#'
#' @examples
#' # Example only. You'd use barb_get_spots() for this instead.
#' raw_json <- barb_query_api(
#'   "https://dev.barb-api.co.uk/api/v1/advertising_spots",
#'   list(
#'     min_transmission_date = "2022-01-01",
#'     max_transmission_date = "2022-12-31",
#'     advertiser_name = "PLAYMOBIL UK")
#' )
barb_query_api <- function(url, query = list()){

  token = barb_login()

  # If no queries have been passed then this is a pagination so run without the 'query' option
  # You can't pass query = list() because httr will URL encode the pagination URL, which the API doesn't like
  if(length(query) > 0){
    response <- httr::GET(url = url,
                          httr::add_headers(Authorization = paste0('Bearer ',
                                                                   httr::content(token, as = "parsed")$access)),
                          query = query)
  } else {
    response <- httr::GET(url = url,
                          httr::add_headers(Authorization = paste0('Bearer ',
                                                                   httr::content(token, as = "parsed")$access)))
  }

  # Writes the raw return from httr::GET() to a working directory tempfile for debugging
  # readr::write_rds(response, tempfile(tmpdir = getwd(), fileext = ".rds"))

  raw_json <- response %>%
    httr::content()

    # Get URL for the next page of results (if there is one)
    next_url <- response$all_headers[[length(response$all_headers)]][["headers"]][["x-next"]]

  list(json = raw_json, next_url = next_url)
}
