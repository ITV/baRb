barb_url_root <- function(){
  "https://barb-api.co.uk/api/v1"
}

barb_url_token <- function(){
  glue::glue("{barb_url_root()}/auth/token/")
}

barb_url_spots <- function(async = FALSE){
  if(!async){
    return(glue::glue("{barb_url_root()}/advertising_spots"))
  } else {
    return(glue::glue("{barb_url_root()}/async-batch/advertising_spots/"))
  }
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

barb_url_async_results <- function(job_id){
  glue::glue("{barb_url_root()}/async-batch/results/{job_id}")
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

  auth_header <- paste0('Bearer ', httr::content(token, as = "parsed")$access)

  auth_header
}

#' Submit a query to the BARB API (usually not called directly)
#'
#' @param url Endpoint URL
#' @param query List of query parameters
#'
#' @return API results json if async = FALSE or download URLs if async = TRUE
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
barb_query_api <- function(url, query = list(), async = FALSE, async_check_interval = 20){

  token = barb_login()

  if(!async){

    # If no queries have been passed then this is a pagination so run without the 'query' option
    # You can't pass query = list() because httr will URL encode the pagination URL, which the API doesn't like
    if(length(query) > 0){
      response <- httr::GET(url = url,
                            httr::add_headers(Authorization = token),
                            query = query)
    } else {
      response <- httr::GET(url = url,
                            httr::add_headers(Authorization = token))
    }

    # Writes the raw return from httr::GET() to a working directory tempfile for debugging
    # readr::write_rds(response, tempfile(tmpdir = getwd(), fileext = ".rds"))

    raw_json <- response %>%
      httr::content()

      # Get URL for the next page of results (if there is one)
      next_url <- response$all_headers[[length(response$all_headers)]][["headers"]][["x-next"]]

    return(list(json = raw_json, next_url = next_url))
  } else {

    #Drop nulls from list (needed to remove `standardise_audiences` when not in use)
    query[sapply(query, is.null)] <- NULL

    query_body <- jsonlite::toJSON(append(query, list("output_format" = "parquet")), auto_unbox = TRUE)

    response <- httr::POST(url = url,
                          httr::add_headers(Authorization = token,
                                            `Content-Type` = "application/json"),
                          body = query_body,
                          encode = "raw")

    message(glue::glue("Async job started with job_id = {httr::content(response)$job_id}"))

    async_status <- "init"

    while(!async_status=="successful"){
      Sys.sleep(async_check_interval)

      async_result <- httr::GET(url = barb_url_async_results(httr::content(response)$job_id),
                            httr::add_headers(Authorization = token))

      async_status <- httr::content(async_result)$status

      message(glue::glue("Server status: {async_status}"))

      if(!async_status %in% c("started", "successful")){
        stop(glue::glue("Unknown server status: {async_status}. Abandoning query."))
      }

    }

    output <- tibble::tibble(
      result = httr::content(async_result)$result
    ) |>
      dplyr::mutate(
        type = purrr::map(result, ~ .["type"]),
        data = purrr::map(result, ~ .["data"])
      ) |>
      dplyr::mutate(data = unlist(data)) |>
    dplyr::mutate(
      downloaded_data =
        purrr::map(
          .x = data,
          .f = ~ download_and_load_parquet(.x)
        )
    ) |>
    dplyr::select(downloaded_data) |>
    tidyr::unnest(downloaded_data)

    return(async_to_sync_format(output))
  }
}

# Downloads parquet files as tempfiles and then returns them as a dataframe
download_and_load_parquet <- function(url){

  filename <- tempfile(fileext = ".parquet")

  download.file(url, filename, mode = "wb")

  arrow::read_parquet(filename)

}

# Converts async files to match the list format returned by sync requests
async_to_sync_format <- function(async_data){

  empty_output <-
    list(json = NULL
         ,
         next_url = NULL)

  processed <- async_data |>
    dplyr::select(-TRANSMISSION_DATE) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(
      is.character(.),
      ifelse(substr(., 1, 1)%in% c("{", "["), list(jsonlite::fromJSON(.)), .),
      .
    ))) |>
    dplyr::rename_all(tolower) |>
    dplyr::ungroup() |>
    tidyr::nest(events = -id) |>
    dplyr::mutate(events = purrr::map(events, .f = ~as.list(.))) |>
    as.list()

  empty_output$json$events <- unpack_sublists(processed$events)

  empty_output
}

# used by async_to_sync_format() to unpack awkward nesting
unpack_sublists <- function(events_list){
  for (i in seq_along(events_list)) {
    # Get all variable names in the current event
    variables <- names(events_list[[i]])

    for (var in variables) {
      # Check if the variable is a list and contains a list as the first element
      if (is.list(events_list[[i]][[var]][[1]])) {
        replacement_data <- NULL
        type_switch=NULL
        if(!length(events_list[[i]][[var]][[1]][[1]])>1){ #check it's not a table like audience_views
          type_switch="simple"
          # Parse as a simple list
          subvariables <- names(events_list[[i]][[var]][[1]])
          for (subvar in subvariables){
            events_list[[i]][[var]][[subvar]] <- events_list[[i]][[var]][[1]][[subvar]]
          }
          events_list[[i]][[var]][[1]] <- NULL
        } else {
          # Parse as a table
          type_switch="table"
          replacement_data <- list()
          for (n in 1:length(events_list[[i]][[var]][[1]][[1]])){
            temp_sub_list <- list()
            for (subvar in names(events_list[[i]][[var]][[1]])){
              temp_sub_list[[subvar]] <- events_list[[i]][[var]][[1]][[subvar]][[n]]
            }
            replacement_data[[n]] <- temp_sub_list
          }
        }
        if (type_switch!="simple"){
          events_list[[i]][[var]] <- replacement_data
        }
      }
    }
  }
  events_list
}
