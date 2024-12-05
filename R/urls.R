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

barb_url_programmes <- function(async = FALSE){
  if(!async){
    return(glue::glue("{barb_url_root()}/programme_ratings"))
  } else {
    return(glue::glue("{barb_url_root()}/async-batch/programme_ratings/"))
  }
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

#' Submit a query to the BARB API. Not called directly by users.
#'
#' @param url Endpoint URL
#' @param query List of query parameters
#'
#' @return API results json if async = FALSE or download URLs if async = TRUE
#'
#' @examples
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

    if(response$status_code==504){
      stop("Request timed out. If you're using the sync API, switch to async=TRUE instead. See the readme at https://github.com/ITV/baRb for more details.")
    }

    if(response$status_code!=200){
      stop(glue::glue("Server responded with status {response$status_code}. Aborting query."))
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


#' Manages sync and async queries with retries for timeout problems on sync queries.
#' Called by barb_get_spots() and barb_get_programmes(). Not called directly by users.
#'
#' @param query_url An API url e.g. barb_url_programmes()
#' @param query_params A list of query parameters
#' @param metric Either "audience_size_hundreds" to return impacts, or "tvrs" to return TVR's
#' @param retry_on_initial_no_response If the API responds with no data for the first page of results, should baRb retry? Pagination will always automatically retry to avoid incomplete datasets.
#' @param fail_on_unsuccessful_pagination If the API has still not responded with data for a results page after all retries, should the function fail? FALSE will generate a warning but return results anyway.
#' @param pause_before_retry Time in seconds to pause before retrying. Helps to avoid a quick succession of consecutive failed queries that can trigger rate limiting.
#' @param retries Number of times to retry a page request that has responded with no data
#' @param async should the async API be used?
#' @param json_processor A json processing function e.g. process_spot_json. Pass this without brackets().
#'
#' @return
#'
#' @examples
barb_manage_query <- function(query_url = NULL,
                              query_params = list(),
                              metric = "audience_size_hundreds",
                              retry_on_initial_no_response = FALSE,
                              fail_on_unsuccessful_pagination = FALSE,
                              retries = 5,
                              pause_before_retry = 90,
                              async = FALSE,
                              json_processor = NULL) {

  success <- FALSE
  i <- 0

  if (!async) {
    message("Using sync api")
      api_result <- barb_query_api(
        query_url,
        query_params,
        async = async
      )
  } else {
    message("Using async api")
    api_result <- barb_query_api(
      query_url,
      query_params,
      async = async
    )
  }

  if (length(api_result$json$events) == 0) {
    message("No spots returned for the selected dates")
    return(NULL)
  }

  initial_output <- json_processor(api_result, metric = metric)

  #Paginate if necessary
  while(!is.null(api_result$next_url)){
    message("Paginating")

    retry_url <- api_result$next_url

    api_result <- barb_query_api(api_result$next_url)

    api_page <- json_processor(api_result, metric = metric)

    # API pages sometimes return fewer audiences than initial calls. Add a col of NA's when this happens.
    if(ncol(api_page) < ncol(initial_output)){
      api_page[, names(initial_output)[!names(initial_output) %in% names(api_page)]] <- NA
    }

    if(nrow(api_page) > 0){ #needed in case of failed pagination
      initial_output <- initial_output %>%
        dplyr::union_all(api_page)
    }

    message(glue::glue("Total items: {nrow(initial_output)}"))
  }

  initial_output

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
