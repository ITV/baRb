# by storing this in a hidden variable if you ever decide to change it
# you don't have to change it everywhere
.barb_token_env_var = "BARB_TOKEN"
.barb_user_env_var = "BARB_API_USERNAME"
.barb_password_env_var = "BARB_API_PASSWORD"

.root = "https://barb-api.co.uk/api/v1"
.token = glue::glue("{.root}/auth/token/")

#' Set the user environment variable
#'
#' Sets the default barb user for a session.
#'
#' @param user character(1) barb api username
#' @export
set_user = function(user) {
  set_env_var(user, .barb_user_env_var)
}

#' Set the password environment variable
#'
#' Sets the default barb password for a session.
#'
#' @param password character(1) barb api password
#' @export
set_password = function(password) {
  set_env_var(password, .barb_password_env_var)
}

#' Set the default token
#'
#' Sets the default barb token for the session
#'
#' @param token character(1) barb api token see [baRb::barb_access_token()]
#' @seealso barb_access_token
#' @export
set_token = function(token) {
  set_env_var(token, .barb_token_env_var)
}

#' Accessor for barb token
#' 
#' Retrieve the current token.
#' 
#' If a token is passed, that will take priority. If no token argument
#' is provided, attempt to retrieve the default token.
#' 
#' Throws an error if no token can be found.
#' 
#' @param token a barb API token
#' @return character(1) a barb API token
#' @keywords internal
get_token = function(token = NA_character_) {
  if (!is.na(token)) return (token)
  token = Sys.getenv(.barb_token_env_var, token)
  if (is.na(token)) stop("No barb api token found.")
  token
}

#' Get access token for barb API
#' 
#' Request an access token from the API using your username and password.
#' 
#' @param username character(1) your barb username.
#' @param password character(1) your barb password.
#' @return character(1) your barb API token
#' @export
barb_get_access_token = function(
  username = Sys.getenv(.barb_user_env_var, NA),
  password = Sys.getenv(.barb_password_env_var, NA)
) {
  if (any(is.na(c(username, password)))) {
    stop("please provide username and password or set environment variables BARB_API_USERNAME and BARB_API_PASSWORD")
  }
  res = httr::POST(
    url = .token,
    body = jsonlite::toJSON(list(email = username, password = password),auto_unbox = TRUE),
    httr::content_type_json()
  )
  token = httr::content(res, as = "parsed")$access
  cli::cli_alert_info(
    "Set the token for this R session with {.code baRb::set_token(\"{token}\")}
    
    Note that this token expires 12 hours after it's creation.
    "
  )
  token
}
