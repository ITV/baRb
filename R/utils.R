#' convenience function for setting environment variables
#'
#' @param value of the environment variable
#' @param name of the environment variable
#' @keywords internal
set_env_var = function(value, name) {
  setter = list(value)
  names(setter) = name
  do.call(Sys.setenv, setter)
}
