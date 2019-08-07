#' Setup
#'
#' Specifies API Key and the base URL for session
#'
#' @param api_key Your Coinmarketcap API key.
#' @param sandbox Sets the base URL for the API. If set to TRUE, the sandbox-API
#'   is called. The default is FALSE.
#'
#' @examples
#' setup("xXXXXxxxXXXxx")
#' get_setup()
#'
#' @export
#' @name setup
setup <- function(api_key = NULL, sandbox = FALSE) {
  if (!is.null(api_key)) {
    Sys.setenv("COINMARKETCAP_APIKEY" = api_key)
  }
  url <- ifelse (sandbox,
                 "sandbox-api.coinmarketcap.com",
                 "pro-api.coinmarketcap.com")
  options("COINMARKETCAP_URL" = url)
}

#' @rdname setup
#' @export
get_setup <- function(){
  key <- Sys.getenv("COINMARKETCAP_APIKEY")
  url <- getOption("COINMARKETCAP_URL")

  .prt <- function(val, what){
    cat(crayon::green(cli::symbol$tick),
        sprintf("%s is set up", what), "\n")
  }

  l <- list(
    api_key = key,
    url = url
  )
  names <- c("API-KEY", "Base-URL")
  lapply(1:length(l), function(x) .prt(l[[x]], names[[x]]))

  invisible(l)
}

#' @rdname setup
#' @export
reset_setup <- function(api_key = TRUE, sandbox = TRUE){
  .prt <- function(what){
    cat(crayon::green(cli::symbol$tick),
        sprintf("%s sucessfully reset", what),"\n")
  }

  if (isTRUE(api_key)) {
    Sys.unsetenv("COINMARKETCAP_APIKEY")
    .prt("API Key")
  }
  if (isTRUE(sandbox)) {
    options("COINMARKETCAP_URL" = NULL)
    .prt("Base URL")
  }
}

.get_api_key <- function(){
  Sys.getenv("COINMARKETCAP_APIKEY")
}
.get_baseurl <- function(){
  getOption("COINMARKETCAP_URL")
}
