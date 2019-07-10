#' Extract Global Market Cap of Cryptocurrency Market
#'
#' @param currency currency code - Default is 'USD'
#' @param apikey A valid API-key from Coinmarketcap
#' @param latest If `TRUE` (default), only the latest data is retrieved,
#' otherwise historical data is returned. (NOTE: Historic Data require higher API rights)
#' @param ... Further arguments can be passed to historical data. Further information
#' can be found in the \href{https://coinmarketcap.com/api/documentation/v1/#operation/getV1GlobalmetricsQuotesHistorical}{API documentation}
#'
#' @return A dataframe with global market cap of Cryptocurrencies
#' @examples \dontrun{
#' get_global_marketcap('AUD')
#' get_global_marketcap('EUR', apikey)
#' get_global_marketcap(apikey = apikey, latest = FALSE,
#'                      time_start = Sys.Date()-180, time_end = Sys.Date(),
#'                      count = 10, interval = "yearly")
#'
#' }
#' @export
get_global_marketcap <- function(currency = 'USD', apikey = NULL,
                                 latest = TRUE, ...) {
        ## Check Inputs ##########
        stopifnot(currency %in% get_valid_currencies())
        ## Using old API ? ############
        if (is.null(apikey)) {
                warning("The old API is used when no 'apikey' is given.")
                d <- data.frame(fromJSON(rawToChar(curl_fetch_memory(
                        paste0('https://api.coinmarketcap.com/v1/global/?convert=',currency))$content)))
                d$last_updated  <- as.POSIXct(as.numeric(d$last_updated), origin = as.Date("1970-01-01"))
                return(d)
        }

        ## Build Request (new API) ##########
        base_url <- "pro-api.coinmarketcap.com"
        if (latest) {
                what <- paste0("global-metrics/quotes/latest?convert=", currency)
        } else {
                what <- paste0("global-metrics/quotes/historical?convert=", currency)
                whatelse <- list(...)
                whatelse <- transform_args(whatelse)
                what <- paste0(what,"&", whatelse)
        }
        apiurl <- paste0("https://",base_url,"/v1/",what);
        ## Make Request ##########
        req <- make_request(apiurl, apikey)

        ## Check Response ##########
        check_response(req)

        ## Modify Result ##########
        modify_result(req$content)
}
