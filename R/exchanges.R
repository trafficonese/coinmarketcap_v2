#' Returns a paginated list of all cryptocurrency exchanges by CoinMarketCap ID.
#' We recommend using this convenience endpoint to lookup and utilize our unique
#' exchange id across all endpoints as typical exchange identifiers may change
#' over time. As a convenience you may pass a comma-separated list of exchanges
#' by slug to filter this list to only those you require.
#' @title Get all cryptocurrency exchanges
#' @references \href{https://coinmarketcap.com/api/documentation/v1/#operation/getV1ExchangeMap}{API documentation}
#' @inheritParams get_crypto_map
#' @return A dataframe with exchange values
#' @examples \dontrun{
#' get_exchange_map(apikey)
#' get_exchange_map(apikey, listing_status = "inactive",
#'                  slug = "binance", start = 5, limit = 100)
#' }
#' @export
get_exchange_map <- function(apikey, test=FALSE, ...) {
        ## Input Check ##########
        if (is.null(apikey)) stop("A valid API key is needed for this request.")

        ## Build Request (new API) ##########
        base_url <- ifelse(test, test_url, pro_url)
        what <- "exchange/map"
        whatelse <- list(...)
        whatelse <- transform_args(whatelse)
        if (!is.null(whatelse)) {
                what <- paste0(what,"?", whatelse)
        }

        apiurl <- paste0("https://",base_url,"/v1/",what)
        ## Make Request ##########
        req <- make_request(apiurl, apikey)

        ## Check Response ##########
        check_response(req)

        ## Modify Result ##########
        modify_result(req$content, TRUE)
}

#' Returns all static metadata for one or more exchanges.
#' This information includes details like launch date, logo,
#' official website URL, social links, and market fee documentation URL.
#' @title Get all cryptocurrency exchanges metadata
#' @references \href{https://coinmarketcap.com/api/documentation/v1/#operation/getV1ExchangeInfo}{API documentation}
#' @inheritParams get_crypto_map
#' @return A dataframe with exchange metadata values
#' @examples \dontrun{
#' get_exchange_meta(apikey, id = 5)
#' get_exchange_meta(apikey, slug = c("binance", "gdax"))
#' }
#' @export
get_exchange_meta <- function(apikey, id=NULL, slug=NULL, test=FALSE) {
        ## Input Check ##########
        if (is.null(apikey)) stop("A valid API key is needed for this request.")

        ## Build Request (new API) ##########
        base_url <- ifelse(test, test_url, pro_url)
        what <- "exchange/info"
        args <- c(is.null(id),is.null(slug))
        if (sum(args) != 1) {
                stop("You must use either 'id' or 'slug'")
        }
        if (!is.null(id)) what <- paste0(what, "?id=", paste(id, collapse=","))
        if (!is.null(slug)) what <- paste0(what, "?slug=", paste(slug, collapse=","))

        apiurl <- paste0("https://",base_url,"/v1/",what)

        ## Make Request ##########
        req <- make_request(apiurl, apikey)

        ## Check Response ##########
        check_response(req)

        ## Modify Result ##########
        modify_result(req$content, TRUE)
}
