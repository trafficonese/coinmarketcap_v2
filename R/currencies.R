#' Get all active cryptocurrencies supported by the
#' platform including a unique id
#' @title Get active cryptocurrencies
#' @param apikey A valid API-key from Coinmarketcap
#' @param test If `TRUE`, the requests are done in a testing sandbox environment.
#' This requires an extra testing API key, but allows possible all requests.
#' @param ... Further arguments passed to the request. Further information
#' can be found in the \href{https://coinmarketcap.com/api/documentation/v1/#operation}{API documentation}
#' @references \href{https://coinmarketcap.com/api/documentation/v1/#operation/getV1CryptocurrencyMap}{API documentation}
#' @return A dataframe with all active cryptocurrencies supported by the
#' platform including a unique id for each cryptocurrency.
#' @examples \dontrun{
#' get_crypto_map(apikey)
#' get_crypto_map(apikey, symbol="BTC")
#' get_crypto_map(apikey, symbol=c("BTC","ETH"))
#' get_crypto_map(apikey, listing_status = "active", start = 1, limit = 10)
#' get_crypto_map(apikey, listing_status = "inactive", start = 1, limit = 10)
#' }
#' @export
get_crypto_map <- function(apikey, test=FALSE, ...) {
        ## Input Check ##########
        if (is.null(apikey)) stop("A valid API key is needed for this request.")

        ## Build Request (new API) ##########
        base_url <- ifelse(test, test_url, pro_url)
        what <- "cryptocurrency/map"
        whatelse <- list(...)
        if (length(whatelse) > 0) {
                whatelse <- transform_args(whatelse)
                if (!is.null(whatelse)) {
                        what <- paste0(what,"?", whatelse)
                }
        }
        apiurl <- paste0("https://",base_url,"/v1/",what)

        ## Make Request ##########
        req <- make_request(apiurl, apikey)

        ## Check Response ##########
        check_response(req)

        ## Modify Result ##########
        modify_result(req$content, filter = F)
}

#' Get all static metdata available for one or more cryptocurrencies
#' @title Get static metdata
#' @param apikey A valid API-key from Coinmarketcap
#' @param symbol One or more cryptocurrency symbols.
#' Example: c("BTC","ETH").
#' @param id Alternatively pass one or more CoinMarketCap cryptocurrency IDs.
#' Example: c(1,2)
#' @param slug Alternatively pass a vector of cryptocurrency slugs.
#' Example: c("bitcoin","ethereum")
#' @param test If `TRUE`, the requests are done in a testing sandbox environment.
#' This requires an extra testing API key, but allows possible all requests.
#' @references \href{https://coinmarketcap.com/api/documentation/v1/#operation/getV1CryptocurrencyInfo}{API documentation}
#' @note At least one "id" or "slug" or "symbol" is required for this request.
#' @return A dataframe with metadata of Cryptocurrencies
#' @examples \dontrun{
#' get_crypto_meta(apikey)
#' get_crypto_meta(apikey, symbol = c("BTC","ETH"))
#' get_crypto_meta(apikey, id = c(1,2,3,4))
#' get_crypto_meta(apikey, slug=c("bitcoin", "ethereum"))
#' }
#' @export
get_crypto_meta <- function(apikey, symbol=NULL, id=NULL, slug=NULL, test=FALSE) {
        ## Input Check ##########
        if (is.null(apikey)) stop("A valid API key is needed for this request.")

        ## Build Request (new API) ##########
        base_url <- ifelse(test, test_url, pro_url)
        what <- paste0("cryptocurrency/info?")

        args <- c(is.null(symbol),is.null(id),is.null(slug))
        if (sum(args) == 3) {
                symbol = "BTC"
        } else if (sum(args) != 2) {
                stop("You must use either 'symbol', 'id' or 'slug'")
        }
        if (!is.null(symbol)) what <- paste0(what, "symbol=", paste(symbol, collapse=","))
        if (!is.null(id)) what <- paste0(what, "id=", paste(id, collapse=","))
        if (!is.null(slug)) what <- paste0(what, "slug=", paste(slug, collapse=","))
        apiurl <- paste0("https://",base_url,"/v1/",what);

        ## Make Request ##########
        req <- make_request(apiurl, apikey)

        ## Check Response ##########
        check_response(req)

        ## Modify Result ##########
        modify_result(req$content, filter = TRUE)
}

#' Get a paginated list of all active cryptocurrencies with latest market data.
#' The default "market_cap" sort returns cryptocurrency in order of CoinMarketCap's
#' market cap rank (as outlined in our methodology) but you may configure this call
#' to order by another market ranking field.
#' Use the "convert" option to return market values in multiple fiat and
#' cryptocurrency conversions in the same call.
#' @title Get latest/historical market data
#' @references \href{https://coinmarketcap.com/api/documentation/v1/#operation/getV1CryptocurrencyListingsLatest}{API documentation}
#' @inheritParams get_global_marketcap
#' @return A dataframe of top Cryptocurrencies with current or historic market data
#' @examples \dontrun{
#' get_crypto_listings('EUR')
#' get_crypto_listings('GBP', apikey)
#' get_crypto_listings('GBP', apikey, latest=F, start=1,
#'                          date=Sys.Date()-20, limit=10, sort="price", sort_dir="asc")
#' }
#' @export
get_crypto_listings <- function(currency = 'USD', apikey = NULL,
                                latest = TRUE, test=FALSE, ...) {
        ## Input Check ##########
        stopifnot(currency %in% get_valid_currencies())

        ## Using old API ? ############
        if (is.null(apikey)) {
                warning("The old API is used when no 'apikey' is given.")
                d <- data.frame(fromJSON(rawToChar(curl_fetch_memory(
                        paste0('https://api.coinmarketcap.com/v1/ticker/?convert=', currency,
                               '&limit=0'))$content)))

                d[,4:15] <- apply(d[,4:15], 2, function(x) as.numeric(as.character(x)))
                d$last_updated  <- as.POSIXct(d$last_updated, origin = as.Date("1970-01-01"))
                return(d)
        }


        ## Build Request (new API) ##########
        base_url <- ifelse(test, test_url, pro_url)
        if (latest) {
                what <- paste0("cryptocurrency/listings/latest?convert=", currency)
        } else {
                what <- paste0("cryptocurrency/listings/historical?convert=", currency)
                whatelse <- list(...)
                if (!"date" %in% names(whatelse)) {
                        stop("A 'date' argument is needed for historical data.")
                }
                whatelse <- transform_args(whatelse)
                if (!is.null(whatelse)) {
                        what <- paste0(what,"&", whatelse)
                }
        }
        apiurl <- paste0("https://",base_url,"/v1/",what);

        ## Make Request ##########
        req <- make_request(apiurl, apikey)

        ## Check Response ##########
        check_response(req)

        ## Modify Result ##########
        modify_result(req$content)
}

#' Get the latest/historical market quotes for 1 or more cryptocurrencies
#' @title Get market quotes
#' @inheritParams get_crypto_meta
#' @param currency currency code - Default is 'USD'
#' @param latest If `TRUE` (default), only the latest data is retrieved,
#' otherwise historical data is returned. (NOTE: Historic Data require higher API rights)
#' @param ... Further arguments can be passed to historical data. Further information
#' can be found in the \href{https://coinmarketcap.com/api/documentation/v1/#operation/getV1CryptocurrencyQuotesLatest}{API documentation}
#' @references \href{https://coinmarketcap.com/api/documentation/v1/#operation/getV1CryptocurrencyQuotesLatest}{API documentation}
#' @note At least one "id" or "slug" or "symbol" is required for this request.
#' @return A dataframe with the latest market quote for 1 or more cryptocurrencies
#' @examples \dontrun{
#' get_crypto_quotes(apikey = apikey)
#' get_crypto_quotes(apikey = apikey, symbol="ETH")
#' get_crypto_quotes(apikey = apikey, symbol=c("ETH","BTC"))
#' get_crypto_quotes(apikey = apikey, slug=c("litecoin","dogecoin"))
#' get_crypto_quotes("EUR", apikey = apikey, id=c(3,4))
#' get_crypto_quotes(apikey = apikey, latest = FALSE, symbol = c("BTC","ETH"),
#'                   time_start = Sys.Date()-180, time_end=Sys.Date(), count = 10,
#'                   interval = "30m")
#' }
#' @export
get_crypto_quotes <- function(currency = 'USD', apikey = NULL,
                              symbol = NULL, slug = NULL, id = NULL,
                              latest = TRUE, test = FALSE,
                              ...) {
        ## Input Check ##########
        stopifnot(currency %in% get_valid_currencies())
        if (is.null(apikey)) stop("A valid API key is needed for this request.")

        ## Build Request (new API) ##########
        base_url <- ifelse(test, test_url, pro_url)
        if (latest) {
                what <- paste0("cryptocurrency/quotes/latest?convert=", currency)
                args <- c(is.null(symbol),is.null(id),is.null(slug))
                if (sum(args) == 3) {
                        symbol = "BTC"
                } else if (sum(args) != 2) {
                        stop("You must use either 'symbol', 'id' or 'slug'")
                }
                if (!is.null(symbol)) what <- paste0(what, "&symbol=", paste(symbol, collapse=","))
                if (!is.null(id)) what <- paste0(what, "&id=", paste(id, collapse=","))
                if (!is.null(slug)) what <- paste0(what, "&slug=", paste(slug, collapse=","))
        } else {
                what <- paste0("cryptocurrency/quotes/historical?convert=", currency)
                args <- c(is.null(symbol),is.null(id))
                if (sum(args) == 2) {
                        symbol = "BTC"
                } else if (sum(args) != 1) {
                        stop("You must use either 'symbol' or 'id'")
                }
                if (!is.null(symbol)) what <- paste0(what, "&symbol=", paste(symbol, collapse=","))
                if (!is.null(id)) what <- paste0(what, "&id=", paste(id, collapse=","))

                whatelse <- list(...)
                whatelse <- transform_args(whatelse)
                if (!is.null(whatelse)) {
                        what <- paste0(what,"&", whatelse)
                }
        }
        apiurl <- paste0("https://",base_url,"/v1/",what)

        ## Make Request ##########
        req <- make_request(apiurl, apikey)

        ## Check Response ##########
        check_response(req)

        ## Modify Result ##########
        modify_result(req$content, TRUE, special = 2)
}

#' Get a list of all active market pairs that CoinMarketCap tracks for a
#' given cryptocurrency or fiat currency
#' @title List all active market pairs
#' @references \href{https://coinmarketcap.com/api/documentation/v1/#operation/getV1CryptocurrencyMarketpairsLatest}{API documentation}
#' @inheritParams get_crypto_meta
#' @param currency currency code - Default is 'USD'
#' @param start Optionally offset the start (1-based index) of the paginated
#' list of items to return. - Default is 1
#' @param limit Optionally specify the number of results to return.
#' Use this parameter and the "start" parameter to determine your own
#' pagination size.
#' @note A single cryptocurrency "id", "slug", or "symbol" is required.
#' @return A dataframe with all active market pairs
#' @examples \dontrun{
#' get_crypto_marketpairs("EUR", apikey)
#' get_crypto_marketpairs("EUR", apikey, slug = "bitcoin")
#' get_crypto_marketpairs("EUR", apikey, symbol = "LTC")
#' get_crypto_marketpairs("EUR", apikey, symbol = "BTC", start = 10, limit = 20)
#' }
#' @export
get_crypto_marketpairs <- function(currency = 'USD', apikey,
                                   symbol=NULL, id=NULL, slug=NULL,
                                   start=NULL, limit=NULL, test=FALSE) {
        ## Input Check ##########
        stopifnot(currency %in% get_valid_currencies())
        if (is.null(apikey)) stop("A valid API key is needed for this request.")

        ## Build Request (new API) ##########
        base_url <- ifelse(test, test_url, pro_url)
        what <- paste0("cryptocurrency/market-pairs/latest?convert=", currency)
        args <- c(is.null(symbol),is.null(id),is.null(slug))
        if (sum(args) == 3) {
                symbol = "BTC"
        } else
        if (sum(args) != 2) {
                stop("You must use either 'symbol', 'id' or 'slug'")
        }
        if (!is.null(symbol)) what <- paste0(what, "&symbol=", paste(symbol, collapse=","))
        if (!is.null(id)) what <- paste0(what, "&id=", paste(id, collapse=","))
        if (!is.null(slug)) what <- paste0(what, "&slug=", paste(slug, collapse=","))

        if (!is.null(start)) what <- paste0(what, "&start=", start)
        if (!is.null(limit)) what <- paste0(what, "&limit=", limit)

        apiurl <- paste0("https://",base_url,"/v1/",what)

        ## Make Request ##########
        req <- make_request(apiurl, apikey)

        ## Check Response ##########
        check_response(req)

        ## Modify Result ##########
        modify_result(req$content, TRUE)
}

#' Return the latest/historical OHLCV (Open, High, Low, Close, Volume) market values for
#' one or more cryptocurrencies for the current UTC day. Since the current UTC
#' day is still active these values are updated frequently. You can find the
#' final calculated OHLCV values for the last completed UTC day along with
#' all historic days using /cryptocurrency/ohlcv/historical.
#' @title List latest/historical OHLCV values
#' @references \href{https://coinmarketcap.com/api/documentation/v1/#operation/getV1CryptocurrencyOhlcvLatest}{API documentation}
#' @inheritParams get_crypto_map
#' @param latest If `TRUE` (default), only the latest data is retrieved,
#' otherwise historical data is returned.
#' @param currency currency code - Default is 'USD'
#' @param symbol One or more cryptocurrency symbols.
#' Example: c("BTC","ETH").
#' @param id Alternatively pass one or more CoinMarketCap cryptocurrency IDs.
#' Example: c(1,2)
#' @note One of "id" or "symbol" is required for this request.
#' @return A dataframe with OHLCV values
#' @examples \dontrun{
#' get_crypto_ohlcv("EUR", apikey)
#' get_crypto_ohlcv("EUR", apikey, latest = F)
#' get_crypto_ohlcv("EUR", apikey, latest = F, time_period = "hourly",
#'                  time_start=Sys.Date()-180, count=5, interval="monthly")
#' }
#' @export
get_crypto_ohlcv <- function(currency = 'USD', apikey, latest = TRUE,
                             symbol=NULL, id=NULL, test=FALSE, ...) {
        ## Input Check ##########
        stopifnot(currency %in% get_valid_currencies())
        if (is.null(apikey)) stop("A valid API key is needed for this request.")

        ## Build Request (new API) ##########
        args <- c(is.null(symbol),is.null(id))
        if (sum(args) == 2) {
                symbol = "BTC"
        } else if (sum(args) != 1) {
                stop("You must use either 'symbol' or 'id'")
        }
        base_url <- ifelse(test, test_url, pro_url)
        if (latest) {
                what <- paste0("cryptocurrency/ohlcv/latest?convert=", currency)
                if (!is.null(symbol)) what <- paste0(what, "&symbol=", paste(symbol, collapse=","))
                if (!is.null(id)) what <- paste0(what, "&id=", paste(id, collapse=","))
        } else {
                what <- paste0("cryptocurrency/ohlcv/historical?convert=", currency)
                if (!is.null(symbol)) what <- paste0(what, "&symbol=", paste(symbol, collapse=","))
                if (!is.null(id)) what <- paste0(what, "&id=", paste(id, collapse=","))
                whatelse <- list(...)
                whatelse <- transform_args(whatelse)
                if (!is.null(whatelse)) {
                        what <- paste0(what,"&", whatelse)
                }
        }

        apiurl <- paste0("https://",base_url,"/v1/",what)

        ## Make Request ##########
        req <- make_request(apiurl, apikey)

        ## Check Response ##########
        check_response(req)

        ## Modify Result ##########
        modify_result(req$content, TRUE)
}

