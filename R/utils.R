#' Get Valid Currencies
#' @return A character vector of valid currencies supported by coinmarketcap API
#' @examples
#' get_valid_currencies()
#' @export
get_valid_currencies <- function(){

        currencies_list <- c("AUD", "BRL", "CAD", "CHF",
                             "CLP", "CNY", "CZK", "DKK",
                             "EUR", "GBP", "HKD", "HUF",
                             "IDR", "ILS", "INR", "JPY",
                             "KRW", "MXN", "MYR", "NOK",
                             "NZD", "PHP", "PKR", "PLN",
                             "RUB", "SEK", "SGD", "THB",
                             "TRY", "TWD", "ZAR", "USD")

        return(currencies_list)
}


transform_args <- function(args) {
        if (length(args) == 0) {
                return(NULL)
        } else {
                args <- lapply(1:length(args), function(x) {
                        if (inherits(args[[x]], "Date") || inherits(args[[x]], "POSIXct")) {
                                args[[x]] <- format(args[[x]], "%Y-%m-%dT%H:%M:%S")
                        }
                        paste0(names(args)[x],"=", paste0(args[[x]], collapse=","), collapse = "=")
                })
                paste0(args, collapse = "&")
        }
}

make_request <- function(apiurl, apikey) {
        h <- new_handle()
        handle_setheaders(h, 'Accepts' = 'application/json',
                          'X-CMC_PRO_API_KEY' = apikey)
        req <- curl_fetch_memory(apiurl, handle = h)

}

check_response <- function(req) {
        if (req$status_code != 200) {
                stop("The request was not succesfull! \n",
                     "Request URL:\n", req$url, "\n",
                     "Response Content:\n", jsonlite::prettify(rawToChar(req$content)))
        }
}

modify_result <- function(content, filter = FALSE, special = 1, ncols=20) {
        if (filter) {
                l <- lapply(fromJSON(rawToChar(content), flatten=T)$data, lapply,
                                       function(x)ifelse(is.null(x), NA, x))
        } else {
                l <- fromJSON(rawToChar(content), flatten = T)$data
        }
        if (special == 1) {
                d <- data.frame(l)
                colnames(d) <- gsub(".", "_", colnames(d), fixed = T)
                colnames(d) <- gsub("quote_", "", colnames(d), fixed = T)
        } else if (special == 2) {
                d <- data.frame(matrix(unlist(l), ncol = ncols, byrow = T),
                                stringsAsFactors = F)
                colnames(d) <- c(names(l[[1]])[1:length(l[[1]])-1],
                                 paste0("quote_", names(l[[1]]$quote[[1]])))
        }

        ## Change date/Timestamp columns to POSIXct #####################
        colnams <- colnames(d)
        a <- c(grep("last_updated", colnams),
               grep("date_added", colnams),
               grep("first_historical_data", colnams),
               grep("last_historical_data", colnams),
               grep("timestamp", colnams))
        # which(colnams %in% c("last_updated","quote_last_updated",
        #                      "date_added","timestamp","first_historical_data", "last_historical_data"))

        d[,a] <- lapply(a, function(x) {
                d[,x] <- as.POSIXct(as.character(d[,x]), format = "%Y-%m-%dT%H:%M:%S.%OS")
        })
        return(d)
}


