
transform_args <- function(args) {
    if (length(args) == 0) {
        return(NULL)
    } else {
        args <- lapply(1:length(args), function(x) {
            if (inherits(args[[x]], "Date") ||
                inherits(args[[x]], "POSIXct")) {
                args[[x]] <- format(args[[x]], "%Y-%m-%dT%H:%M:%S")
            }
            paste0(names(args)[x], "=", paste0(args[[x]], collapse = ","),
                   collapse = "=")
        })
        paste0(args, collapse = "&")
    }
}

make_request <- function(apiurl) {
    apikey <- .get_api_key()
    if (is.null(apikey) || apikey == "")
        stop("A valid API key is needed for this request. ",
             "Please call setup() first.")

    h <- new_handle()
    handle_setheaders(h, "Accepts" = "application/json",
                      "X-CMC_PRO_API_KEY" = apikey)
    curl_fetch_memory(apiurl, handle = h)
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
        l <- lapply(fromJSON(rawToChar(content), flatten = T)$data, lapply,
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
        colnames(d) <- c(names(l[[1]])[1:length(l[[1]]) - 1],
                         paste0("quote_", names(l[[1]]$quote[[1]])))
    }

    ## Convert timestamp columns
    colnams <- colnames(d)
    a <- c(grep("last_updated", colnams),
           grep("date_added", colnams),
           grep("first_historical_data", colnams),
           grep("last_historical_data", colnams),
           grep("timestamp", colnams))

    d[, a] <- lapply(a, function(x) {
        d[, x] <- as.POSIXct(as.character(d[, x]),
                             format = "%Y-%m-%dT%H:%M:%S.%OS")
    })

    return(d)
}



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
