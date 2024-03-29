% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/global_metrics.R
\name{get_global_marketcap}
\alias{get_global_marketcap}
\title{Extract Global Market Cap of Cryptocurrency Market}
\usage{
get_global_marketcap(currency = "USD", latest = TRUE, ...)
}
\arguments{
\item{currency}{currency code - Default is 'USD'}

\item{latest}{If `TRUE` (default), only the latest data is retrieved,
otherwise historical data is returned. (NOTE: Historic Data require higher API rights)}

\item{...}{Further arguments can be passed to historical data. Further information
can be found in the \href{https://coinmarketcap.com/api/documentation/v1/#operation/getV1GlobalmetricsQuotesHistorical}{API documentation}}
}
\value{
A dataframe with global market cap of Cryptocurrencies
}
\description{
Extract Global Market Cap of Cryptocurrency Market
}
\examples{
\dontrun{
get_global_marketcap('AUD')
get_global_marketcap('EUR')
get_global_marketcap(latest = FALSE, count = 10, interval = "yearly",
                     time_start = Sys.Date()-180, time_end = Sys.Date())

}
}
