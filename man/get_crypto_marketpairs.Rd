% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/currencies.R
\name{get_crypto_marketpairs}
\alias{get_crypto_marketpairs}
\title{List all active market pairs}
\usage{
get_crypto_marketpairs(currency = "USD", symbol = NULL, id = NULL,
  slug = NULL, start = NULL, limit = NULL)
}
\arguments{
\item{currency}{currency code - Default is 'USD'}

\item{symbol}{One or more cryptocurrency symbols.
Example: c("BTC","ETH").}

\item{id}{Alternatively pass one or more CoinMarketCap cryptocurrency IDs.
Example: c(1,2)}

\item{slug}{Alternatively pass a vector of exchange slugs.
Example: c("binance","cryptsy")}

\item{start}{Optionally offset the start (1-based index) of the paginated
list of items to return. - Default is 1}

\item{limit}{Optionally specify the number of results to return.
Use this parameter and the "start" parameter to determine your own
pagination size.}
}
\value{
A dataframe with all active market pairs
}
\description{
Get a list of all active market pairs that CoinMarketCap tracks for a
given cryptocurrency or fiat currency
}
\note{
A single cryptocurrency "id", "slug", or "symbol" is required.
}
\examples{
\dontrun{
get_crypto_marketpairs("EUR")
get_crypto_marketpairs("EUR", slug = "bitcoin")
get_crypto_marketpairs("EUR", symbol = "LTC")
get_crypto_marketpairs("EUR", symbol = "BTC", start = 10, limit = 20)
}
}
\references{
\href{https://coinmarketcap.com/api/documentation/v1/#operation/getV1CryptocurrencyMarketpairsLatest}{API documentation}
}
