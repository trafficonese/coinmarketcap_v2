% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup.R
\name{setup}
\alias{setup}
\alias{get_setup}
\alias{reset_setup}
\title{Setup}
\usage{
setup(api_key = NULL, sandbox = FALSE)

get_setup()

reset_setup(api_key = TRUE, sandbox = TRUE)
}
\arguments{
\item{api_key}{Your Coinmarketcap API key.}

\item{sandbox}{Sets the base URL for the API. If set to TRUE, the sandbox-API
is called. The default is FALSE.}
}
\description{
Specifies API Key and the base URL for session
}
\examples{
setup("xXXXXxxxXXXxx")
get_setup()

}
