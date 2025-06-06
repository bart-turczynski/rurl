% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rurl.R
\name{get_tld}
\alias{get_tld}
\title{Extract the top-level domain (TLD) from a URL}
\usage{
get_tld(url, source = c("all", "private", "icann"))
}
\arguments{
\item{url}{A character vector of URLs.}

\item{source}{Which TLD source to use: "all", "icann", or "private".}
}
\value{
A character vector of TLDs.
}
\description{
Extract the top-level domain (TLD) from a URL
}
\examples{
get_tld("example.com")
}
