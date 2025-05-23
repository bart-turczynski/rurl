% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rurl.R
\name{get_scheme}
\alias{get_scheme}
\title{Get URL schemes}
\usage{
get_scheme(url, protocol_handling = "keep")
}
\arguments{
\item{url}{A character vector containing URLs from which to extract the scheme.}

\item{protocol_handling}{A character string specifying how to handle protocols.
Can be one of "keep", "none", "strip", "http", "https".
The protocol is preserved if it exists, and "http://" is
added if missing. If "none", no protocol is added. If
"http://" or "https://" the given protocol is added
or changed to the one indicated.}
}
\value{
A character vector with the scheme (e.g., "http") of each URL.
}
\description{
This function extracts the scheme (protocol) of a given URL. It returns the
scheme (e.g., "http", "https", "ftp", etc.) of the URL.
}
\examples{
get_scheme("http://example.com")
get_scheme("ftp://example.com")
get_scheme("https://example.com")
}
