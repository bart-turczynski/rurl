% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rurl.R
\name{get_host}
\alias{get_host}
\title{Get URL hosts}
\usage{
get_host(url, protocol_handling = "keep")
}
\arguments{
\item{url}{A character vector containing URLs from which to extract the host.}

\item{protocol_handling}{A character string specifying how to handle protocols.
Can be one of "keep", "none", "strip", "http", "https".
The protocol is preserved if it exists, and "http://" is
added if missing. If "none", no protocol is added. If
"http://" or "https://" the given protocol is added
or changed to the one indicated.}
}
\value{
A character vector with the host of each URL.
In layman's terms, the host being the part of the address "between the
protocol and first slash / end of the string if no slash is present, e.g.,
test.wordpress.org, www.r-project.org. Note the host and the domain may be
the same thing but for different reasons.
}
\description{
This function extracts the host (domain) of a given URL. It returns the host
name (e.g., "example.com") of the URL.
}
\examples{
get_host("http://example.com")
get_host("ftp://example.com")
get_host("https://sub.domain.com")
}
