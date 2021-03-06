% Generated by roxygen2 (4.0.2): do not edit by hand
\name{pages.access}
\alias{pages.access}
\title{Access Token}
\usage{
pages.access(USER_ID = "self", m = "foursquare")
}
\arguments{
\item{USER_ID}{required The page you want similar pages for.}

\item{m}{Accepts values of "foursquare"}
}
\value{
access_token An OAuth access token for this page for
	the acting OAuth consumer.
}
\description{
Returns an OAuth access token for the specified page.
}
\details{
Returns an OAuth access token for the specified page.  The
	requesting user must be a manager of the page.
	Note: This function requires acting user
}
\examples{
pages.access(USER_ID="HZXXY3")
}

