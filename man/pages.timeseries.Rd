% Generated by roxygen2 (4.0.2): do not edit by hand
\name{pages.timeseries}
\alias{pages.timeseries}
\title{Page Venues Time Series Data}
\usage{
pages.timeseries(PAGE_ID = NULL, startAt = NULL, endAt = NULL,
  fields = NULL, m = "foursquare")
}
\arguments{
\item{PAGE_ID}{The page whose venues to get timeseries data
for}

\item{startAt}{required. The start of the time range to
retrieve stats for (seconds since epoch).}

\item{endAt}{The end of the time range to retrieve stats for
(seconds since epoch). If omitted, the current time is
assumed.}

\item{fields}{Specifies which fields to return. May be one
or more of totalCheckins, newCheckins, uniqueVisitors,
sharing, genders, ages, hours, separated by commas.}

\item{m}{Accepts values of "foursquare"}
}
\value{
timeseries An array of venue time series data
	objects, one for each venue the page manages.
}
\description{
Get daily venue stats for venues managed by a page.
}
\details{
Get daily venue stats for venues managed by a page over a
	time range.
	Note: This function requires acting user
	User restrictions: Must be venue manager
}
\examples{
pages.timeseries(PAGE_ID="2345", startAt="1284286794",
	endAt="1284286794", fields="totalCheckins,newCheckins")
}

