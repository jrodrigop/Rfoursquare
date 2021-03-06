% Generated by roxygen2 (4.0.2): do not edit by hand
\name{venues.stats}
\alias{venues.stats}
\title{Venue Stats}
\usage{
venues.stats(VENUE_ID = NULL, startAt = NULL, endAt = NULL,
  m = "foursquare")
}
\arguments{
\item{VENUE_ID}{required The venue id to retrieve stats for.}

\item{startAt}{The start of the time range to retrieve stats
for (seconds since epoch). If omitted, all-time stats will
be returned.}

\item{endAt}{The end of the time range to retrieve stats for
(seconds since epoch). If omitted, the current time is
assumed.}

\item{m}{Accepts values of "foursquare"}
}
\value{
stats A venue stats object.
}
\description{
Get venue stats over a given time range.
}
\details{
Get venue stats over a given time range. Only available to
	the manager of a venue.
	Note: This function requires acting user
	User restrictions: Must be venue manager
}
\examples{
venues.stats(VENUE_ID="AVNU234", startAt="1284286794",
	endAt="1284286794")
}

