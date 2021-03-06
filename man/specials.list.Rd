% Generated by roxygen2 (4.0.2): do not edit by hand
\name{specials.list}
\alias{specials.list}
\title{List Specials}
\usage{
specials.list(venueId = NULL, status = NULL, m = "foursquare")
}
\arguments{
\item{venueId}{comma-separated list of venue IDs; filters
results to the specials  assigned to the venue(s).}

\item{status}{(optional, requires venueId) which specials to
return: pending, active, expired, all}

\item{m}{Accepts values of "foursquare"}
}
\value{
specials A count and items of specials.
}
\description{
List available specials.
}
\details{
List available specials.
	Note: This function requires acting user
	User restrictions: Must be venue manager
}
\examples{
specials.list(venueId="IHR8THISVNU", status="active")
}

