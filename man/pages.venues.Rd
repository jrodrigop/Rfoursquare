% Generated by roxygen2 (4.0.2): do not edit by hand
\name{pages.venues}
\alias{pages.venues}
\title{Page Venues}
\usage{
pages.venues(PAGE_ID = NULL, ll = NULL, radius = NULL, sw = NULL,
  ne = NULL, offset = NULL, limit = NULL, storeId = NULL,
  m = "foursquare")
}
\arguments{
\item{PAGE_ID}{required The page id for which venues are
being requested.}

\item{ll}{optional Not valid with ne or sw. Limits results
to venues near this latitude and longitude within an
optional radius.}

\item{radius}{optional Can be used when including ll.  Not
valid with ne or sw. Limit results to venues within this
many meters of the specified ll. The maximum supported
radius is currently 100,000 meters.}

\item{sw}{With ne, limits results to the bounding quadrangle
defined by the latitude and longitude given by sw as its
south-west corner, and ne as its north-east corner. Not
valid with ll or radius. Bounding quadrangles with an area
up to approximately 10,000 square kilometers are supported.}

\item{ne}{See sw}

\item{offset}{The offset of which venues to return. Defaults
to 0.}

\item{limit}{The number of venues to return. Defaults to 20,
max of 100.}

\item{storeId}{optional Only return venues whose storeId
matches. storeIds are defined by the page manager (and
therefore namespaced to that particular page). They are the
page's own internal identifier for that venue. Cannot be
used with any geo params.}

\item{m}{Accepts values of "foursquare"}
}
\value{
venues A count and items of compact venues.
}
\description{
Allows you to get the page's venues.
}
\details{
Allows you to get the page's venues.
}
\examples{
pages.venues(PAGE_ID="AVNU234", ll="44.3,37.2",
	radius="800", sw="44.3,37.2", ne="44.1,37.4", offset="50",
	limit="30", storeId="995")
}

