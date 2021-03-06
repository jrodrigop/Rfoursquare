% Generated by roxygen2 (4.0.2): do not edit by hand
\name{pages.similar}
\alias{pages.similar}
\title{Similar Pages}
\usage{
pages.similar(USER_ID = "self", limit = NULL, offset = NULL,
  includeFollowing = NULL, m = "foursquare")
}
\arguments{
\item{USER_ID}{required The page you want similar pages for.}

\item{limit}{Number of results to return, up to 500.}

\item{offset}{Used to page through results.}

\item{includeFollowing}{optional Boolean indicating whether
results include pages already being followed by the user.}

\item{m}{Accepts values of "foursquare"}
}
\value{
similarPages A count and items array. Each element
	in items contains a pageInfo compact page, a user compact
	user, Both followers and tips contain a count and tips may
	also contain an array of selected tips as items.
}
\description{
Returns a list of pages similar to the specified page.
}
\details{
Returns a list of pages similar to the specified page.
	Note: This function requires acting user
}
\examples{
pages.similar(USER_ID="HZXXY3", limit="20", offset="100",
	includeFollowing="true")
}

