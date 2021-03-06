% Generated by roxygen2 (4.0.2): do not edit by hand
\name{lists.saves}
\alias{lists.saves}
\title{Users who have saved a list}
\usage{
lists.saves(LIST_ID = NULL, m = "foursquare")
}
\arguments{
\item{LIST_ID}{The ID of the list to get saves for.}

\item{m}{Accepts values of "foursquare"}
}
\value{
saves A count and groups of users who saved this
	list. Groups generally include friends and others
	(indicating the relationship to the acting user), but are
	subject to change.
}
\description{
Friends and a total count of users who have saved this list.
}
\details{
Returns friends and a total count of users who have saved
	this list.
}
\examples{
lists.saves(LIST_ID="HZXXY3Y")
}

