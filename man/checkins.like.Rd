% Generated by roxygen2 (4.0.2): do not edit by hand
\name{checkins.like}
\alias{checkins.like}
\title{Like or unlike a checkin}
\usage{
checkins.like(CHECKIN_ID = NULL, set = NULL, m = "swarm")
}
\arguments{
\item{CHECKIN_ID}{required The checkin to like or unlike.}

\item{set}{If 1, like this checkin. If 0 unlike (un-do a
previous like) it. Default value is 1.}

\item{m}{Accepts values of "swarm"}
}
\value{
likes Updated count and groups of users who like
	this checkin. Groups generally include friends and others
	(indicating the relationship to the acting user), but are
	subject to change.
}
\description{
Allows the acting user to like or unlike a checkin.
}
\details{
Allows the acting user to like or unlike a checkin.
	Note: This function requires acting user
}
\examples{
checkins.like(CHECKIN_ID="XXX123YYYY", set="1")
}

