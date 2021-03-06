% Generated by roxygen2 (4.0.2): do not edit by hand
\name{updates.notifications}
\alias{updates.notifications}
\title{Notification tray}
\usage{
updates.notifications(limit = NULL, m = "foursquare")
}
\arguments{
\item{limit}{Maximum number of results to return, up to 99.
Notifications are grouped over time, so there will usually
be fewer than 99 results available at any given time.
offset 0 Used to page through results. Only the 99 most
recent notifications are visible, so offset must be no more
than 99 - limit.}

\item{m}{Accepts values of "foursquare" or "swarm"}
}
\value{
notifications The user's notification updates.
}
\description{
Retrieve a user's notification tray notifications.
}
\details{
Retrieve a user's notification tray notifications
	Note: This function requires acting user
}
\examples{
updates.notifications(limit="20")
}

