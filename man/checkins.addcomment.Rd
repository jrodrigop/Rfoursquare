% Generated by roxygen2 (4.0.2): do not edit by hand
\name{checkins.addcomment}
\alias{checkins.addcomment}
\title{Add a comment to a check-in}
\usage{
checkins.addcomment(CHECKIN_ID = NULL, text = NULL, mentions = NULL,
  m = "swarm")
}
\arguments{
\item{CHECKIN_ID}{The ID of the checkin to add a comment to.}

\item{text}{The text of the comment, up to 200 characters.}

\item{mentions}{Mentions in your check-in. This parameter is
a semicolon-delimited list of mentions. A single mention is
of the form "start,end,userid", where start is the index of
the first character in the shout representing the mention,
end is the index of the first character in the shout after
the mention, and userid is the userid of the user being
mentioned. Character indices in shouts are 0-based.}

\item{m}{Accepts values of "swarm"}
}
\value{
comment The newly-created comment.
}
\description{
Comment on a checkin-in.
}
\details{
Comment on a checkin-in
	Note: This function requires acting user
}
\examples{
checkins.addcomment(CHECKIN_ID="IHR8THISVNU",
	text="Awesome!", mentions="5,10,HZXXY3Y;15,20,GZYYZ3Z")
}

