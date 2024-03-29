% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_users.R
\name{get_user_v2}
\alias{get_user_v2}
\title{This function allows you to collect data about one ore more users on twitter}
\usage{
get_user_v2(
  token = NA,
  user_ids = NA,
  user_names = NA,
  user_fields = "ALL",
  tweet_fields = "author_id",
  expansions = "ALL",
  JSON = FALSE
)
}
\arguments{
\item{token}{string of the bearer token from your personal twitter API access}

\item{user_ids}{string of twitter users ids (multiple ids are separated with a comma)}

\item{user_names}{string of twitter user names (multiple user names are separated with a comma)}

\item{user_fields}{string which defaults to ALL (no other argument accepted at the moment)}

\item{tweet_fields}{string which defaults to author_id (no other argument accepted at the moment)}

\item{expansions}{string which defaults to ALL (no other arguent accepted at the moment)}

\item{JSON}{boolean which defaults to FALSE (no other argument accepted at the moment)}
}
\value{
a data frame
}
\description{
This function allows you to collect data about one ore more users on twitter
}
\examples{
\dontrun{
users <- get_users_v2(token=bearer_token, user_names = "Twitter,TiwtterDev")
}
}
