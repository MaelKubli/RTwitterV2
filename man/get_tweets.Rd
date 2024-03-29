% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_tweets.R
\name{get_tweets}
\alias{get_tweets}
\title{This function allows you to collect tweets with there status_id (max 100 tweet ids per call)}
\usage{
get_tweets(
  token = NA,
  tweet_ids = NA,
  tweet_fields = "ALL",
  user_fields = "ALL",
  api_wait = 12,
  expansions = "ALL",
  place_fields = "ALL",
  media_fields = "ALL",
  poll_fields = "NONE",
  JSON = FALSE,
  storage_path = "get_tweets_by_ids.json",
  n_try = 10
)
}
\arguments{
\item{token}{string of the bearer token from your personal twitter API access}

\item{tweet_ids}{string representing the query to search tweets with}

\item{tweet_fields}{string which defaults to ALL (no other argument accepted at the moment)}

\item{user_fields}{string which defaults to ALL (no other argument accepted at the moment)}

\item{api_wait}{integer specifying how long the function should wait for the API to answer (defaults to 12 seconds)}

\item{expansions}{string which defaults to ALL (no other argument accepted at the moment)}

\item{place_fields}{string which defaults to ALL (no other argument accepted at the moment)}

\item{media_fields}{string which defaults to ALL (no other argument accepted at the moment)}

\item{poll_fields}{string which defaults to NONE (no other argument accepted at the moment)}

\item{JSON}{boolean which defaults to FALSE}

\item{storage_path}{character string specifying the path and file name to write the json file to}

\item{n_try}{integer specifying number of retries in case of 503 error}
}
\value{
a data frame
}
\description{
This function allows you to collect tweets with there status_id (max 100 tweet ids per call)
}
\examples{
\dontrun{
users <- get_tweets(token=bearer_token, tweet_ids = "123456789458,45678988975744")
}
}
