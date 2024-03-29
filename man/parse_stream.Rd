% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_stream.R
\name{parse_stream}
\alias{parse_stream}
\title{Converts Twitter stream data (JSON file) into parsed data frame.}
\usage{
parse_stream(path, filtered = F)
}
\arguments{
\item{path}{Character, name of JSON file with data collected by
[stream_tweets()].}

\item{filtered}{logical, indicating whether the stream to parse is filtered or not. Defaults to FALSE. This will add the filter rule variables to the data}
}
\value{
a data frame
}
\description{
Converts Twitter stream data (JSON file) into parsed data frame.
}
\examples{
\dontrun{
stream_tweets(timeout = 1, file_name = "stream.json", parse = FALSE)
parse_stream("stream.json")
}

}
