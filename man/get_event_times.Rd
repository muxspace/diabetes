% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explore.R
\name{get_event_times}
\alias{get_event_times}
\title{Get the times associated with events for a given feature}
\usage{
get_event_times(df, feature, id = NULL)
}
\arguments{
\item{df}{Diabetes data frame produced by \link{read_diabetes}}

\item{feature}{The feature to get times for}

\item{id}{Limit to a specific patient}
}
\description{
Get the times associated with events for a given feature
}
\examples{
\dontrun{
table(get_event_times(df, 58))
}
}
