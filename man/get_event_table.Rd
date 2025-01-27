% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explore.R
\name{get_event_table}
\alias{get_event_table}
\title{Show the number of events per patient}
\usage{
get_event_table(df)
}
\arguments{
\item{df}{Diabetes data frame produced by \link{read_diabetes}}
}
\description{
Show the number of events per patient
}
\examples{
\dontrun{
df <- read_diabetes()
get_event_table(df)
}
}
