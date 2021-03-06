% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fret_format_data.R
\name{fret_format_one_dataset}
\alias{fret_format_one_dataset}
\title{Format a single dataframe for subsequent processing}
\usage{
fret_format_one_dataset(raw_data, experiment_name)
}
\arguments{
\item{raw_data}{A single dataframe to process.}

\item{experiment_name}{The name of the corresponding experiment.}
}
\value{
A dataframe containing 8 columns: \code{Experiment}, \code{Type},
    \code{Replicate}, \code{Observation}, \code{fret_channel},
    \code{acceptor_channel}, \code{donor_channel} and \code{concentration}.
}
\description{
This internal function processes a single dataframe to generate
    the \code{Experiment}, \code{Type}, \code{Replicate}, and
    \code{Observation} columns required for subsequent processing by
    \code{\link{fret_average_replicates}} and
    \code{\link{fret_correct_signal}}.
}
\examples{
\dontrun{
format_one_dataset(my_data, "my_experiment")
}

}
