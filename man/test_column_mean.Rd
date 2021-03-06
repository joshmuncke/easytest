% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/col-tests.R
\name{test_column_mean}
\alias{test_column_mean}
\title{Test the mean value of a column}
\usage{
test_column_mean(df, col, mean_expected, sd_tolerance = 0)
}
\arguments{
\item{df}{A dataframe}

\item{col}{The unquoted column name of the column you want to test}

\item{mean_expected}{The expected mean value}

\item{sd_tolerance}{The maximum number of standard deviations (+/-) tolerance}
}
\value{
The dataframe passed to the function.
}
\description{
Performs a test that a specific column has an expected mean value.
Optionally - an acceptable tolerance can be specified in standard
deviations.
}
\examples{
# Basic usage
test_column_mean(mtcars, qsec, 17.84875)

# Usage without a stdev tolerance means match must be exactly equal
# Often including a small tolerance is good to allow for rounding errors
mtcars \%>\%
test_column_mean(mpg, 20.09062, 0.01)

}
