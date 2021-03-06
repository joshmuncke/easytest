% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/col-tests.R
\name{test_column_min}
\alias{test_column_min}
\title{Test minimum value of a column}
\usage{
test_column_min(df, col, min_expected)
}
\arguments{
\item{df}{A dataframe}

\item{col}{The unquoted column name of the column you want to test}

\item{min_expected}{The expected minimum value}
}
\value{
The dataframe passed to the function.
}
\description{
Performs a test that no value in a specific column is smaller
than an expected value.
}
\details{
Note that this test only verifies that all values in the column are
greater than or equal to the specified value. It does \strong{not} test that the
minimum value of the column is equal to a certain value.
}
\examples{
# Basic usage
test_column_min(mtcars, disp, 50)

}
