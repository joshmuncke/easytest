#' Test the sum of a column
#'
#' Performs a test that a specific column has an expected sum.
#'
#' @param df A dataframe
#' @param col The unquoted column name of the column you want to test
#' @param sum_expected The expected sum of the column
#'
#' @return The dataframe passed to the function.
#'
#' @details
#' This function is good to utilize after performing joining operations
#' to validate that no duplicates or missing values have been created.
#'
#' @examples
#' # Basic usage
#' test_column_sum(mtcars, wt, 102.953)
#'
#' @export
test_column_sum <- function(df, col, sum_expected) {
  df_name <- deparse(substitute(df))
  col_name <- deparse(substitute(col))
  sum_actual <- df %>% dplyr::summarize(n = sum(!!rlang::enquo(col))) %>% dplyr::pull(n)

  test_message <- glue::glue("Sum of column [{col_name}] in dataframe [{df_name}] == ({sum_expected})", col_name = col_name, df_name = df_name, sum_expected = sum_expected)

  testthat::test_that(test_message,
                      testthat::expect_equal(sum_actual, sum_expected))

  invisible(df)
}

#' Test unique values in a column
#'
#' Performs a test that a specific column has an expected number
#' of unique values.
#'
#' @param df A dataframe
#' @param col The unquoted column name of the column you want to test
#' @param n_expected The expected number of unique values
#'
#' @return The dataframe passed to the function.
#'
#' @details
#' This function is good to utilize after performing joining operations
#' to validate that no duplicates or missing values have been created. Note
#' that this function does not count how many times each unique value
#' appears - just the overall number of distinct items.
#'
#' @examples
#' # Basic usage
#' test_column_n_unique_values(mtcars, cyl, 3)
#'
#' @export
test_column_n_unique_values <- function(df, col, n_expected) {
  df_name <- deparse(substitute(df))
  col_name <- deparse(substitute(col))
  n_actual <- df %>% dplyr::summarize(n = n_distinct(!!rlang::enquo(col))) %>% dplyr::pull(n)

  test_message <- glue::glue("Number of unique values in column [{col_name}] in dataframe [{df_name}] == ({n_expected})", col_name = col_name, df_name = df_name, n_expected = n_expected)

  testthat::test_that(test_message,
                      testthat::expect_equal(n_actual, n_expected))

  invisible(df)
}

#' Test maximum value of a column
#'
#' Performs a test that no value in a specific column is larger
#' than an expected value.
#'
#' @param df A dataframe
#' @param col The unquoted column name of the column you want to test
#' @param max_expected The expected maximum value
#'
#' @return The dataframe passed to the function.
#'
#' @details
#' Note that this test only verifies that all values in the column are
#' less than or equal to the specified value. It does **not** test that the
#' maximum value of the column is equal to a certain value.
#'
#' @examples
#' # Basic usage
#' test_column_max(mtcars, disp, 500)
#'
#' @export
test_column_max <- function(df, col, max_expected) {
  df_name <- deparse(substitute(df))
  col_name <- deparse(substitute(col))
  max_actual <- df %>% dplyr::summarize(n = max(!!rlang::enquo(col))) %>% dplyr::pull(n)

  test_message <- glue::glue("Max value of column [{col_name}] in dataframe [{df_name}] is <= ({max_expected})", col_name = col_name, df_name = df_name, max_expected = max_expected)

  testthat::test_that(test_message,
                      testthat::expect_lte(max_actual, max_expected))

  invisible(df)
}

#' Test minimum value of a column
#'
#' Performs a test that no value in a specific column is smaller
#' than an expected value.
#'
#' @param df A dataframe
#' @param col The unquoted column name of the column you want to test
#' @param min_expected The expected minimum value
#'
#' @return The dataframe passed to the function.
#'
#' @details
#' Note that this test only verifies that all values in the column are
#' greater than or equal to the specified value. It does **not** test that the
#' minimum value of the column is equal to a certain value.
#'
#' @examples
#' # Basic usage
#' test_column_min(mtcars, disp, 50)
#'
#' @export
test_column_min <- function(df, col, min_expected) {
  df_name <- deparse(substitute(df))
  col_name <- deparse(substitute(col))
  min_actual <- df %>% dplyr::summarize(n = min(!!rlang::enquo(col))) %>% dplyr::pull(n)

  test_message <- glue::glue("Min value of column [{col_name}] in dataframe [{df_name}] is >= ({min_expected})", col_name = col_name, df_name = df_name, min_expected = min_expected)

  testthat::test_that(test_message,
                      testthat::expect_gte(min_actual, min_expected))

  invisible(df)
}

#' Test a column is within an expected range
#'
#' Performs a test that no value in a specified column is
#' greater or smaller than an expected minimum and maximum
#' value.
#'
#' @param df A dataframe
#' @param col The unquoted column name of the column you want to test
#' @param min_expected The expected minimum value
#' @param max_expected The expected maximum value
#'
#' @return The dataframe passed to the function.
#'
#' @details
#' This function is also good for verifying that rescaling operations
#' have worked as expected.
#'
#' @examples
#' # Basic usage
#' test_column_between(mtcars, disp, 50, 500)
#'
#' # Test a scaling operation
#' library(dplyr)
#' mtcars %>%
#' mutate(disp_scaled = (disp - min(disp)) / (max(disp) - min(disp))) %>%
#' test_column_between(disp_scaled, 0, 1)
#'
#' @export
test_column_between <- function(df, col, min_expected, max_expected) {
  df_name <- deparse(substitute(df))
  col_name <- deparse(substitute(col))
  min_actual <- df %>% dplyr::summarize(n = min(!!rlang::enquo(col))) %>% dplyr::pull(n)
  max_actual <- df %>% dplyr::summarize(n = max(!!rlang::enquo(col))) %>% dplyr::pull(n)

  test_message <- glue::glue("Value of column [{col_name}] in dataframe [{df_name}] is ({min_expected}) <= [{col_name}] <= ({max_expected})",
                             col_name = col_name, df_name = df_name, min_expected = min_expected, max_expected = max_expected)

  testthat::test_that(test_message,
                      {testthat::expect_gte(min_actual, min_expected)
                        testthat::expect_lte(max_actual, max_expected)})

  invisible(df)
}

#' Test the mean value of a column
#'
#' Performs a test that a specific column has an expected mean value.
#' Optionally - an acceptable tolerance can be specified in standard
#' deviations.
#'
#' @param df A dataframe
#' @param col The unquoted column name of the column you want to test
#' @param mean_expected The expected mean value
#' @param sd_tolerance The maximum number of standard deviations (+/-) tolerance
#'
#' @return The dataframe passed to the function.
#'
#' @examples
#' # Basic usage
#' test_column_mean(mtcars, qsec, 17.84875)
#'
#' # Usage without a stdev tolerance means match must be exactly equal
#' # Often including a small tolerance is good to allow for rounding errors
#' mtcars %>%
#' test_column_mean(mpg, 20.09062, 0.01)
#'
#' @export
test_column_mean <- function(df, col, mean_expected, sd_tolerance = 0) {
  df_name <- deparse(substitute(df))
  col_name <- deparse(substitute(col))
  mean_actual <- df %>% dplyr::summarize(n = mean(!!rlang::enquo(col))) %>% dplyr::pull(n)
  sd_actual <- df %>% dplyr::summarize(n = sd(!!rlang::enquo(col))) %>% dplyr::pull(n)

  # Check standard deviation tolerance is >= 0
  if(!sd_actual>=0) {
    stop("Standard deviation tolerance cannot be negative", call. = F)
  }

  absolute_sd_diff = abs((mean_actual - mean_expected) / actual_sd)

  if(sd == 0) {
    test_message <- glue::glue("Mean of column [{col_name}] in dataframe [{df_name}] == ({mean_expected})",
                               col_name = col_name, df_name = df_name, mean_expected = mean_expected)

    testthat::test_that(test_message, testthat::expect_equal(mean_actual, mean_expected))
  }

  else {
    test_message <- glue::glue("Mean of column [{col_name}] in dataframe [{df_name}] is within ({sd_tolerance}) standard deviations of ({mean_expected})",
                               col_name = col_name, df_name = df_name, sd_tolerance = sd_tolerance, mean_expected = mean_expected)

    testthat::test_that(test_message, testthat::expect_lte(absolute_sd_diff, sd_tolerance))
  }

  invisible(df)
}
