#' Test the sum of a column
#'
#' Performs a test that a specific column has an expected sum.
#'
#' @param df A dataframe
#' @param col The unquoted column name of the column you want to test
#' @param sum The expected sum of the column
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
test_column_sum <- function(df, col, sum) {
  df_name <- deparse(substitute(df))
  col_name <- deparse(substitute(col))
  actual_sum <- df %>% dplyr::summarize(n = sum(!!rlang::enquo(col))) %>% dplyr::pull(n)

  test_message <- glue::glue("Sum of column [{col_name}] in dataframe [{df_name}] == ({sum})", col_name = col_name, df_name = df_name, sum = sum)

  testthat::test_that(test_message,
                      testthat::expect_equal(actual_sum, sum))

  invisible(df)
}

#' Test unique values in a column
#'
#' Performs a test that a specific column has an expected number
#' of unique values.
#'
#' @param df A dataframe
#' @param col The unquoted column name of the column you want to test
#' @param n The expected number of unique values
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
test_column_n_unique_values <- function(df, col, n) {
  df_name <- deparse(substitute(df))
  col_name <- deparse(substitute(col))
  actual_n <- df %>% dplyr::summarize(n = n_distinct(!!rlang::enquo(col))) %>% dplyr::pull(n)

  test_message <- glue::glue("Number of unique values in column [{col_name}] in dataframe [{df_name}] == ({n})", col_name = col_name, df_name = df_name, n = n)

  testthat::test_that(test_message,
                      testthat::expect_equal(actual_n, n))

  invisible(df)
}

#' Test maximum value of a column
#'
#' Performs a test that no value in a specific column is larger
#' than an expected value.
#'
#' @param df A dataframe
#' @param col The unquoted column name of the column you want to test
#' @param max The expected maximum value
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
#' test_column_max(mtcars, cyl, 400)
#'
#' @export
test_column_max <- function(df, col, max) {
  df_name <- deparse(substitute(df))
  col_name <- deparse(substitute(col))
  actual_max <- df %>% dplyr::summarize(n = max(!!rlang::enquo(col))) %>% dplyr::pull(n)

  test_message <- glue::glue("Max value of column [{col_name}] in dataframe [{df_name}] is <= ({max})", col_name = col_name, df_name = df_name, max = max)

  testthat::test_that(test_message,
                      testthat::expect_lte(actual_max, max))

  invisible(df)
}

test_column_min <- function(df, col, min) {
  df_name <- deparse(substitute(df))
  col_name <- deparse(substitute(col))
  actual_min <- df %>% dplyr::summarize(n = min(!!rlang::enquo(col))) %>% dplyr::pull(n)

  test_message <- glue::glue("Min value of column [{col_name}] in dataframe [{df_name}] is >= ({min})", col_name = col_name, df_name = df_name, min = min)

  testthat::test_that(test_message,
                      testthat::expect_gte(actual_min, min))

  invisible(df)
}

test_column_between <- function(df, col, min, max) {
  df_name <- deparse(substitute(df))
  col_name <- deparse(substitute(col))
  actual_min <- df %>% dplyr::summarize(n = min(!!rlang::enquo(col))) %>% dplyr::pull(n)
  actual_max <- df %>% dplyr::summarize(n = max(!!rlang::enquo(col))) %>% dplyr::pull(n)

  test_message <- glue::glue("Value of column [{col_name}] in dataframe [{df_name}] is ({min}) <= [{col_name}] <= ({max})", col_name = col_name, df_name = df_name, min = min, max = max)

  testthat::test_that(test_message,
                      {testthat::expect_gte(actual_min, min)
                        testthat::expect_lte(actual_max, max)})

  invisible(df)
}

test_column_mean <- function(df, col, mu, sd = 0) {
  df_name <- deparse(substitute(df))
  col_name <- deparse(substitute(col))
  actual_mean <- df %>% dplyr::summarize(n = mean(!!rlang::enquo(col))) %>% dplyr::pull(n)
  actual_sd <- df %>% dplyr::summarize(n = sd(!!rlang::enquo(col))) %>% dplyr::pull(n)

  delta_mean = actual_mean - mu

  if(!sd>=0) {
    stop("Standard deviation tolerance must be positive", call. = F)
  }

  if(sd == 0) {
    test_message <- glue::glue("Mean of column [{col_name}] in dataframe [{df_name}] == ({mean})",
                               col_name = col_name, df_name = df_name, mean = mu)

    testthat::test_that(test_message, testthat::expect_equal(actual_mean, mu))
  }

  else {
    test_message <- glue::glue("Mean of column [{col_name}] in dataframe [{df_name}] is within ({sd}) standard deviations of ({mean})",
                               col_name = col_name, df_name = df_name, sd = sd, mean = mu)

    absolute_sd_diff = abs(delta_mean / actual_sd)

    testthat::test_that(test_message, testthat::expect_lte(absolute_sd_diff, sd))
  }

  invisible(df)
}
