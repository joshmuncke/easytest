test_no_duplicates <- function(df, ...) {
  df <- df %>% dplyr::select(!!!enquos(...))

  testthat::test_that(paste0("No duplicates in dataframe [", deparse(substitute(df)), "]"),
                      testthat::expect_identical(df, dplyr::distinct(df)))

  invisible(df)
}

test_complete <- function(df) {
  testthat::test_that(paste0("Dataframe [", deparse(substitute(df)), "] is complete"),
                      testthat::expect_identical(df, df %>% dplyr::filter(complete.cases(.))))

  invisible(df)
}

#' Test the number rows in a dataframe
#'
#' Performs a test that the dataframe has the number of rows you expect.
#'
#' @param df A dataframe
#' @param n Expected number of rows
#'
#' @return The dataframe passed to the function.
#'
#' @examples
#' # Basic usage
#' test_n_rows(mtcars, 32)
#'
#' # Can also be used in magrittr pipe
#' mtcars %>% test_n_rows(32) %>% select(mpg)
#'
#' @export
test_n_rows <- function(df, n) {
  df_name <- deparse(substitute(df))
  df2_name <- deparse(substitute(df2))

  df_rows <- nrow(df)

  test_message <- glue::glue("Number of rows in dataframe [{df_name}] is == ({n})", df_name = df_name, n = n)

  testthat::test_that(test_message,
                      testthat::expect_equal(df_rows, n))

  invisible(df)
}

#' Test that two dataframes are equal
#'
#' Performs a test that the dataframe is equal to another dataframe.
#'
#' @param df A dataframe
#' @param df2 Another dataframe
#'
#' @return df
#'
#' @examples
#' # Basic usage
#' test_same_as_other_df(mtcars, mtcars)
#' test_same_as_other_df(mtcars, iris)
#'
#' # Can also be used in magrittr pipe
#' mtcars %>%
#' mutate(mpg = 50) %>%
#' test_same_as_other_df(mtcars)
#'
#' @export
test_same_as_other_df <- function(df, df2) {
  df_name <- deparse(substitute(df))
  df2_name <- deparse(substitute(df2))

  test_message <- glue::glue("Dataframe [{df_name}] is equal to dataframe [{df2_name}]", df_name = df_name, df2_name = df2_name)

  testthat::test_that(test_message,
                      testthat::expect_equal(df, df2))

  invisible(df)
}

#' Test that two dataframes have the same number of rows
#'
#' Performs a test that the dataframe has the number of rows as
#' another dataframe (even if the contents are different).
#'
#' @param df A dataframe
#' @param df2 Another dataframe
#'
#' @return df
#'
#' @examples
#' # Basic usage
#' test_equal_rows(mtcars, mtcars)
#' test_equal_rows(mtcars, iris)
#'
#' # Can also be used in magrittr pipe
#' mtcars %>%
#' mutate(mpg = 50) %>%
#' test_equal_rows(mtcars)
#'
#' @export
test_equal_rows <- function(df, df2) {
  df_name <- deparse(substitute(df))
  df2_name <- deparse(substitute(df2))

  df_rows <- nrow(df)
  df2_rows <- nrow(df2)

  test_message <- glue::glue("Dataframe [{df_name}] has the same number of rows as dataframe [{df2_name}]", df_name = df_name, df2_name = df2_name)

  testthat::test_that(test_message,
                      testthat::expect_equal(df_rows, df2_rows))

  invisible(df)
}
