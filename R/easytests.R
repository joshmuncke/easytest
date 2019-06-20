test_no_duplicates <- function(df, ...) {

  df <- df %>% dplyr::select(!!!enquo(...))

  testthat::test_that(paste0("No duplicates in dataframe [", deparse(substitute(mc)), "]"),
                      testthat::expect_identical(df, dplyr::distinct(df)))
}

test_expected_rows <- function(df, expected_rows) {
  testthat::test_that(paste0("Number of rows in dataframe [", deparse(substitute(mc)), "] is (", expected_rows,")"),
                      testthat::expect_equal(df %>% nrow(), expected_rows))
}

test_complete <- function(df) {
  testthat::test_that(paste0("Dataframe [", deparse(substitute(mc)), "] is complete"),
                      testthat::expect_identical(df, df %>% dplyr::filter(complete.cases(.))))
}

test_column_sum <- function(df, col, expected_sum) {
  testthat::test_that(paste0("Sum of column [", deparse(substitute(col)) ,"] in dataframe [", deparse(substitute(mc)), "] is (", expected_sum,")"),
                      testthat::expect_equal(df %>% summarize(n = sum(!!enquo(col))) %>% pull(n), expected_sum))
}

test_column_unique_vals <- function(df, col, expected_unique_vals) {
  testthat::test_that(paste0("Number of unique values in column [", deparse(substitute(col)) ,"] in dataframe [", deparse(substitute(mc)), "] is (", expected_unique_vals,")"),
                      testthat::expect_equal(df %>% summarize(n = n_distinct(!!enquo(col))) %>% pull(n), expected_unique_vals))
}

test_same_as_other_df <- function(df, df2) {
  testthat::test_that(paste0("Sum of column [", deparse(substitute(col)) ,"] in dataframe [", deparse(substitute(mc)), "] is (", expected_sum,")"),
                      testthat::expect_equal(df %>% summarize(n = sum(!!enquo(col))) %>% pull(n), expected_sum))
}
