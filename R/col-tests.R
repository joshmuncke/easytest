test_column_sum <- function(df, col, sum) {
  df_name <- deparse(substitute(df))
  col_name <- deparse(substitute(col))
  actual_sum <- df %>% summarize(n = sum(!!enquo(col))) %>% pull(n)

  test_message <- glue::glue("Sum of column [{col_name}] in dataframe [{df_name}] == ({sum})", col_name = col_name, df_name = df_name, sum = sum)

  testthat::test_that(test_message,
                      testthat::expect_equal(actual_sum, sum))

  invisible(df)
}

test_column_n_unique_values <- function(df, col, n) {
  df_name <- deparse(substitute(df))
  col_name <- deparse(substitute(col))
  actual_n <- df %>% summarize(n = n_distinct(!!enquo(col))) %>% pull(n)

  test_message <- glue::glue("Number of unique values in column [{col_name}] in dataframe [{df_name}] == ({n})", col_name = col_name, df_name = df_name, n = n)

  testthat::test_that(test_message,
                      testthat::expect_equal(actual_n, n))

  invisible(df)
}

test_column_max <- function(df, col, max) {
  df_name <- deparse(substitute(df))
  col_name <- deparse(substitute(col))
  actual_max <- df %>% summarize(n = max(!!enquo(col))) %>% pull(n)

  test_message <- glue::glue("Max value of column [{col_name}] in dataframe [{df_name}] is <= ({max})", col_name = col_name, df_name = df_name, max = max)

  testthat::test_that(test_message,
                      testthat::expect_lte(actual_max, max))

  invisible(df)
}

test_column_min <- function(df, col, min) {
  df_name <- deparse(substitute(df))
  col_name <- deparse(substitute(col))
  actual_min <- df %>% summarize(n = min(!!enquo(col))) %>% pull(n)

  test_message <- glue::glue("Min value of column [{col_name}] in dataframe [{df_name}] is >= ({min})", col_name = col_name, df_name = df_name, min = min)

  testthat::test_that(test_message,
                      testthat::expect_gte(actual_min, min))

  invisible(df)
}

test_column_between <- function(df, col, min, max) {
  df_name <- deparse(substitute(df))
  col_name <- deparse(substitute(col))
  actual_min <- df %>% summarize(n = min(!!enquo(col))) %>% pull(n)
  actual_max <- df %>% summarize(n = max(!!enquo(col))) %>% pull(n)

  test_message <- glue::glue("Value of column [{col_name}] in dataframe [{df_name}] is ({min}) <= [{col_name}] <= ({max})", col_name = col_name, df_name = df_name, min = min, max = max)

  testthat::test_that(test_message,
                      {testthat::expect_gte(actual_min, min)
                        testthat::expect_lte(actual_max, max)})

  invisible(df)
}
