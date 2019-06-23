
<!-- README.md is generated from README.Rmd. Please edit that file -->
easytest
========

<!-- <img src="man/figures/logo.png" align="right" /> -->
<!-- badges: start -->
<!-- badges: end -->
Overview
--------

I will admit it - I'm lazy. **easytest** is for people like me.

I *want* to incorporate great unit tests throughout my analyses using [**testthat**](https://github.com/r-lib/testthat) but often the additional effort to write out something like this prevents from being as thorough as I could/should be.

``` r
library(testthat)

### SOME OPERATION ON my_data_frame

testthat::test_that("I didn't accidentally create any duplicates in my dataframe",
                    expect_equal(distinct(my_data_frame), my_data_frame))
```

The thing is - testthat is a very robust library that provides testing tools for all manner of situations including package development.

What I found is that during the course of an *analysis* that I was typically doing the same kinds of tests again and again: checking for duplicates, verifying row counts, summing up columns etc; I didn't really need the full power and flexibility that testthat provides.

The goal of easytest is to provide make testing in these kinds of scenarios so painlessly easy that you almost forget **not** to do it:

``` r
library(easytest)

### SOME OPERATION ON my_data_frame

my_data_frame %>% test_no_duplicates()
```

Voilà! We have essentially done the exact same thing as in the chunk above but the named function and pipe-ability makes it practically seamless. And less friction means more testing means - hopefully - better code.

Installation
------------

You can install `easytest` from Github using the following command:

``` r
# You must have devtools installed first
devtools::install_github("joshmuncke/easytest")
```

Usage
-----

All easytest tests have the `test_` naming convention and take a dataframe as the first argument for ease of pipe-ing using magrittr. They silently return the same dataframe so you can utilize them within pipe chains.

``` r
mtcars %>%
  test_expected_rows(32) %>%
  summarize(averagedisp = mean(disp))
```

### Available tests

The available tests reflect the types of tests I find myself doing most frequently during the course of an analysis. If you feel that a particularly useful test function should be added - please feel free to raise an issue or submit a PR!

#### Dataframe tests

-   **test\_no\_duplicates:** Test that there are no duplicates in the dataframe. If column names are provided then only these subset of columns will be checked for duplicates.
-   **test\_complete:** Test that there are no missing values in the dataframe. Column names can also be provided to limit the test scope.
-   **test\_n\_rows:** Test that the dataframe has an expected number of rows.
-   **test\_same\_as\_other\_df:** Test that the dataframe is the same as another.
-   **test\_equal\_rows:** Test that the dataframe has the same number of rows as another (even if the content is different).

#### Column tests

-   **test\_column\_sum:** Test that the sum of a specific column in the dataframe equals a specific number.
-   **test\_column\_n\_unique\_values:** Test that there are a specific number of unique values in the column.
-   **test\_column\_max:** Test that the maximum value of a column does not exceed a specified value.
-   **test\_column\_min:** Test that the minimum value of a column is not less than specified value.
-   **test\_column\_between:** Test that all values of a column are within an expected range.
-   **test\_column\_mean:** Test that the mean value of a column has a specified value (within a certain number of standard deviations).
