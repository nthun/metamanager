context("test-add_columns.R")

test_that("Columns added", {
    expect_true(
        all(
            tibble::has_name(
            add_columns(df = iris, columns = c("x","y"), before = FALSE), c("x", "y"))
            )
    )})
