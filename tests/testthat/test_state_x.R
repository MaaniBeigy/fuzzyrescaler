context("state_x")
test_that(
    desc = "error message is thrown when x is NULL", {
        x = NULL
        expect_error(
            QuantileRescale$new(x, na.rm = FALSE)$transform_x(),
            "object 'x' not found"
        )
    }
)
test_that(
    desc = "error message is thrown when x is missing", {
        expect_error(
            QuantileRescale$new(na.rm = FALSE)$transform_x(),
            "object 'x' not found"
        )
    }
)
test_that(
    desc = "error message is thrown when x is atomic but not numeric", {
        y = c("a", "b")
        expect_error(
            QuantileRescale$new(x = y, na.rm = FALSE)$transform_x(),
            "argument is not a numeric vector"
        )
    }
)
test_that(
    desc = "error message is thrown when x is data.frame but has non-numerics",
    {
        df <- data.frame(
            id = c(1:3, NA),
            gender = c("m", "f", "m", NA),
            fbs = c(104, 98, 129, NA)
        )
        expect_error(
            QuantileRescale$new(x = df, na.rm = FALSE)$transform_df(),
            "argument is not a numeric vector"
        )
        expect_error(
            QuantileRescale$new(
                x = df, select = c(2), na.rm = FALSE
                )$transform_df(),
            "argument is not a numeric vector"
        )
    }
)
