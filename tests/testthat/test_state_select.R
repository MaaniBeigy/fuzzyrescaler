context("state_select")
test_that(
    desc = "understands the default value of select = NA", {
        x = data.frame(
            id = c(1:3, NA),
            gender = c("m", "f", "m", NA),
            fbs = c(104, 98, 129, NA)
        )
        expect_true(
            is.na(QuantileRescale$new(x)$select)
            )
        expect_equal(
            QuantileRescale$new(x)$x,
            subset(x, select = c(1:ncol(x)))
        )
    }
)
test_that(
    desc = "understands the user input value of select", {
        x = data.frame(
            id = c(1:3, NA),
            gender = c("m", "f", "m", NA),
            fbs = c(104, 98, 129, NA)
        )
        expect_equal(
            QuantileRescale$new(x, select = c(1,3))$select,
            c(1, 3)
        )
        expect_equal(
            QuantileRescale$new(x, select = c(1,3))$x,
            subset(x, select = c(1,3))
        )
    }
)
test_that(
    desc = "correct handling of atomic vectors", {
        x = 1:10
        expect_true(
            is.na(QuantileRescale$new(x)$select)
        )
        expect_equal(
            QuantileRescale$new(x)$x,
            1:10
        )
    }
)
