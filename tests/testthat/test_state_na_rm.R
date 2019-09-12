context("state_na_rm")
test_that(
    desc = "understands the default value of na.rm = FALSE", {
        x = 1:10
        expect_false(
            QuantileRescale$new(x)$na.rm
        )
        expect_false(
            FuzzyRescale$new(x)$na.rm
        )
        expect_equal(
            QuantileRescale$new(x)$x, 1:10
        )
        expect_equal(
            FuzzyRescale$new(x)$x, 1:10
        )
    }
)
test_that(
    desc = "understands the user input of na.rm = TRUE", {
        y = c(1, 2.6, 10, NA, 6.8, NA)
        expect_true(
            QuantileRescale$new(x = y, na.rm = TRUE)$na.rm
        )
        expect_true(
            FuzzyRescale$new(x = y, na.rm = TRUE)$na.rm
        )
        expect_equal(
            QuantileRescale$new(x = y, na.rm = TRUE)$x, c(1, 2.6, 10, 6.8)
        )
        expect_equal(
            FuzzyRescale$new(x = y, na.rm = TRUE)$x, c(1, 2.6, 10, 6.8)
        )
    }
)
test_that(
    desc = "correct missing handling when x is data.frame", {
        df <- data.frame(
            id = c(1:3, NA),
            gender = c("m", "f", "m", NA),
            fbs = c(104, 98, 129, NA)
        )
        expect_equivalent(
            QuantileRescale$new(x = df, na.rm = TRUE)$x,
            data.frame(
                id = c(1:3),
                gender = c("m", "f", "m"),
                fbs = c(104, 98, 129)
            )
        )
        expect_equivalent(
            FuzzyRescale$new(x = df, na.rm = TRUE)$x,
            data.frame(
                id = c(1:3),
                gender = c("m", "f", "m"),
                fbs = c(104, 98, 129)
            )
        )
    }
)
