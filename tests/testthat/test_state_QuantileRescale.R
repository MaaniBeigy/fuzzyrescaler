context("state_QuantileRescale")
test_that(
    desc = "finds the correct length of atomic vector x", {
        x = c(
            0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
            4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
        )
        expect_equal(
            QuantileRescale$new(x)$n(), 20
        )
    }
)
test_that(
    desc = "finds the correct length of matrix x", {
        x = matrix(1:6, ncol = 3, nrow = 2)
        expect_equal(
            QuantileRescale$new(x)$n(), 6
        )
    }
)
test_that(
    desc = "finds the correct length of dataframe x", {
        df = data.frame(
            id = c(1:3, NA),
            gender = c("m", "f", "m", NA),
            fbs = c(104, 98, 129, NA)
        )
        expect_equal(
            QuantileRescale$new(x = df)$n(), 4
        )
    }
)
test_that(
    desc = "finds the correct transform of atomic vector x", {
        x = c(
            0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
            4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
        )
        expect_equal(
            unname(QuantileRescale$new(x)$quant1()$qx()), 7.5
        )
        expect_equal(
            unname(QuantileRescale$new(x)$quant2()$qx()), 0.3
        )
        expect_equal(
            QuantileRescale$new(x)$transform_x(),
            c(
                0.0, 0.0, 0.1, 0.2, 0.2, 0.3, 0.3, 0.3, 0.4, 0.6, 0.6, 0.7, 0.7,
                0.8, 0.8, 0.8, 0.8, 0.9, 0.9, 1.0
            )
        )
    }
)
test_that(
    desc = "finds the correct transform of atomic vector x 2", {
        x = c(
            0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
            4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9, NA
        )
        expect_equal(
            unname(QuantileRescale$new(x)$quant1()$qx()), 7.5
        )
        expect_equal(
            unname(QuantileRescale$new(x)$quant2()$qx()), 0.3
        )
        expect_equal(
            QuantileRescale$new(x)$transform_x(),
            c(
                0.0, 0.0, 0.1, 0.2, 0.2, 0.3, 0.3, 0.3, 0.4, 0.6, 0.6, 0.7, 0.7,
                0.8, 0.8, 0.8, 0.8, 0.9, 0.9, 1.0, NA
            )
        )
    }
)
test_that(
    desc = "finds the correct transform of data frame x", {
        x = data.frame(
            id = c(1:3, NA),
            gender = c("m", "f", "m", NA),
            fbs = c(104, 98, 129, NA)
        )
        expect_equal(
            unname(QuantileRescale$new(x, select = c(1, 3))$quant1()),
            c(3.0, 127.8)
        )
        expect_equal(
            unname(QuantileRescale$new(x, select = c(1, 3))$quant2()),
            c(1.1, 98.3)
        )
        expect_equal(
            QuantileRescale$new(x, select = c(1, 3))$transform_df(),
            data.frame(
                id = c(0.0, 0.5, 1, NA),
                fbs = c(0.2, 0.0, 1.0, NA)
            )
        )
    }
)
test_that(
    desc = "finds the correct transform of data frame x 2", {
        x = data.frame(
            id = c(1:3, NA),
            gender = c("m", "f", "m", NA),
            fbs = c(104, 98, 129, NA)
        )
        expect_equal(
            unname(QuantileRescale$new(
                x, select = c(1, 3), na.rm = TRUE
                )$quant1()),
            c(3.0, 127.8)
        )
        expect_equal(
            unname(QuantileRescale$new(
                x, select = c(1, 3), na.rm = TRUE
                )$quant2()),
            c(1.1, 98.3)
        )
        expect_equal(
            QuantileRescale$new(
                x, select = c(1, 3), na.rm = TRUE
                )$transform_df(),
            data.frame(
                id = c(0.0, 0.5, 1),
                fbs = c(0.2, 0.0, 1.0)
            )
        )
    }
)
test_that(
    desc = "error message is thrown when transform_x function is incorrect", {
        x = data.frame(
            id = c(1:3, NA),
            gender = c("m", "f", "m", NA),
            fbs = c(104, 98, 129, NA)
        )
        expect_error(
            QuantileRescale$new(x, select = c(1, 3))$transform_x(),
            "x is not an atomic vector"
        )
    }
)
test_that(
    desc = "error message is thrown when transform_df function is incorrect", {
        x = 1:100
        expect_error(
            QuantileRescale$new(x, select = c(1, 3))$transform_df(),
            "x is not a data frame"
        )
    }
)
