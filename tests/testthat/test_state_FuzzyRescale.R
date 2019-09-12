context("state_FuzzyRescale")
test_that(
  desc = "finds the correct length of atomic vector x", {
    x = c(
      0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
      4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
    )
    expect_equal(
      FuzzyRescale$new(x)$n(), 20
    )
  }
)
test_that(
  desc = "finds the correct length of matrix x", {
    x = matrix(1:6, ncol = 3, nrow = 2)
    expect_equal(
      FuzzyRescale$new(x)$n(), 6
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
      FuzzyRescale$new(x = df)$n(), 4
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
      FuzzyRescale$new(x)$gaussmf(),
      c(
        0.3, 0.3, 0.5, 0.5, 0.6, 0.8, 0.8, 0.8, 1.0, 1.0,
        1.0, 0.8, 0.8, 0.8, 0.8, 0.7, 0.7, 0.5, 0.4, 0.3
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
      FuzzyRescale$new(x)$gaussmf(),
      c(
        0.3, 0.3, 0.5, 0.5, 0.6, 0.8, 0.8, 0.8, 1.0, 1.0,
        1.0, 0.8, 0.8, 0.8, 0.8, 0.7, 0.7, 0.5, 0.4, 0.3, NA
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
      FuzzyRescale$new(x, select = c(1, 3))$gaussmf(),
      data.frame(
        id = c(0.6, 1.0, 0.6, NA),
        fbs = c(0.9, 0.8, 0.5, NA)
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
      FuzzyRescale$new(
        x, select = c(1, 3), na.rm = TRUE
      )$gaussmf(),
      data.frame(
        id = c(0.6, 1.0, 0.6),
        fbs = c(0.9, 0.8, 0.5)
      )
    )
  }
)
test_that(
  desc = "correct handling of factors in df", {
    x = data.frame(
      id = c(1:3, NA),
      gender = c("m", "f", "m", NA),
      fbs = c(104, 98, 129, NA)
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE
      )$gaussmf(),
      data.frame(
        id = c(0.6, 1.0, 0.6),
        gender = c(1, 0, 1),
        fbs = c(0.9, 0.8, 0.5)
      )
    )
  }
)
test_that(
  desc = "correct handling of factors in vector", {
    x = factor(c("I", "II", "III", "IV", "I", "II"))
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE
      )$gaussmf(),
      c(0.0, 0.3, 0.7, 1.0, 0.0, 0.3)
    )
  }
)
test_that(
  desc = "correct handling of super", {
    x = 1:100
    x_trf = FuzzyRescale$new(
      x, na.rm = TRUE
    )
    expect_equal(
      unname(x_trf$super_$qx()),
      50.5
    )
  }
)
