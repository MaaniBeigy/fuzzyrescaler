context("state_center")
test_that(
  desc = "understands the default value of center = `mean`", {
    x = 1:10
    expect_equal(
      FuzzyRescale$new(x)$center, "mean"
    )
  }
)
test_that(
  desc = "understands the user input of center = `median`", {
    x = 1:10
    expect_equal(
      FuzzyRescale$new(x, center = "median")$center, "median"
    )
  }
)

test_that(
  desc = "understands the user input of center = is.numeric", {
    x = 1:10
    expect_equal(
      FuzzyRescale$new(x, center = 5)$center, 5
    )
  }
)
test_that(
  desc = "correct center handling when x is data.frame", {
    df <- data.frame(
      id = c(1:3, NA),
      gender = c("m", "f", "m", NA),
      fbs = c(104, 98, 129, NA)
    )
    expect_equivalent(
      FuzzyRescale$new(x = df, na.rm = TRUE, center = "mean")$gaussmf(),
      data.frame(
        id = c(0.6, 1.0, 0.6),
        gender = c(1, 0, 1),
        fbs = c(0.9, 0.8, 0.5)
      )
    )
    expect_equivalent(
      FuzzyRescale$new(x = df, na.rm = TRUE, center = "median")$gaussmf(),
      data.frame(
        id = c(0.6, 1.0, 0.6),
        gender = c(1, 0, 1),
        fbs = c(1, 0.9, 0.3)
      )
    )
  }
)
test_that(
  desc = "correct center handling when x is vector", {
    x = 1:10
    expect_equal(
      FuzzyRescale$new(x, center = 5)$gaussmf(),
      c(0.4, 0.6, 0.8, 0.9, 1.0, 0.9, 0.8, 0.6, 0.4, 0.3)
    )
  }
)
test_that(
  desc = "stops if incorrect method for center is used", {
    x = 1:10
    expect_error(
      FuzzyRescale$new(x, center = "mode")$gaussmf()
    )
  }
)
