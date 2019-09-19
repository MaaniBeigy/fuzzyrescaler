context("state_sigma")
test_that(
  desc = "understands the default value of sigma = `sd`", {
    x = 1:10
    expect_equal(
      FuzzyRescale$new(x)$sigma, "sd"
    )
  }
)
test_that(
  desc = "understands the user input of sigma = `IQR`", {
    x = 1:10
    expect_equal(
      FuzzyRescale$new(x, sigma = "IQR")$sigma, "IQR"
    )
  }
)

test_that(
  desc = "understands the user input of sigma = is.numeric", {
    x = 1:10
    expect_equal(
      FuzzyRescale$new(x, sigma = 1.5)$sigma, 1.5
    )
  }
)
test_that(
  desc = "correct sigma handling when x is data.frame", {
    df <- data.frame(
      id = c(1:3, NA),
      gender = c("m", "f", "m", NA),
      fbs = c(104, 98, 129, NA)
    )
    expect_equivalent(
      FuzzyRescale$new(x = df, na.rm = TRUE, sigma = "sd")$gaussmf(),
      data.frame(
        id = c(0.6, 1.0, 0.6),
        gender = c(1, 0, 1),
        fbs = c(0.9, 0.8, 0.5)
      )
    )
    expect_equivalent(
      FuzzyRescale$new(x = df, na.rm = TRUE, sigma = "IQR")$gaussmf(),
      data.frame(
        id = c(0.6, 1.0, 0.6),
        gender = c(1, 0, 1),
        fbs = c(0.9, 0.7, 0.5)
      )
    )
  }
)
test_that(
  desc = "correct sigma handling when x is vector", {
    x = 1:10
    expect_equal(
      FuzzyRescale$new(x, sigma = 3)$gaussmf(),
      c(0.3, 0.5, 0.7, 0.9, 1.0, 1.0, 0.9, 0.7, 0.5, 0.3)
    )
  }
)
test_that(
  desc = "stops if incorrect method for sigma is used", {
    x = 1:10
    expect_error(
      FuzzyRescale$new(x, sigma = "MAD")$gaussmf(),
      "Incorrect method for sigma"
    )
  }
)
