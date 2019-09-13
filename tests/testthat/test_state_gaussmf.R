context("state_gaussmf")
test_that(
  desc = "gaussmf of vector x when center = `mean` & sigma = `sd`", {
    x = c(
      0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
      4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
    )
    expect_equal(
      FuzzyRescale$new(x, center = "mean", sigma = "sd")$gaussmf(),
      c(
        0.3, 0.3, 0.5, 0.5, 0.6, 0.8, 0.8, 0.8, 1.0, 1.0,
        1.0, 0.8, 0.8, 0.8, 0.8, 0.7, 0.7, 0.5, 0.4, 0.3
      )
    )
  }
)
test_that(
  desc = "gaussmf of vector x when center = `median` & sigma = `IQR`", {
    x = c(
      0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
      4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
    )
    expect_equal(
      FuzzyRescale$new(x, center = "median", sigma = "IQR")$gaussmf(),
      c(
        0.5, 0.5, 0.6, 0.7, 0.8, 0.8, 0.9, 0.9, 1.0, 1.0,
        1.0, 1.0, 1.0, 0.9, 0.9, 0.9, 0.9, 0.8, 0.8, 0.6
      )
    )
  }
)
test_that(
  desc = "gaussmf of vector x when center = `mean` & sigma = `IQR`", {
    x = c(
      0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
      4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
    )
    expect_equal(
      FuzzyRescale$new(x, center = "mean", sigma = "IQR")$gaussmf(),
      c(
        0.6, 0.6, 0.7, 0.8, 0.8, 0.9, 0.9, 0.9, 1.0, 1.0,
        1.0, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.8, 0.7, 0.6
      )
    )
  }
)
test_that(
  desc = "gaussmf of vector x when center = `median` & sigma = `sd`", {
    x = c(
      0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
      4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
    )
    expect_equal(
      FuzzyRescale$new(x, center = "median", sigma = "sd")$gaussmf(),
      c(
        0.2, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.7, 0.9, 1.0,
        1.0, 0.9, 0.9, 0.9, 0.9, 0.8, 0.8, 0.7, 0.5, 0.3
      )
    )
  }
)
test_that(
  desc = "gaussmf of vector x when center & sigma are numeric", {
    x = c(
      0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
      4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
    )
    expect_equal(
      FuzzyRescale$new(x, center = 2.8, sigma = 1.4)$gaussmf(),
      c(
        0.2, 0.3, 0.5, 0.6, 0.8, 0.9, 1.0, 1.0, 0.9, 0.5,
        0.4, 0.2, 0.2, 0.1, 0.1, 0.1, 0.1, 0.0, 0.0, 0.0
      )
    )
  }
)
test_that(
  desc = "gaussmf of vector x when only center is numeric", {
    x = c(
      0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
      4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
    )
    expect_equal(
      FuzzyRescale$new(x, center = 2.8)$gaussmf(),
      c(
        0.5, 0.6, 0.8, 0.8, 0.9, 1.0, 1.0, 1.0, 1.0, 0.8,
        0.7, 0.5, 0.5, 0.5, 0.4, 0.4, 0.4, 0.3, 0.2, 0.1
      )
    )
    expect_equal(
      FuzzyRescale$new(x, center = 2.8, sigma = "IQR")$gaussmf(),
      c(
        0.8, 0.8, 0.9, 0.9, 1.0, 1.0, 1.0, 1.0, 1.0, 0.9,
        0.9, 0.8, 0.8, 0.7, 0.7, 0.7, 0.7, 0.6, 0.5, 0.4
      )
    )
    expect_equal(
      FuzzyRescale$new(x, center = 2.8, sigma = "sd")$gaussmf(),
      c(
        0.5, 0.6, 0.8, 0.8, 0.9, 1.0, 1.0, 1.0, 1.0, 0.8,
        0.7, 0.5, 0.5, 0.5, 0.4, 0.4, 0.4, 0.3, 0.2, 0.1
      )
    )
  }
)
test_that(
  desc = "gaussmf of vector x when only sigma is numeric", {
    x = c(
      0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
      4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
    )
    expect_equal(
      FuzzyRescale$new(x, sigma = 1.4)$gaussmf(),
      c(
        0.0, 0.0, 0.1, 0.2, 0.3, 0.5, 0.5, 0.6, 0.9, 1.0,
        0.9, 0.6, 0.6, 0.5, 0.5, 0.4, 0.4, 0.2, 0.1, 0.0
      )
    )
    expect_equal(
      FuzzyRescale$new(x, sigma = 1.4, center = "mean")$gaussmf(),
      c(
        0.0, 0.0, 0.1, 0.2, 0.3, 0.5, 0.5, 0.6, 0.9, 1.0,
        0.9, 0.6, 0.6, 0.5, 0.5, 0.4, 0.4, 0.2, 0.1, 0.0
      )
    )
    expect_equal(
      FuzzyRescale$new(x, sigma = 1.4, center = "median")$gaussmf(),
      c(
        0.0, 0.0, 0.1, 0.1, 0.2, 0.3, 0.4, 0.4, 0.8, 1.0,
        1.0, 0.8, 0.8, 0.7, 0.6, 0.6, 0.6, 0.3, 0.2, 0.1
      )
    )
  }
)
test_that(
  desc = "gaussmf of df when center = `mean` & sigma = `sd`", {
    x = data.frame(
      id = c(1:3, NA),
      gender = c("m", "f", "m", NA),
      fbs = c(104, 98, 129, NA)
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, center = "mean", sigma = "sd"
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
  desc = "gaussmf of df when center = `median` & sigma = `IQR`", {
    x = data.frame(
      id = c(1:3, NA),
      gender = c("m", "f", "m", NA),
      fbs = c(104, 98, 129, NA)
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, center = "median", sigma = "IQR"
      )$gaussmf(),
      data.frame(
        id = c(0.6, 1.0, 0.6),
        gender = c(1, 0, 1),
        fbs = c(1.0, 0.9, 0.3)
      )
    )
  }
)
test_that(
  desc = "gaussmf of df when center = `mean` & sigma = `IQR`", {
    x = data.frame(
      id = c(1:3, NA),
      gender = c("m", "f", "m", NA),
      fbs = c(104, 98, 129, NA)
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, center = "mean", sigma = "IQR"
      )$gaussmf(),
      data.frame(
        id = c(0.6, 1.0, 0.6),
        gender = c(1, 0, 1),
        fbs = c(0.9, 0.7, 0.5)
      )
    )
  }
)
test_that(
  desc = "gaussmf of df when center = `median` & sigma = `sd`", {
    x = data.frame(
      id = c(1:3, NA),
      gender = c("m", "f", "m", NA),
      fbs = c(104, 98, 129, NA)
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, center = "median", sigma = "sd"
      )$gaussmf(),
      data.frame(
        id = c(0.6, 1.0, 0.6),
        gender = c(1, 0, 1),
        fbs = c(1, 0.9, 0.3)
      )
    )
  }
)
test_that(
  desc = "gaussmf of df when center & sigma are numeric", {
    x = data.frame(
      id = c(1:3, NA),
      gender = c("m", "f", "m", NA),
      fbs = c(104, 98, 129, NA)
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, center = 100, sigma = 10
      )$gaussmf(),
      data.frame(
        id = c(0, 0, 0),
        gender = c(1, 0, 1),
        fbs = c(0.9, 1.0, 0.0)
      )
    )
  }
)
test_that(
  desc = "gaussmf of df when only center is numeric", {
    x = data.frame(
      id = c(1:3, NA),
      gender = c("m", "f", "m", NA),
      fbs = c(104, 98, 129, NA)
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, center = 100
      )$gaussmf(),
      data.frame(
        id = c(0, 0, 0),
        gender = c(1, 0, 1),
        fbs = c(1.0, 1.0, 0.2)
      )
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, center = 100, sigma = "sd"
      )$gaussmf(),
      data.frame(
        id = c(0, 0, 0),
        gender = c(1, 0, 1),
        fbs = c(1.0, 1.0, 0.2)
      )
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, center = 100, sigma = "IQR"
      )$gaussmf(),
      data.frame(
        id = c(0, 0, 0),
        gender = c(1, 0, 1),
        fbs = c(1.0, 1.0, 0.2)
      )
    )
  }
)
test_that(
  desc = "gaussmf of df when only sigma is numeric", {
    x = data.frame(
      id = c(1:3, NA),
      gender = c("m", "f", "m", NA),
      fbs = c(104, 98, 129, NA)
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, sigma = 10
      )$gaussmf(),
      data.frame(
        id = c(1, 1, 1),
        gender = c(1, 0, 1),
        fbs = c(0.8, 0.5, 0.2)
      )
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, sigma = 10, center = "mean"
      )$gaussmf(),
      data.frame(
        id = c(1, 1, 1),
        gender = c(1, 0, 1),
        fbs = c(0.8, 0.5, 0.2)
      )
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, sigma = 10, center = "median"
      )$gaussmf(),
      data.frame(
        id = c(1, 1, 1),
        gender = c(1, 0, 1),
        fbs = c(1.0, 0.8, 0.0)
      )
    )
  }
)
