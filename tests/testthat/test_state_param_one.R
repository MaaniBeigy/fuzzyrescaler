context("state_param_one")
test_that(
  desc = "understands the default value of param_one = `p2.5`", {
    x = 1:10
    expect_equal(
      FuzzyRescale$new(x)$param_one, "p2.5"
    )
  }
)
test_that(
  desc = "understands the user input of param_one = `min`", {
    x = 1:10
    expect_equal(
      FuzzyRescale$new(x, param_one = "min")$param_one, "min"
    )
  }
)
test_that(
  desc = "understands the user input of param_one = is.numeric", {
    x = 1:10
    expect_equal(
      FuzzyRescale$new(x, param_one = 1.5)$param_one, 1.5
    )
  }
)
test_that(
  desc = "stops if incorrect method for param_one is used", {
    x = 1:10
    expect_error(
      FuzzyRescale$new(x, param_one = "upper_bound")$param_one,
      "Incorrect method for param_one"
    )
  }
)
test_that(
  desc = "correct handling of quantile param_one", {
    x = 1:10
    expect_equal(
      FuzzyRescale$new(x, param_one = "p98.5")$param_one, "p98.5"
    )
  }
)
test_that(
  desc = "correct handling of probs for param_one", {
    x = 1:10
    expect_error(
      FuzzyRescale$new(x, param_one = "p1000")$param_one,
      "probs outside \\[0\\,100\\]"
    )
  }
)
test_that(
  desc = "understands the tolerance of probs above 1 and 1+2e-14", {
    x = 1:10
    expect_equal(
      FuzzyRescale$new(
        x,
        param_one = "p100.000000000000000000000000000002"
      )$param_one,
      "p100"
    )
  }
)
