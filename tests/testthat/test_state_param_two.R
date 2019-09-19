context("state_param_two")
test_that(
  desc = "understands the default value of param_two = `p97.5`", {
    x = 1:10
    expect_equal(
      FuzzyRescale$new(x)$param_two, "p97.5"
    )
  }
)
test_that(
  desc = "understands the user input of param_two = `min`", {
    x = 1:10
    expect_equal(
      FuzzyRescale$new(x, param_two = "min")$param_two, "min"
    )
  }
)
test_that(
  desc = "understands the user input of param_two = is.numeric", {
    x = 1:10
    expect_equal(
      FuzzyRescale$new(x, param_two = 1.5)$param_two, 1.5
    )
  }
)
test_that(
  desc = "stops if incorrect method for param_two is used", {
    x = 1:10
    expect_error(
      FuzzyRescale$new(x, param_two = "upper_bound")$param_two,
      "Incorrect method for param_two"
    )
  }
)
test_that(
  desc = "correct handling of quantile param_two", {
    x = 1:10
    expect_equal(
      FuzzyRescale$new(x, param_two = "p98.5")$param_two, "p98.5"
    )
  }
)
test_that(
  desc = "correct handling of probs for param_two", {
    x = 1:10
    expect_error(
      FuzzyRescale$new(x, param_two = "p1000")$param_two,
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
        param_two = "p100.000000000000000000000000000002"
      )$param_two,
      "p100"
    )
  }
)
