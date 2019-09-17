context("state_sigmf")
test_that(
  desc = "sigmf of vector x when center = `mean` & sigma = `sd`", {
    x = c(
      0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
      4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
    )
    expect_equal(
      FuzzyRescale$new(x, center = "mean", sigma = "sd", digits = 5)$sigmf(),
      c(
        0.00013, 0.00026, 0.00105, 0.00210, 0.00533, 0.01694, 0.02674, 0.04198,
        0.22091, 0.69852, 0.78702, 0.95986, 0.95986, 0.97966, 0.98382, 0.98715,
        0.98980, 0.99747, 0.99921, 0.99988
      )
    )
  }
)
test_that(
  desc = "sigmf of vector x when center = `median` & sigma = `IQR`", {
    x = c(
      0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
      4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
    )
    expect_equal(
      FuzzyRescale$new(x, center = "median", sigma = "IQR", digits = 5)$sigmf(),
      c(
        0.00000, 0.00000, 0.00000, 0.00001, 0.00005, 0.00033, 0.00068, 0.00140,
        0.02533, 0.40975, 0.59025, 0.96391, 0.96391, 0.98763, 0.99138, 0.99400,
        0.99583, 0.99953, 0.99992, 1.00000
      )
    )
  }
)
test_that(
  desc = "sigmf of vector x when center = `mean` & sigma = `IQR`", {
    x = c(
      0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
      4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
    )
    expect_equal(
      FuzzyRescale$new(x, center = "mean", sigma = "IQR", digits = 5)$sigmf(),
      c(
        0.00000, 0.00000, 0.00002, 0.00007, 0.00028, 0.00174, 0.00361, 0.00746,
        0.12228, 0.78818, 0.88534, 0.99306, 0.99306, 0.99767, 0.99838, 0.99888,
        0.99922, 0.99991, 0.99999, 1.00000
      )
    )
  }
)
test_that(
  desc = "sigmf of vector x when center = `median` & sigma = `sd`", {
    x = c(
      0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
      4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
    )
    expect_equal(
      FuzzyRescale$new(x, center = "median", sigma = "sd", digits = 5)$sigmf(),
      c(
        0.00004, 0.00009, 0.00036, 0.00072, 0.00183, 0.00585, 0.00930, 0.01476,
        0.08834, 0.44191, 0.55809, 0.89097, 0.89097, 0.94272, 0.95410, 0.96330,
        0.97072, 0.99262, 0.99769, 0.99964
      )
    )
  }
)
test_that(
  desc = "sigmf of vector x when center & sigma are numeric", {
    x = c(
      0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
      4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
    )
    expect_equal(
      FuzzyRescale$new(x, center = 2.8, sigma = -1.4, digits = 5)$sigmf(),
      c(
        0.97442, 0.96158, 0.91529, 0.87653, 0.80218, 0.66819, 0.60348, 0.53494,
        0.27289, 0.09622, 0.07447, 0.02558, 0.02558, 0.01696, 0.01477, 0.01287,
        0.01121, 0.00487, 0.00242, 0.00079
      )
    )
  }
)
test_that(
  desc = "sigmf of vector x when only center is numeric", {
    x = c(
      0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
      4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
    )
    expect_equal(
      FuzzyRescale$new(x, center = 2.8, digits = 5)$sigmf(),
      c(
        0.00231, 0.00464, 0.01856, 0.03670, 0.08834, 0.23739, 0.33176, 0.44191,
        0.83669, 0.97667, 0.98524, 0.99769, 0.99769, 0.99885, 0.99909, 0.99928,
        0.99943, 0.99986, 0.99996, 0.99999
      )
    )
    expect_equal(
      FuzzyRescale$new(x, center = 2.8, sigma = "IQR", digits = 5)$sigmf(),
      c(
        0.00008, 0.00023, 0.00202, 0.00600, 0.02533, 0.13883, 0.25068, 0.40975,
        0.92791, 0.99710, 0.99860, 0.99992, 0.99992, 0.99997, 0.99998, 0.99999,
        0.99999, 1.00000, 1.00000, 1.00000
      )
    )
    expect_equal(
      FuzzyRescale$new(x, center = 2.8, sigma = "sd", digits = 5)$sigmf(),
      c(
        0.00231, 0.00464, 0.01856, 0.03670, 0.08834, 0.23739, 0.33176, 0.44191,
        0.83669, 0.97667, 0.98524, 0.99769, 0.99769, 0.99885, 0.99909, 0.99928,
        0.99943, 0.99986, 0.99996, 0.99999
      )
    )
  }
)
test_that(
  desc = "sigmf of vector x when only sigma is numeric", {
    x = c(
      0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
      4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
    )
    expect_equal(
      FuzzyRescale$new(x, sigma = 1.4, digits = 5)$sigmf(),
      c(
        0.00460, 0.00699, 0.01605, 0.02422, 0.04165, 0.08047, 0.10377, 0.13285,
        0.31952, 0.62340, 0.68654, 0.87034, 0.87034, 0.91085, 0.92158, 0.93112,
        0.93957, 0.97299, 0.98640, 0.99552
      )
    )
    expect_equal(
      FuzzyRescale$new(x, sigma = 1.4, center = "mean", digits = 5)$sigmf(),
      c(
        0.00460, 0.00699, 0.01605, 0.02422, 0.04165, 0.08047, 0.10377, 0.13285,
        0.31952, 0.62340, 0.68654, 0.87034, 0.87034, 0.91085, 0.92158, 0.93112,
        0.93957, 0.97299, 0.98640, 0.99552
      )
    )
    expect_equal(
      FuzzyRescale$new(x, sigma = 1.4, center = "median", digits = 5)$sigmf(),
      c(
        0.00242, 0.00368, 0.00849, 0.01287, 0.02231, 0.04394, 0.05732, 0.07447,
        0.19782, 0.46506, 0.53494, 0.77903, 0.77903, 0.84290, 0.86057, 0.87653, 0.89090, 0.94979, 0.97442, 0.99151
      )
    )
  }
)
test_that(
  desc = "sigmf of df when center = `mean` & sigma = `sd`", {
    x = data.frame(
      id = c(1:3, NA),
      gender = c("m", "f", "m", NA),
      fbs = c(104, 98, 129, NA)
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, center = "mean", sigma = "sd", digits = 5
      )$sigmf(),
      data.frame(
        id = c(0.26894, 0.50000, 0.73106),
        gender = c(1, 0, 1),
        fbs = c(0, 0, 1)
      )
    )
  }
)
test_that(
  desc = "sigmf of df when center = `median` & sigma = `IQR`", {
    x = data.frame(
      id = c(1:3, NA),
      gender = c("m", "f", "m", NA),
      fbs = c(104, 98, 129, NA)
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, center = "median", sigma = "IQR", digits = 5
      )$sigmf(),
      data.frame(
        id = c(0.26894, 0.50000, 0.73106),
        gender = c(1, 0, 1),
        fbs = c(0.5, 0.0, 1.0)
      )
    )
  }
)
test_that(
  desc = "sigmf of df when center = `mean` & sigma = `IQR`", {
    x = data.frame(
      id = c(1:3, NA),
      gender = c("m", "f", "m", NA),
      fbs = c(104, 98, 129, NA)
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, center = "mean", sigma = "IQR", digits = 5
      )$sigmf(),
      data.frame(
        id = c(0.26894, 0.50000, 0.73106),
        gender = c(1, 0, 1),
        fbs = c(0, 0, 1)
      )
    )
  }
)
test_that(
  desc = "sigmf of df when center = `median` & sigma = `sd`", {
    x = data.frame(
      id = c(1:3, NA),
      gender = c("m", "f", "m", NA),
      fbs = c(104, 98, 129, NA)
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, center = "median", sigma = "sd", digits = 5
      )$sigmf(),
      data.frame(
        id = c(0.26894, 0.50000, 0.73106),
        gender = c(1, 0, 1),
        fbs = c(0.5, 0.0, 1.0)
      )
    )
  }
)
test_that(
  desc = "sigmf of df when center & sigma are numeric", {
    x = data.frame(
      id = c(1:3, NA),
      gender = c("m", "f", "m", NA),
      fbs = c(104, 98, 129, NA)
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, center = 100, sigma = 10, digits = 5
      )$sigmf(),
      data.frame(
        id = c(0, 0, 0),
        gender = c(1, 0, 1),
        fbs = c(1, 0, 1)
      )
    )
  }
)
test_that(
  desc = "sigmf of df when only center is numeric", {
    x = data.frame(
      id = c(1:3, NA),
      gender = c("m", "f", "m", NA),
      fbs = c(104, 98, 129, NA)
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, center = 100, digits = 5
      )$sigmf(),
      data.frame(
        id = c(0, 0, 0),
        gender = c(1, 0, 1),
        fbs = c(1, 0, 1)
      )
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, center = 100, sigma = "sd", digits = 5
      )$sigmf(),
      data.frame(
        id = c(0, 0, 0),
        gender = c(1, 0, 1),
        fbs = c(1, 0, 1)
      )
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, center = 100, sigma = "IQR", digits = 5
      )$sigmf(),
      data.frame(
        id = c(0, 0, 0),
        gender = c(1, 0, 1),
        fbs = c(1, 0, 1)
      )
    )
  }
)
test_that(
  desc = "sigmf of df when only sigma is numeric", {
    x = data.frame(
      id = c(1:3, NA),
      gender = c("m", "f", "m", NA),
      fbs = c(104, 98, 129, NA)
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, sigma = 10, digits = 5
      )$sigmf(),
      data.frame(
        id = c(0.00005, 0.50000, 0.99995),
        gender = c(1, 0, 1),
        fbs = c(0, 0, 1)
      )
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, sigma = 10, center = "mean", digits = 5
      )$sigmf(),
      data.frame(
        id = c(0.00005, 0.50000, 0.99995),
        gender = c(1, 0, 1),
        fbs = c(0, 0, 1)
      )
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = TRUE, sigma = 10, center = "median", digits = 5
      )$sigmf(),
      data.frame(
        id = c(0.00005, 0.50000, 0.99995),
        gender = c(1, 0, 1),
        fbs = c(0.5, 0.0, 1.0)
      )
    )
  }
)
test_that(
  desc = "sigmf correctly handles missings in vectors", {
    y = c(1, 2.6, 10, NA, 6.8, NA)
    expect_equal(
      FuzzyRescale$new(
        x = y, center = "mean", sigma = "sd", digits = 5
        )$sigmf(),
      c(
        0.00000, 0.00004, 1.00000, NA, 0.99903, NA
      )
    )
  }
)
test_that(
  desc = "sigmf correctly handles factors", {
    x = factor(c("I", "II", "III", "IV", "I", "II"))
    expect_equal(
      FuzzyRescale$new(
        x, center = "mean", sigma = "sd", digits = 5
      )$sigmf(),
      c(
        0.00000, 0.33333, 0.66667, 1.00000, 0.00000, 0.33333
      )
    )
  }
)

test_that(
  desc = "sigmf correctly handles missings in data frames`", {
    x = data.frame(
      id = c(1:3, NA),
      gender = c("m", "f", "m", NA),
      fbs = c(104, 98, 129, NA)
    )
    expect_equal(
      FuzzyRescale$new(
        x, na.rm = FALSE, center = "median", sigma = "sd"
      )$sigmf(),
      data.frame(
        id = c(0.3, 0.5, 0.7, NA),
        gender = c(1, 0, 1, NA),
        fbs = c(0.5, 0.0, 1.0, NA)
      )
    )
  }
)
