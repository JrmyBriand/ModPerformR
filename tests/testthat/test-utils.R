test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("di_prampero_running calculates correctly", {
  expect_equal(di_prampero_running(velocity = 5, distance = 1000), 326.9, tolerance = 0.1)
  expect_equal(di_prampero_running(velocity = 6, distance = 5000, BM = 60, BSA = 1.7), 322.7328, tolerance = 0.1)
  expect_error(di_prampero_running(velocity = -1, distance = 1000), "Velocity must be positive")
  expect_error(di_prampero_running(velocity = 5, distance = 0), "Distance must be positive")
})

test_that("inv_di_prampero_running calculates correctly", {
  # Test with default parameters
  expect_equal(
    inv_di_prampero_running(Power = 300, t = 600),
    3000,
    tolerance = 100  # Allow some tolerance due to numerical optimization
  )

  # Test with custom parameters
  expect_equal(
    inv_di_prampero_running(Power = 400, t = 300, BM = 80, BSA = 2),
    1800,
    tolerance = 100
  )

  # Test that increasing power increases distance
  dist1 <- inv_di_prampero_running(Power = 300, t = 600)
  dist2 <- inv_di_prampero_running(Power = 400, t = 600)
  expect_gt(dist2, dist1)

  # Test that increasing time increases distance
  dist3 <- inv_di_prampero_running(Power = 300, t = 600)
  dist4 <- inv_di_prampero_running(Power = 300, t = 900)
  expect_gt(dist4, dist3)

  # Test error handling
  expect_error(inv_di_prampero_running(Power = -100, t = 600), "Power must be positive")
  expect_error(inv_di_prampero_running(Power = 300, t = -10), "Running duration \\(t\\) must be positive")
  expect_error(inv_di_prampero_running(Power = 300, t = 600, BM = -70), "Body mass \\(BM\\) must be positive")
  expect_error(inv_di_prampero_running(Power = 300, t = 600, eff = 1.5), "Efficiency \\(eff\\) must be between 0 and 1")

  # Test that the function is consistent with di_prampero_running
  power <- 300
  time <- 600
  distance <- inv_di_prampero_running(Power = power, t = time)
  velocity <- distance / time
  calculated_power <- di_prampero_running(velocity = velocity, distance = distance)
  expect_equal(power, calculated_power, tolerance = 1)  # Allow 1W tolerance due to numerical precision
})

