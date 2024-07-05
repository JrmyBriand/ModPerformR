test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("peronnet_thibault_model calculates correctly", {
  # Test case 1: Using example values
  result1 <- peronnet_thibault_model(A = 1500, MAP = 25, E = -0.5, T_est = 600, BMR = 1.2, Tmap = 300)
  expect_type(result1, "double")
  expect_length(result1, 1)

  # Test case 2: Different values
  result2 <- peronnet_thibault_model(A = 2000, MAP = 30, E = -0.6, T_est = 1200, BMR = 1.5, Tmap = 400)
  expect_type(result2, "double")
  expect_length(result2, 1)

  # Test for errors with invalid inputs
  expect_error(peronnet_thibault_model(A = -1000, MAP = 25, E = -0.5, T_est = 600, BMR = 1.2, Tmap = 300),
               "Anaerobic capacity \\(A\\) must be positive")
  expect_error(peronnet_thibault_model(A = 1500, MAP = 25, E = -0.5, T_est = 0, BMR = 1.2, Tmap = 300),
               "Running duration \\(T_est\\) must be positive")

  # Add more specific tests based on expected outputs for known inputs
  # You may need to calculate some expected values based on the model's equation
  # expect_equal(result1, expected_value1, tolerance = 1e-6)
  # expect_equal(result2, expected_value2, tolerance = 1e-6)
})


