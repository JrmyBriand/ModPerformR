test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("peronnet_thibault_model calculates correctly", {
  # Test case 1: Using example values
  result1 <- peronnet_thibault_model(A = 1500, MAP = 25, E = -0.5, t = 600, BMR = 1.2, Tmap = 300)
  expect_type(result1, "double")
  expect_length(result1, 1)

  # Test case 2: Different values
  result2 <- peronnet_thibault_model(A = 2000, MAP = 30, E = -0.6, t = 1200, BMR = 1.5, Tmap = 400)
  expect_type(result2, "double")
  expect_length(result2, 1)

  # Test for errors with invalid inputs
  expect_error(peronnet_thibault_model(A = -1000, MAP = 25, E = -0.5, t = 600, BMR = 1.2, Tmap = 300),
               "Anaerobic capacity \\(A\\) must be positive")
  expect_error(peronnet_thibault_model(A = 1500, MAP = 25, E = -0.5, t = 0, BMR = 1.2, Tmap = 300),
               "Running duration \\(t\\) must be positive")

  # Add more specific tests based on expected outputs for known inputs
  # You may need to calculate some expected values based on the model's equation
  # expect_equal(result1, expected_value1, tolerance = 1e-6)
  # expect_equal(result2, expected_value2, tolerance = 1e-6)
})

test_that("critical_power_model works correctly", {
  expect_equal(critical_power_model(t = 300, cp = 300, Wprime = 20000), 366.6667, tolerance = 1e-4)
  expect_error(critical_power_model(t = 0, cp = 300, Wprime = 20000), "Running duration \\(t\\) must be positive")
  expect_error(critical_power_model(t = 300, cp = 0, Wprime = 20000), "Critical power \\(cp\\) must be positive")
  expect_error(critical_power_model(t = 300, cp = 300, Wprime = 0), "Anaerobic energy reserve \\(Wprime\\) must be positive")
  expect_error(critical_power_model(t = -1, cp = 300, Wprime = 20000), "Running duration \\(t\\) must be positive")
  expect_error(critical_power_model(t = 300, cp = -1, Wprime = 20000), "Critical power \\(cp\\) must be positive")
  expect_error(critical_power_model(t = 300, cp = 300, Wprime = -1), "Anaerobic energy reserve \\(Wprime\\) must be positive")
})

test_that("critical_power_model_3p works correctly", {
  expect_equal(critical_power_model_3p(t = 300, cp = 300, Wprime = 20000, Pmax = 1000), 338.0952, tolerance = 1e-4)
  expect_error(critical_power_model_3p(t = 0, cp = 300, Wprime = 20000, Pmax = 1000), "Running duration \\(t\\) must be positive")
  expect_error(critical_power_model_3p(t = 300, cp = 0, Wprime = 20000, Pmax = 1000), "Critical power \\(cp\\) must be positive")
  expect_error(critical_power_model_3p(t = 300, cp = 300, Wprime = 0, Pmax = 1000), "Anaerobic energy reserve \\(Wprime\\) must be positive")
  expect_error(critical_power_model_3p(t = 300, cp = 300, Wprime = 20000, Pmax = 0), "Maximal instantaneous power \\(Pmax\\) must be positive")
  expect_error(critical_power_model_3p(t = 300, cp = 300, Wprime = 20000, Pmax = 300), "Maximal instantaneous power \\(Pmax\\) must be greater than critical power \\(cp\\)")
})

test_that("Kennelly models work correctly", {
  expect_equal(Kennelly_1906_V(1000, 10), 4.216965, tolerance = 1e-6)
  expect_error(Kennelly_1906_V(-1000, 10), "Distance must be positive")
  expect_error(Kennelly_1906_V(1000, -10), "Ck must be positive")

  expect_equal(Kennelly_1906_P(1000, 10),  257.583, tolerance = 1e-6)
  expect_type(Kennelly_1906_P(1000, 10), "double")
  expect_error(Kennelly_1906_P(-1000, 10), "Distance must be positive")
  expect_error(Kennelly_1906_P(1000, -10), "Ck must be positive")
})

test_that("Riegel models work correctly", {
  expect_equal(Riegel_time(1000, 2.5, 1.06), 3783.903, tolerance = 1e-6)
  expect_error(Riegel_time(-1000, 2.5, 1.06), "Distance must be positive")
  expect_error(Riegel_time(1000, -2.5, 1.06), "a must be positive")
  expect_error(Riegel_time(1000, 2.5, -1.06), "b must be positive")

  expect_equal(Riegel_velocity(1000, 2.5, 1.06),0.2642774, tolerance = 1e-6)
  expect_type(Riegel_velocity(1000, 10, 1.06), "double")
  expect_error(Riegel_velocity(-1000, 10, 1.06), "Distance must be positive")
  expect_error(Riegel_velocity(1000, -10, 1.06), "a must be positive")
  expect_error(Riegel_velocity(1000, 10, -1.06), "b must be positive")

  expect_equal(Riegel_power(1000, 2.5, 1.06), 31.08472, tolerance = 1e-6)
  expect_type(Riegel_power(1000, 10, 1.06), "double")
  expect_error(Riegel_power(-1000, 10, 1.06), "Distance must be positive")
  expect_error(Riegel_power(1000, -10, 1.06), "a must be positive")
  expect_error(Riegel_power(1000, 10, -1.06), "b must be positive")
})

test_that("Rumball models work correctly", {
  expect_type(Rumball_velocity(1000, 0.2, 2), "double")
  expect_error(Rumball_velocity(-1000, 0.2, 2), "Distance must be positive")
  expect_error(Rumball_velocity(1000, -0.2, 2), "Fatigue coefficient \\(f\\) must be positive")

  expect_type(Rumball_power(1000, 0.2, 2), "double")
  expect_error(Rumball_power(1000, -0.2, 2), "Fatigue coefficient \\(f\\) must be positive")
})

test_that("Lloyd model works correctly", {
  expect_type(Lloyd(25, 0.05, 20, 10, 100), "double")
  expect_error(Lloyd(-25, 0.05, 20, 10, 100), "Pmax must be positive")
  expect_error(Lloyd(25, -0.05, 20, 10, 100), "g must be positive")
  expect_error(Lloyd(25, 0.05, -20, 10, 100), "MAP must be positive")
  expect_error(Lloyd(25, 0.05, 20, -10, 100), "k must be positive")
  expect_error(Lloyd(25, 0.05, 20, 10, -100), "Time must be positive")
})

test_that("Ward-Smith model works correctly", {
  expect_type(WardSmith(25, 20, 0.05, 100), "double")
  expect_error(WardSmith(-25, 20, 0.05, 100), "Pmax must be positive")
  expect_error(WardSmith(25, -20, 0.05, 100), "MAP must be positive")
  expect_error(WardSmith(25, 20, -0.05, 100), "lambda must be positive")
  expect_error(WardSmith(25, 20, 0.05, -100), "Time must be positive")
})

# Test OmniPD model

test_that("OmniPD model works correctly", {
  expect_type(OmPD(Pmax = 25, Wprime = 20000, CP = 300, A = 0.05, Time = 1800), "double")
  expect_error(OmPD(Pmax = -25, Wprime = 20000, CP = 300, A = 0.05, Time = 1800), "Pmax must be positive")
  expect_error(OmPD(Pmax = 25, Wprime = -20000, CP = 300, A = 0.05, Time = 1800), "Wprime must be positive")
  expect_error(OmPD(Pmax = 25, Wprime = 20000, CP = -300, A = 0.05, Time = 1800), "CP must be positive")
  expect_error(OmPD(Pmax = 25, Wprime = 20000, CP = 300, A = 0.05, Time = -1800), "Time must be positive")

  # Test transition at 30 minutes
  result_before <- OmPD(Pmax = 25, Wprime = 20000, CP = 300, A = 0.05, Time = 29 * 60)
  result_after <- OmPD(Pmax = 25, Wprime = 20000, CP = 300, A = 0.05, Time = 31 * 60)
  expect_lt(result_after, result_before)
})

#Tests Roy and Joyner model

test_that("Roy and Joyner model works correctly", {
  expect_type(model_roy(S = 0.1, V0 = 12, B = 2, b = 1.5, d = 200, Distance = 1000), "double")
  expect_error(model_roy(S = -0.1, V0 = 12, B = 2, b = 1.5, d = 200, Distance = 1000), "S must be positive")
  expect_error(model_roy(S = 0.1, V0 = -12, B = 2, b = 1.5, d = 200, Distance = 1000), "V0 must be positive")
  expect_error(model_roy(S = 0.1, V0 = 12, B = -2, b = 1.5, d = 200, Distance = 1000), "B must be positive")
  expect_error(model_roy(S = 0.1, V0 = 12, B = 2, b = -1.5, d = 200, Distance = 1000), "b must be positive")
  expect_error(model_roy(S = 0.1, V0 = 12, B = 2, b = 1.5, d = -200, Distance = 1000), "d must be positive")
  expect_error(model_roy(S = 0.1, V0 = 12, B = 2, b = 1.5, d = 200, Distance = -1000), "Distance must be positive")

  # Test that speed decreases with distance
  speed_100m <- model_roy(S = 0.1, V0 = 12, B = 2, b = 1.5, d = 200, Distance = 100)
  speed_1000m <- model_roy(S = 0.1, V0 = 12, B = 2, b = 1.5, d = 200, Distance = 1000)
  expect_gt(speed_100m, speed_1000m)
})



