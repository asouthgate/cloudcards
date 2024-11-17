library(testthat)

test_that("TimeDeltaCalculator produces expected results", {
    tdc <- TimeDeltaCalculator$new()
    x <- 1:3
    exp <- c(1.00000, 4.00000, 15.58846)
    expect_equal(tdc$cal_time_delta(x), exp, tolerance = 1e-3)
})
