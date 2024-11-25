library(testthat)

test_that("TimeDeltaCalculator produces expected results", {
    tdc <- TimeDeltaCalculator$new()
    x <- 1:13
    res <- tdc$cal_time_delta(x)
    for (i in 2:length(res)) {
        expect_gt(res[i], res[i-1])
    }
    expect_gt(res[length(res)], 5000)
    res <- tdc$cal_time_delta(x)
    expect_gt(tdc$cal_time_delta(2), tdc$cal_time_delta(1))
    expect_gt(tdc$cal_time_delta(1), tdc$cal_time_delta(0))
})
