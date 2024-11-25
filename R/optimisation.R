# TODO: support online optimization

#' TimeDeltaCalculator
#'
#' Calculates future event times
#'
#' @import R6
TimeDeltaCalculator <- R6::R6Class(
    "TimeDeltaCalculator",
    public = list(
        initialize = function() {
        },
        cal_time_delta = function(x) {
#            private$expf(x, 1.0, 0.5) 
            private$polyf(x)
        }
    ),
    private = list(
        expf = function(x, a, b) {
            x^(a + b * x)
        },
        polyf = function(x) {
            pows <- 1:length(private$w) 
            outer <- outer(x, pows, function(x, a) x^a)
            result <- outer %*% private$w
            result
        },
        w = c(1.0, 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.01)
    )
)

