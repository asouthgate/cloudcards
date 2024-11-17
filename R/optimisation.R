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
            private$expf(x, 1.0, 0.5) 
        }
    ),
    private = list(
        expf = function(x, a, b) {
            x^(a + b * x)
        }
    )
)
