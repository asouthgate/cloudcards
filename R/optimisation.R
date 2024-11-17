#' Exponential function with an affine exponent in x
#'
#' Calculates the time before the next event as:
#'  x^(a + x * b)
#'
#' @param x Input sequence
#' @param a Constant exponent
#' @param b Scalar for exponent
#'
#' @return Time in seconds until next event is scheduled.
exponential_affine_exponent <- function(x, a, b) {
    x^(a + b * x)
}


# TODO: support online optimization

#' TimeDeltaCalculator
#'
#' Calculates future event times
#'
#' @import R6A
#' @export
TimeDeltaCalculator <- R6::R6Class(
    "TimeDeltaCalculator",
    public = list(
        initialize = function() {

        },
        cal_time_delta = function(x) {
            exponential_affine_exponent(x, 1.0, 0.5) 
        }
    )
)
