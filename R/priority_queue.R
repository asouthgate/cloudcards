#' Priority Queue Class (Linear Scan)
#'
#' A priority queue implemented with a simple linear scan using a custom priority function.
#'
#' @import R6
#' @export
PriorityQueue <- R6::R6Class(
    "PriorityQueue",
    public = list(
        data = NULL,
        priority_fn = NULL,
        
        #' Initialize the PriorityQueue
        #' 
        #' @param priority_fn A function to calculate the priority of each row
        initialize = function(priority_fn) {
            if (!is.function(priority_fn)) {
                stop("priority_fn must be a valid function.")
            }
            self$data <- list()
            self$priority_fn <- priority_fn
        },
        
        #' Insert a row into the priority queue
        #' 
        #' @param row A list representing a row of data
        insert = function(row) {
            self$data <- append(self$data, list(row))
            vals <- sapply(self$data, self$priority_fn)
            inds <- order(vals, decreasing=FALSE)
            self$data = self$data[inds]
        },
        
        #' Extract the highest-priority row
        #' 
        #' @return A list representing the highest-priority row
        pop_max = function() {
            if (length(self$data) == 0) {
                return(NULL)
            }
            el <- self$peek_max()
            self$data = head(self$data, -1)
            return(el)
        },

        #' Peek at the highest priority row
        #'
        #' @return A list representing the highest-priority row
        peek_max = function() {
            return(self$data[[length(self$data)]])
        }
        
    )
) 
