library(testthat)

test_that("PriorityQueue pop and peek max work for a simple int priority function", {
    # Arbitrary priority function
    priority_function <- function(row) {
        row$int + 1
    }
    
    pq <- PriorityQueue$new(priority_fn = priority_function)
    
    sample_rows <- list(
        list(str1 = "A", str2 = "B", int = 5, datetime = Sys.time()),
        list(str1 = "C", str2 = "D", int = 10, datetime = Sys.time()),
        list(str1 = "E", str2 = "F", int = 3, datetime = Sys.time()),
        list(str1 = "C", str2 = "D", int = 10, datetime = Sys.time()),
        list(str1 = "C", str2 = "D", int = 7, datetime = Sys.time())
    )
    
    # Insert rows into the priority queue
    for (row in sample_rows) {
        pq$insert(row)
    }
    
    expect_equal(pq$pop_max()$int, 10)
    expect_equal(pq$pop_max()$int, 10)
    expect_equal(pq$pop_max()$int, 7)
    expect_equal(pq$peek_max()$int, 5)
    expect_equal(pq$peek_max()$int, 5)
    expect_equal(pq$pop_max()$int, 5)
    expect_equal(pq$pop_max()$int, 3)
    expect_null(pq$pop_max())
})

test_that("PriorityQueue handles an empty queue gracefully", {
    priority_function <- function(row) {
        row$int
    }
    
    pq <- PriorityQueue$new(priority_fn = priority_function)
    
    expect_null(pq$pop_max())
})
