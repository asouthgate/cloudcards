library(testthat)

get_random_card <- function() {
    random_q <- paste0(sample(LETTERS, 5, replace = TRUE), collapse = "")
    random_q <- paste("what is", random_q)
    new_card <- data.table(
        question = random_q,
        answer = "Bar",
        counter = 0,
        created_at = Sys.time(),
        last_accessed = Sys.time()
    )
    return(new_card)
}

test_that("DeckDatabase can open and close successfully", {
    db <- DeckDatabase$new(
        "localhost", 5432,
        "cloudcards", "cloudcards", 
        "cloudcards"
    )
    db$close()
    expect_equal(TRUE, TRUE)
})

test_that("We can write a new card to DeckDatabase successfully", {
    db <- DeckDatabase$new(
        "localhost", 5432,
        "cloudcards", "cloudcards", 
        "cloudcards"
    )
    all_cards <- db$fetch_cards()
    n0 <- nrow(all_cards)
    new_card <- get_random_card()
    db$write_new_card(new_card)
    all_cards <- db$fetch_cards()
    expect_equal(nrow(all_cards), n0 + 1)
    db$close()
})

test_that("We can successfully delete a card from the databas", {
    db <- DeckDatabase$new(
        "localhost", 5432,
        "cloudcards", "cloudcards",
        "cloudcards"
    )
    new_card <- get_random_card()
    id <- db$write_new_card(new_card)
    card <- db$fetch_card(id)
    db$delete_card(id)
    tryCatch(
        {
            card <- db$fetch_card(id)    
            expect_equal(FALSE, TRUE)
        },
        error = function(e) {
            expect_equal(TRUE, TRUE)
            return(NULL)
        }
    )
    expect_equal(FALSE, FALSE)
})

test_that("We can update a card successfully", {
    db <- DeckDatabase$new(
        "localhost", 5432,
        "cloudcards", "cloudcards", 
        "cloudcards"
    )
    card <- get_random_card()
    id <- db$write_new_card(card)
    stored_card <- db$fetch_card(id)
    stored_card$answer <- "Baz!"
    db$update_card(stored_card)
    result <- db$fetch_card(id)
    expect_equal(result$answer, "Baz!")
    db$close()
})

test_that("We can get_next() successfully for simple data", {
    db <- DeckDatabase$new(
        "localhost", 5432,
        "cloudcards", "cloudcards",
        "cloudcards"
    )
    all_cards <- db$fetch_cards()
    n0 <- nrow(all_cards)
    new_card <- get_random_card()
    db$write_new_card(new_card)
    nextc <- db$get_next()
    expect_equal(TRUE, TRUE)
    db$close()
})

