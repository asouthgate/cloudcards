library(testthat)

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
    new_card <- data.table(
        question = "Foo?",
        answer = "Bar",
        counter = 0,
        created_at = Sys.time(),
        last_accessed = Sys.time()
    )
    db$write_new_card(new_card)
    all_cards <- db$fetch_cards()
    expect_equal(nrow(all_cards), n0 + 1)
    db$close()
})

test_that("We can update a card successfully", {
    db <- DeckDatabase$new(
        "localhost", 5432,
        "cloudcards", "cloudcards", 
        "cloudcards"
    )
    card <- data.table(
        question = "Foo?",
        answer = "Bar",
        counter = 0,
        created_at = Sys.time(),
        last_accessed = Sys.time()
    )
    id <- db$write_new_card(card)
    stored_card <- db$fetch_card(id)
    stored_card$answer <- "Baz!"
    db$update_card(stored_card)
    result <- db$fetch_card(id)
    expect_equal(result$answer, "Baz!")
    db$close()
})
