library(cloudcards)
library(data.table)
library(shiny)

ui <- fluidPage(
  textOutput("question_answer"),
  verbatimTextOutput("DEBUG_INFO"),
  actionButton("yes", "Yes"),
  actionButton("no", "No"),
  actionButton("nextb", "Next"),
  actionButton("delete", "Delete")
)

server <- function(input, output, session) {

  deckdb <- DeckDatabase$new(
      "localhost", 5432,
      "cloudcards", "cloudcards",
      "cloudcards"
  )

  currcard <- reactiveVal("")
  ready_to_deal <- reactiveVal(TRUE) 
  debug_deck <- reactiveVal("")
  
  observe({
    if (ready_to_deal() == TRUE) {
        card <- deckdb$get_next()
        dt <- as.numeric(difftime(card$due, Sys.time(), units = "secs"))
        dt <- max(0.0, dt)
        if (dt == 0.0) {
            currcard(card) 
            ready_to_deal(FALSE)
        }
        invalidateLater(dt * 1000, session)
    } else {
        invalidateLater(100, session)
    }
  })

  observe({
    cards <- deckdb$get_sorted_cards()
    cards$mindue2 <- pmax(cards$due, Sys.time())
    formatted_table <- capture.output(print(head(cards), row.names = FALSE))
    out <- paste(formatted_table, collapse = "\n")
    out <- paste(out, Sys.time())
    debug_deck(out)
    invalidateLater(500, session)
  })

  observeEvent(input$no, {
    if (ready_to_deal() == FALSE) {  # a card is not out
        card <- currcard()
        card$counter = 0
        deckdb$update_card(card)   
        currcard("")
        ready_to_deal(TRUE)
    }
  })

  observeEvent(input$delete, {
    cc <- currcard()
    txt <- paste("Are you sure you want to delete this card?", cc$question, cc$answer)
    showModal(
      modalDialog(
        title = "Are you sure you want to delete this card?",
        txt,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_yes", "Yes")
        )
      )
    )
  })

  observeEvent(input$confirm_yes, {
    removeModal()
    cc <- currcard()
    deckdb$delete_card(cc$id)
    currcard("")
    ready_to_deal(TRUE)
  })

  observeEvent(input$yes, {
    if (ready_to_deal() == FALSE) {  # a card is not out
        card <- currcard()
        card$counter = card$counter + 1
        deckdb$update_card(card)
        currcard("")
        ready_to_deal(TRUE)
    }
  })

  observeEvent(input$nextb, {
    ready_to_deal(TRUE)
  })
  
  output$question_answer <- renderText({
    cc <- currcard()
    paste(format(cc))
  })

  output$DEBUG_INFO <- renderText({
    debug_deck()
  })

}

shinyApp(ui, server)
