library(cloudcards)
library(data.table)
library(shiny)

ui <- fluidPage(
  textOutput("question_answer"),
  verbatimTextOutput("DEBUG_INFO"),
  actionButton("reveal", "Reveal"),
  actionButton("yes", "Yes"),
  actionButton("no", "No"),
  numericInput("integer_box", "Enter an integer:", value = 3, step = 1),
  actionButton("nextb", "Activate"),
  actionButton("delete", "Delete"),
  textInput("text_input", "Add a card", value = "Add a hash delimited q-a"),
  actionButton("add_card", "Add new card")
)

server <- function(input, output, session) {

  deckdb <- DeckDatabase$new(
      "localhost", 5432,
      "cloudcards", "cloudcards",
      "cloudcards"
  )

  currqa <- reactiveVal("")
  currcard <- reactiveVal("")
  ready_to_deal <- reactiveVal(TRUE) 
  debug_deck <- reactiveVal("")
  
  observe({
    if (ready_to_deal() == TRUE) {
        card <- deckdb$get_next(TRUE)
        dt <- as.numeric(difftime(card$due, Sys.time(), units = "secs"))
        dt <- max(0.0, dt)
        if (dt == 0.0) {
            currcard(card) 
            currqa(card$question)
            ready_to_deal(FALSE)
        }
#        invalidateLater(dt * 1000, session)
        invalidateLater(100, session)
    } else {
        invalidateLater(100, session)
    }
  })

  observe({
    cards <- deckdb$get_sorted_cards()
    cards$mindue2 <- pmax(cards$due, Sys.time())
    formatted_table <- capture.output(print(cards, row.names = FALSE))
    out <- paste(formatted_table, collapse = "\n")
    out <- paste(out, Sys.time())
    debug_deck(out)
    invalidateLater(500, session)
  })

  observeEvent(input$reveal, {
    currqa(paste(currcard()$question, currcard()$answer))
  }) 

  observeEvent(input$no, {
    if (ready_to_deal() == FALSE) {  # a card is not out
        card <- currcard()
        card$counter = 0
        deckdb$update_card(card)   
        currcard("")
        currqa("")
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
    currqa("")
    ready_to_deal(TRUE)
  })

  observeEvent(input$yes, {
    if (ready_to_deal() == FALSE) {  # a card is not out
        card <- currcard()
        card$counter = card$counter + 1
        deckdb$update_card(card)
        currcard("")
        currqa("")
        ready_to_deal(TRUE)
    }
  })

  observeEvent(input$nextb, {
    cards <- deckdb$activate_new_cards(as.numeric(input$integer_box)) 
    ready_to_deal(TRUE)
  })

  observeEvent(input$add_card, {
    qa <- input$text_input
    spl <- strsplit(qa, "#")[[1]]
    if (length(spl) == 2) {
        q <- trimws(spl[1])
        a <- trimws(spl[2])
        qlen <- nchar(q)
        alen <- nchar(a)
        if (qlen > 0 && alen > 0) {
           deckdb$write_qa(q, a)
           updateTextInput(session, "text_input", value = "Add a hash delimited q-a")
        }
    }
  })

  output$question_answer <- renderText({
    currqa()
  })

  output$DEBUG_INFO <- renderText({
    debug_deck()
  })

}

shinyApp(ui, server)
