library(data.table)

source("R/optimisation.R")
source("R/logging.R")

#' DeckDatabase
#'
#' Abstract interface to database containing deck of flash cards
#'
#' @import R6
#' @export
DeckDatabase <- R6::R6Class(
    "DeckDatabase",
    public = list(
        #' Initialize the DeckDatabase by connecting to postgres
        #'
        #' @param dbname
        #' @param host
        #' @param port
        #' @param user
        #' @param password
        initialize = function(host, port, user, password, dbname) {
            private$con <- DBI::dbConnect(
                RPostgres::Postgres(),
                dbname = dbname,
                host = host,
                port = port,
                user = user,
                password = password
            )
        },
        activate_new_cards = function(n) {

            log4r::info(
              logger,
              message = "Activating new cards",
              n = n
            )

            cards <- DBI::dbGetQuery(private$con, paste0("SELECT * FROM cloudcards WHERE active = FALSE LIMIT ", n))
            cards <- as.data.table(cards)
            cards[, active := TRUE]

            log4r::info(
              logger,
              message = "Got inactive cards:",
              cards = format(cards)
            )

            for (i in seq_len(nrow(cards))) {
              self$update_card(cards[i,])
            }
        },
        close = function() {
            DBI::dbDisconnect(private$con)
            private$con <- NULL
        },
        delete_card = function(id) {
            query <- paste0("DELETE FROM cloudcards WHERE id = ", id)
            data <- DBI::dbExecute(private$con, query)
        },
        fetch_cards = function(active=NULL) {
            if (is.null(active)) {
                data <- DBI::dbGetQuery(private$con, "SELECT * FROM cloudcards")
            } else {
                data <- DBI::dbGetQuery(private$con, paste0("SELECT * FROM cloudcards WHERE active = ", active))
            }
            as.data.table(data)
        },
        fetch_card = function(id) {
            data <- DBI::dbGetQuery(private$con, paste0("SELECT * FROM cloudcards where id = ", id))
            as.data.table(data)
        },
        get_next = function(active=NULL) {
            cards <- self$fetch_cards(active);
            if (nrow(cards) == 0) {
                return(NULL)
            }
            mininds <- which.min(cards$due)
            if (length(mininds) > 1) {
                mini <- sample(mininds, 1)
            } else {
                mini <- mininds[1]
            }
            minc <- cards[mini] 
            return(minc)
        },
        get_sorted_cards = function() {
            cards <- self$fetch_cards();
            due <- cards$due
            ordering <- order(due)
            return(cards[ordering])
        },
        update_card = function(card) {         

            log4r::info(
              logger,
              message = "Updating a card",
              card = format(card)
            )

            card$last_accessed <- Sys.time()
            card$due <- card$last_accessed + private$time_delta_calculator$cal_time_delta(card$counter)
            update_cols <- card[, setdiff(names(card), "id"), with = FALSE]
            set_clause <- paste0(names(update_cols), " = $", seq_along(update_cols), collapse = ", ")
            query <- paste0("UPDATE cloudcards SET ", set_clause, " WHERE id = ", card$id)
            params <- unname(as.list(update_cols))
            DBI::dbExecute(private$con, query, params = params)
        },
        write_qa = function(q, a) {
            new_card <- data.table(
                question = q,
                answer = a,
                counter = 0,
                created_at = Sys.time(),
                last_accessed = Sys.time(),
                active = TRUE
            )
            self$write_new_card(new_card)
        },
        write_new_card = function(new_card) {

            log4r::info(
              logger,
              message = "Writing a new card",
              card = format(new_card)
            )

            new_card$due <- new_card$last_accessed + private$time_delta_calculator$cal_time_delta(new_card$counter)
            columns <- names(new_card)
            value_placeholders <- paste0("$", seq_along(columns), collapse = ", ")
            column_names <- paste(columns, collapse = ", ")
            query <- paste0("INSERT INTO cloudcards (", column_names, ") ",
                          "VALUES (", value_placeholders, ") RETURNING id")
            params <- unname(as.list(new_card))
            result <- DBI::dbGetQuery(private$con, query, params = params)
            return(result$id)
        }
    ),
    private = list(
        con = NULL,
        time_delta_calculator = TimeDeltaCalculator$new()
    )
)
