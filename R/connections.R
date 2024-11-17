library(data.table)

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
        fetch_cards = function() {
            data <- DBI::dbGetQuery(private$con, "SELECT * FROM cloudcards")
            as.data.table(data)
        },
        fetch_card = function(id) {
            data <- DBI::dbGetQuery(private$con, paste0("SELECT * FROM cloudcards where id = ", id))
            as.data.table(data)
        },
        write_new_card = function(new_card) {
            columns <- names(new_card)
            value_placeholders <- paste0("$", seq_along(columns), collapse = ", ")
            column_names <- paste(columns, collapse = ", ")
            query <- paste0("INSERT INTO cloudcards (", column_names, ") ",
                          "VALUES (", value_placeholders, ") RETURNING id")
            params <- unname(as.list(new_card))
            result <- DBI::dbGetQuery(private$con, query, params = params)
            return(result$id)
        },
        update_card = function(card) {         
            update_cols <- card[, setdiff(names(card), "id"), with = FALSE]
            set_clause <- paste0(names(update_cols), " = $", seq_along(update_cols), collapse = ", ")
            query <- paste0("UPDATE cloudcards SET ", set_clause, " WHERE id = ", card$id)
            params <- unname(as.list(update_cols))
            DBI::dbExecute(private$con, query, params = params)
        },
        get_next = function() {
            cards <- self$fetch_cards();
            cal_due <- function(d, counter) {
                d <- as.POSIXct(d)
                dt <- private$time_delta_calculator$cal_time_delta()
                return(d + dt)
            }
            cards[, due := pcal_due(last_accessed, counter)]
            minc <- cards[which.min(due)]
            return(minc)
        },
        close = function() {
            DBI::dbDisconnect(private$con)
            private$con <- NULL
        }
    ),
    private = list(
        con = NULL,
        time_delta_calculator = TimeDeltaCalculator$new()
    )
)
