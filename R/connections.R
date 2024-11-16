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
        write_new_card = function(new_card) {
            dbWriteTable(private$con, "cloudcards", new_card, append = TRUE, row.names = FALSE)
        },
        close = function() {
            DBI::dbDisconnect(private$con)
            private$con <- NULL
        }
    ),
    private = list(
        con = NULL
    )
)
