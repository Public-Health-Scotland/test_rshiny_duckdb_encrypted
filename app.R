library(shiny)
library(DBI)
library(duckdb)
library(glue)

# ---- DuckDB connection function ----
connect_encrypted_db <- function(db_path, encryption_key) {
  con <- dbConnect(duckdb())

  tryCatch({
    # Install httpfs if required
    tryCatch(
      dbExecute(con, "INSTALL httpfs;"),
      error = function(e) {
        message("httpfs installation failed: ", e$message)
      }
    )

    # Load extension
    dbExecute(con, "LOAD httpfs;")

    # Attach encrypted database
    dbExecute(
      con,
      glue("
        ATTACH '{db_path}' AS enc (
          ENCRYPTION_KEY '{encryption_key}',
          ENCRYPTION_CIPHER 'GCM'
        );
      ")
    )

    dbExecute(con, "USE enc;")
    return(con)
  }, error = function(e) {
    message("Unable to open encrypted database: ", e$message)
    dbDisconnect(con, shutdown = TRUE)
    return(NULL)
  })
}

# ---- Shiny UI ----
ui <- fluidPage(
  h2("Encrypted DuckDB example"),
  actionButton("read_data", "Read encrypted data"),
  br(),
  br(),
  tableOutput("data")
)

# ---- Shiny server ----
server <- function(input, output, session) {
  db_path <- "data/encrypted.duckdb"
  encryption_key <- "this-is-a-test"

  observeEvent(input$read_data, {
    con <- connect_encrypted_db(
      db_path = db_path,
      encryption_key = encryption_key
    )

    # Connection failed
    if (is.null(con)) {
      showNotification("Unable to open the encrypted database.", type = "error")
      return()
    }

    # Make sure connection is closed afterwards
    on.exit(
      dbDisconnect(con, shutdown = TRUE), add = TRUE
    )

    tryCatch({
      df <- dbGetQuery(con, "SELECT id, message, current_dt FROM encrypted_table;")
      output$data <- renderTable(df)
    }, error = function(e) {
      showNotification(
        paste("Database error:", e$message),
        type = "error"
      )
    })
  })
}

shinyApp(ui, server)