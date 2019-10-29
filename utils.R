#' utils

if (is.null(.GlobalEnv[["UTILS_LOADED"]]) || !UTILS_LOADED) {

  UTILS_LOADED <- TRUE

  #' Return a new DB connection
  db_connect <- function() {

    # define a list of "known" connectors
    CONNECTORS <- list(
      "LOCAL_POSTGRES" = function() {
        DBI::dbConnect(
          RPostgres::Postgres(),
          host     = "postgres",
          port     = 5432,
          user     = "postgres",
          password = "postgres"
        )
      }
    )

    spec <- Sys.getenv("PFR_DB_CONNECTION")

    # for testing
    spec <- "LOCAL_POSTGRES"

    if (is.null(spec) || spec == "") {
      stop(sprintf("missing connection spec"))
    }

    connector <- CONNECTORS[[spec]]

    if (is.null(connector)) {
      stop(sprintf("bad connection spec: %s:", spec))
    }

    connector()
  }
}
