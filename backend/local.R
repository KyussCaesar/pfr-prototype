# Local storage backend using SQLite.

message("using local storage backend")
library(dplyr)

vals = reactiveValues()

conn <-
  DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = "pfr.db" # TODO: probably accept this by envvar
  )

if (!("records" %in% DBI::dbListTables(conn))) {
  DBI::dbCreateTable(
    conn,
    "records",
    c(
      id = "text",
      name = "text",
      type = "text",
      amount = "text",
      freq = "text",
      datetime_added = "text",
      datetime_added_str = "text",
      mthly_equiv = "text"
    )
  )
}

vals$records <- tbl(conn, "records")

vals$idctr = 0

#' Add a record to the transaction table
add_record <- function(n, t, a, f) {
  rid = vals$idctr + 1
  vals$idctr = rid

  dttm_add = Sys.time()
  mthly =
    a *
    switch(
      f,
      daily = 4.28*7,
      workdays = 4.28*5,
      weekly = 4.28,
      monthly = 1,
      yearly = 1/12
    )

  DBI::dbExecute(
    conn,
    "INSERT INTO records VALUES (?,?,?,?,?,?,?,?)",
    param = list(rid, n, t, a, f, dttm_add, strftime(dttm_add, "%Y-%m-%dT%H:%M:%S"), mthly)
  )

  rid
}
