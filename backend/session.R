# R session storage backend; just use a dataframe inside reactiveValues

# app data is persisted to RDS; at the moment, this is done on _every_ write
# not scalable, but works for now.

# default storage dir
.storage_dir = "."

# use envvar to set the storage dir
.env_storage_dir = Sys.getenv("PFR_APP_STORAGE")
if (!is.null(.env_storage_dir) && .env_storage_dir != "") {
  .storage_dir <- .env_storage_dir
}

.storage = paste0(.storage_dir, "/app.rds")
message(sprintf("using storage file %s", .storage))

new_records_table = function() {
    data.frame(
      id = c(),
      name = c(),
      type = c(),
      amount = c(),
      freq = c(),
      datetime_added = c(),
      datetime_added_str = c(),
      mthly_equiv = c()
    )
}

if (file.exists(.storage)) {
  message("loading state from persistent storage")
  vals <- readRDS(.storage)

} else {
  vals = reactiveValues()
  vals$records <- new_records_table()
  vals$idctr = 0

  # initialise the persistence file
  saveRDS(vals, .storage)
}

# note: might make sense to expose this as part of the "backend API"
# leave it up to the application to persist, instead of persisting on every write
.persist = function() {
  message("saving state to persistent storage")
  saveRDS(vals, .storage)
}

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

  vals$records[
    nrow(vals$records) + 1,
    c(
      "id", "name", "type", "amount", "freq",
      "datetime_added", "datetime_added_str", "mthly_equiv"
    )
  ] =
    c(rid, n, t, a, f, dttm_add, strftime(dttm_add, "%Y-%m-%dT%H:%M:%S"), mthly)

  .persist()

  rid
}

#' Remove a record from the transactions table
rm_record <- function(rid) {

  if (!(rid %in% vals$records[["id"]])) {
    stop(sprintf("cannot remove record: id %i is invalid", rid))
  }

  keep = vals$records[["id"]] != rid
  rcrd = vals$records[!keep,]
  vals$records <- vals$records[keep,]

  .persist()

  rcrd

}

#' Get the transaction table
get_transactions <- function() {
  vals$records
}

#' Get the number of transactions
get_num_transactions <- function() {
  nrow(vals$records)
}

.get_mthly_type <- function(what) {

  mthly = 0
  if (nrow(vals$records) != 0) {
    mthly =
      sum(
        c(
          0,
          as.numeric(
            vals$records[vals$records[,"type"] == what,"mthly_equiv"]
          )
        )
      )
  }

  mthly
}

#' Get the monthly income
get_mthly_income <- function() {
  .get_mthly_type("income")
}

#' Get the monthly expenditure
get_mthly_expense <- function() {
  .get_mthly_type("expense")
}

#' Removes all transactions
rm_all <- function() {
  vals$records <- new_records_table()
  .persist()
}
