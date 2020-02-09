# R session storage backend; just use a dataframe inside reactiveValues

message("using session storage backend")

vals = reactiveValues()
vals$records <-
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

  vals$records[
    nrow(vals$records) + 1,
    c(
      "id", "name", "type", "amount", "freq",
      "datetime_added", "datetime_added_str", "mthly_equiv"
    )
  ] =
    c(rid, n, t, a, f, dttm_add, strftime(dttm_add, "%Y-%m-%dT%H:%M:%S"), mthly)

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

