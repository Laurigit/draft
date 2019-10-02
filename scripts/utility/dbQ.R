dbQ <- function(query) {
  con <- connDB(con)
  res <- as.data.table(dbFetch(dbSendQuery(con, query)))
  return(res)
}
