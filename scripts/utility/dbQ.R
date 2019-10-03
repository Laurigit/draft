dbQ <- function(query, con) {

  res <- as.data.table(dbFetch(dbSendQuery(con, query),
                               n = -1))
  return(res)
}
