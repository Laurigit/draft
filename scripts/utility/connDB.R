connDB <- function(con, dbname_input = "betmtg2") {
  con <- tryCatch({

    res <- dbFetch(dbSendQuery(con, "SHOW TABLES"))

    con
  }, error = function(ef) {

    con <<- dbConnect(MySQL(),
                     user = 'root',
                     password = 'betmtg_pw',
                     host = '34.88.252.73',
                     port = 3306,
                     dbname = dbname_input)
  })
  return(con)
}
# con <<- dbConnect(MySQL(),  user = 'root', password = 'my-spw', host = '127.0.0.1', port = 3306,  dbname = 'betmtg')
# con <<- dbConnect(MySQL(),  user = 'root', password = 'my-spw', host = 'localhost', port = 3306,  dbname = 'betmtg')
#
# con <<- dbConnect(MySQL(),  user = 'root', password = 'my-spw', host = '172.19.0.3', port = 3306,  dbname = 'betmtg')
# "
#con <<- dbConnect(MySQL(),  user = 'root', password = 'my-spw', host = '172.19.0.3', port = 3306,  dbname = 'sys')

