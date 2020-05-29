#' Connect to an existing database
#'
#' Takes username and password to connect to an existing database, which will allow the user to query or updata the database. This can also be done using DBI::dbConnect()
#'
#' @name connect_db
#'
#' @param dbname Character. Name of the database schema.
#' @param host Character. Name of the host.
#' @param username Character. Username.
#' @param password Character. Password.
#' @param local_infile Boolean. Set to TRUE when necessary.
#'
#' @return Database connection
#'
#' @examples
#' connect_db('testdb', 'localhost', 'root', 'password')
#'
#' @import DBI
#' @import RMySQL
#'
#' @export

connect_db <- function(dbname, host, username, password, local_infile=NULL) {
  con <- dbConnect(
    RMySQL::MySQL(),
    dbname = dbname,
    host = host,
    user = username,
    password = password
  )
  if (!is.null(local_infile)) {
    dbSendQuery(con, "SET GLOBAL local_infile = true;")
  }

  return(con)

}
