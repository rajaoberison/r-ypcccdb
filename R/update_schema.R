#' Update the structure of an existing database
#'
#' Use an existing SQL file containing statement about the schema to update the database structure. The SQL file can be created using MySQL Workbench by exporting the database.
#'
#' @name update_schema
#'
#' @param con Database connection
#' @param sql_file Path to .sql file from MySQL
#'
#' @return None
#'
#' @examples
#' update_schema(con, '/Users/user/Desktop/structure.sql')
#'
#' @import DBI
#'
#' @export

update_schema <- function(con, sql_file) {

  create.statement <- readLines(sql_file)

  for (statement in create.statement){
    if (nchar(statement) > 0){
      dbSendStatement(con, statement)
    }
  }

}
