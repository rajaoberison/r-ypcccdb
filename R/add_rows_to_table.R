#' Add dataframe rows to a database table
#'
#' Takes a database connection with a table and add to it the selected column values from a dataframe
#'
#' @name add_rows_to_table
#'
#' @param con Database connection created using connect_db() or DBI::dbConnect()
#' @param db_table Character. Name of the table to be updated within the database
#' @param db_columns Character. List of column names of the table to be updated within the database. The format should in be an SQL syntax: (col1, col2, col3, ...)
#' @param df_name Dataframe with the updated values
#'
#' @return Updated dataframe from the database
#'
#' @examples add_rows_to_table(con, 'survey', '(source, wave, year)', survey)
#'
#' @import tibble
#' @import DBI
#'
#' @export

add_rows_to_table <- function(con, db_table, db_columns, df_name) {

  dbSendStatement(
    con,
    gsub(", 'NA'|, ''", ", NULL", as.character(paste0(
      "INSERT INTO ", trimws(db_table), " ", trimws(db_columns), " VALUES ",
      paste0(apply(
        df_name,
        1, function(x) paste0("('", paste0(x, collapse = "', '"), "')")
      ), collapse = ", "), ";"
    )))
  )

  df_name <- as_tibble(dbReadTable(con, db_table))

  return(df_name)

}
