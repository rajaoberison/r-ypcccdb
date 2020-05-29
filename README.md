# ypcccdb
R Package for managing YPCCC database. All data processing and database management are done in R, except for the 'first' creation of the database.

To install the package from GitHub, run `devtools::install_github('YPCCC/ypcccdb', auth_token=your_access_token)`. You can create token from here: https://github.com/settings/tokens.

## Creating a database
To create a database, open Terminal and type:
`mysql -u root -p`
Then enter password when prompted.

(Also make sure that you have mysql installed: `brew install mysql`).

After you authenticate, you'll be in the mysql command line. To create the database, type:
`CREATE DATABASE database_name;`

To check existing / created databases, type:
`SHOW DATABASES;`

To exit, type:
`exit`

## Accessing the database in R

To connect: `con <- connect_db()`

To disconnect: `dbDisconnect(con)`
