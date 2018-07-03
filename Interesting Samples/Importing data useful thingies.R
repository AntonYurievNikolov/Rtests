# Load the DBI package
library(DBI)
#install.packages("DBI")

# Connect to the MySQL database: con
con <- dbConnect(odbc(),
                 Driver = "SQLServer",
                 dbname = "master", 
                 host = "romesql1", 
          
                 user = "profiler",
                 password = "profiler")

# Get table names
table_names <- dbListTables(con)

# Import all tables
tables <- lapply(table_names, dbReadTable, conn = con)

# Print out tables
tables

``

res <- dbSendQuery(con, "SELECT * FROM comments WHERE user_id > 4")
# Use dbFetch() twice
dbFetch(res, n =2)
dbFetch(res, n =2)
# Clear res
dbClearResult(res)

