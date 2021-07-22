#install.packages(c('DBI','RMySQL'))

library(RMySQL)
library(DBI)

conn <- DBI::dbConnect(
  RMySQL::MySQL(),
  user = 'root',
  password = '980415xmyXMY!',
  dbname = 'MySQL',
  host = 'localhost',
  port=3306)


dbWriteTable(conn, "proj", proj)



#dbListTables(mydb)
#dbListTables(mydb,'dept')



