#install.packages(c('DBI','RMySQL'))

###

#RMySQL 核心命令

#DBI 是一个用于数据库连接的基础包，
#表的连接 dbConnect()/dbDisconnect()、
#创建 dbCreateTable()、
#读 dbReadTable() 、
#写 dbWriteTable()、
#删除 dbRemoveTable()、
#查 dbSendQuery()/dbGetQuery()，

#更细一层，
#查看表是否存在 dbExistsTable()、 
#表的各个字段 dbListFields()、
#各个字段的存储类型 dbDataType() 等。
#以上操作对于每一种数据库都是需要支持的，
#所以它们被抽象出来作为一个基础的类，
#被具体的数据库连接接口如 RMySQL/RSQLite 等实例化继承。
#就 RMySQL 来说，和 DBI 共同的函数占到了其自身的 53.33%，
#由于 MySQL 数据库本身支持事务操作（简单点，可以理解为单条记录的频繁增、删、改、查），
#所以 RMySQL 提供了支持事务操作的特别函数 dbWithTransaction()。

###




library(RMySQL)
library(DBI)

help(package='RMySQL')

# 用 root 账户登录连接数据库 lims
con <- DBI::dbConnect(
  RMySQL::MySQL(),
  user = 'root',
  password = '980415xmyXMY!',
  dbname = 'lims',
  host = 'localhost',
  port=3306
  )
#获取连接信息，查看database下所有表，以及删除testname表
summary(con)
dbGetInfo(con) # get connect info
dbListTables(con)  # list all tables in the database
dbRemoveTable(con,"test") # delete table


# 写数据库表
fruits <-data.frame(id=1:5,name=c("苹果","香蕉","梨子","玉米","西瓜"),price=c(8.8,4.98,7.8,6,2.1),status=c("无","打折","无","售罄","批发"))
dbListTables(con)
dbWriteTable(con,"fruits",fruits)
dbListTables(con)


RMySQL::dbWriteTable(conn=con, 
                     name="proj", 
                     value=proj,
                     row.names = FALSE)

RMySQL::dbReadTable(conn,'proj')

#dbListTables(mydb)
#dbListTables(mydb,'dept')


server <- function(input, output, session) {
  output$tbl <- renderTable({
    conn <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "shinydemo",
      host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
      username = "guest",
      password = "guest")
    on.exit(dbDisconnect(conn), add = TRUE)
    sql <- "SELECT * FROM City WHERE ID = ?id1 OR ID = ?id2 OR ID = ?id3;"
    query <- sqlInterpolate(conn, sql, id1 = input$ID1,
                            id2 = input$ID2, id3 = input$ID3)
    ### SQL injection prevention
    ### function sqlInterpolate() is to safely interpolate values into an SQL string, 
    ### therefore protecting you from injection attacks.
    dbGetQuery(conn, query)
  })
}



