# Connecting to RPostgreSQL
require(RPostgreSQL)
library(RPostgreSQL)


createConnection <- function(){
  #Enter the values for you database connection
  dsn_database = "crawler"            # e.g. "compose"
  dsn_hostname = "db01.sle.stg" # e.g.: "aws-us-east-1-portal.4.dblayer.com"
  dsn_port = "5432"                 # e.g. 11101 
  dsn_uid = "crawler"        # e.g. "admin"
  dsn_pwd = "debSOKvyO7OV"      # e.g. "xxx"
  
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    #    print("Connecting to database")
    conn <- dbConnect(drv, 
                      dbname = dsn_database,
                      host = dsn_hostname, 
                      port = dsn_port,
                      user = dsn_uid, 
                      password = dsn_pwd)
    #   print("Connected!")
    return(conn)
  },
  error=function(cond) {
    print("Unable to connect to database.")
  })
}

closeConnection <-function(conn){
  #close connection
  dbDisconnect(conn)
}

