# Connecting to RPostgreSQL
require(RPostgreSQL)
library(RPostgreSQL)


createConnection <- function(){
  #Enter the values for you database connection
  dsn_database = "lottery"            # e.g. "compose"
  dsn_hostname = "10.10.30.31" # e.g.: "aws-us-east-1-portal.4.dblayer.com"
  dsn_port = "15431"                 # e.g. 11101 
  dsn_uid = "lottery"        # e.g. "admin"
  dsn_pwd = "-302=(Dkl2$"      # e.g. "xxx"
  
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    print("Connecting to database")
    conn <- dbConnect(drv, 
                      dbname = dsn_database,
                      host = dsn_hostname, 
                      port = dsn_port,
                      user = dsn_uid, 
                      password = dsn_pwd)
    print("Connected!")
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

