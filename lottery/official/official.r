path<-getwd()
source(paste0(path,"/lottery/common/db01-crawler.r"))
source(paste0(path,"/lottery/common/statistic-ssc.r"))
query <- function(conn,game,crawler, startDate, endDate){
  queryString <- "select TO_NUMBER(substring(result from 1 for 1),'9') as loc1,
                        TO_NUMBER(substring(result from 3 for 1),'9') as loc2,
                        TO_NUMBER(substring(result from 5 for 1),'9') as loc3,
                        TO_NUMBER(substring(result from 7 for 1),'9') as loc4,
                        TO_NUMBER(substring(result from 9 for 1),'9') as loc5
                        from crawler_result where game= 'GAME' and crawler='CRAWLER' 
                        and update_timestamp between 'START_DATE' and 'END_DATE' and result IS NOT NULL"
  
  #select result from crawler_result where game= 'TXFFC' and crawler='TJ' 
  #and update_timestamp between '2020-02-01' and '2020-03-10' and result IS NOT null
  
  #更換vendor id
  queryString <- gsub("GAME",game,queryString)
  
  #更換彩種
  queryString <- gsub("CRAWLER",crawler,queryString)
  
  #更換vendor id
  queryString <- gsub("START_DATE",startDate,queryString)
  
  #更換vendor id
  queryString <- gsub("END_DATE",endDate,queryString)
  
  
  result <- dbGetQuery(conn,queryString)
  return(result)
}


# vendorId : 18LUCK , MANBETX ...
# kind : NYSSC30S 
# startDate : 2020-03-01
# endDate   : 2020-03-09
main <- function (game,crawler,startDate,endDate){
  conn<-createConnection()
  report(conn,game,crawler,startDate,endDate)
  closeConnection(conn)
  
}

#
# 官方彩，統一在STG db 讀取crawler寫入的資料
# KTJ,OPEN_CAI,MANY_CAI
#
print("================= TXFFC")
main("CQSSC",'MANY_CAI','2020-03-15','2020-03-16')
print("================= TXFFC======================")

