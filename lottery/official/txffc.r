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
  result <- query(conn,game,crawler,startDate,endDate)
  
  # show title
  loc <- list(game,crawler,startDate,endDate,length(result$loc1))
  m <- matrix(loc,1,5,dimnames = list(c("查詢條件"),c("玩法","獎源","開始","結束","筆數")))
  print(m)
  

  newList = count(result)
  # 五位數出現次數
  fl <- frequenciesLocation(newList)
  print(fl)
  
  # 位數單雙
  fre <- frequenciesSD(result)
  print(fre)
  
  # 出現次數，對子，三條
  fu<-frequenciesUnique(result)
  print(fu)
  
  n<-normalTest(as.numeric(newList[[1]]),as.numeric(newList[[2]]),as.numeric(newList[[3]]),as.numeric(newList[[4]]),as.numeric(newList[[5]]))
  print(n)
  t<-ttest(as.numeric(newList[[1]]),as.numeric(newList[[2]]),as.numeric(newList[[3]]),as.numeric(newList[[4]]),as.numeric(newList[[5]]))
  print(t)
  closeConnection(conn)
}

#
# 官方彩，統一在STG db 讀取crawler寫入的資料
#
#
print("================= TXFFC")
main("TXFFC",'TJ','2020-03-01','2020-03-12')
print("================= TXFFC======================")

