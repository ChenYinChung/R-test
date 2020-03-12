path<-getwd()
source(paste0(path,"/lottery/common/db01-sle.r"))
source(paste0(path,"/lottery/common/statistic-ssc.r"))
library(rJava)
library(xlsx)
query <- function(conn,vendorId,gameId, startDate, endDate){
  queryString <- "select TO_NUMBER(substring(draw_result from 1 for 1),'9') as loc1,
                        TO_NUMBER(substring(draw_result from 3 for 1),'9') as loc2,
                        TO_NUMBER(substring(draw_result from 5 for 1),'9') as loc3,
                        TO_NUMBER(substring(draw_result from 7 for 1),'9') as loc4,
                        TO_NUMBER(substring(draw_result from 9 for 1),'9') as loc5
                        from self_open_result where vendor_id= 'VENDOR_ID' and game_id='GAME_ID' 
                        and create_date between 'START_DATE' and 'END_DATE' and draw_result IS NOT NULL"
  #更換vendor id
  queryString <- gsub("VENDOR_ID",vendorId,queryString)
  
  #更換彩種
  queryString <- gsub("GAME_ID",gameId,queryString)
  
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
main <- function (vendorId,gameId,startDate,endDate){
  conn<-createConnection()
  result <- query(conn,vendorId,gameId,startDate,endDate)
  
  loc <- list(vendorId,gameId,startDate,endDate,length(result$loc1))
  m <- matrix(loc,1,5,dimnames = list(c("查詢條件"),c("廠商","玩法","起始","結束","筆數")))
  print(m)
  
    
  newList = count(result)
  #陣列各行列出現次數
  fl <- frequenciesLocation(newList)
  print(fl)
  
  #單雙
  fsd<-frequenciesSD(result)
  print(fsd)
  
  
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
#自開彩獎號驗證，以Production實際內容比對
#
#
main("18LUCK",'NYSSC30S','2020-03-01','2020-03-12')
main("MANBETX",'NYSSC30S','2020-03-01','2020-03-10')
