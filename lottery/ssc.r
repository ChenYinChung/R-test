source("/Users/sammy/git/survey/r/lottery/db-connect.r")

conn<-createConnection()

query <- function(vendorId, startDate, endDate){
  queryString <- "select TO_NUMBER(substring(draw_result from 1 for 1),'9') as loc1,
                        TO_NUMBER(substring(draw_result from 3 for 1),'9') as loc2,
                        TO_NUMBER(substring(draw_result from 5 for 1),'9') as loc3,
                        TO_NUMBER(substring(draw_result from 7 for 1),'9') as loc4,
                        TO_NUMBER(substring(draw_result from 9 for 1),'9') as loc5
                        from self_open_result where vendor_id= 'VENDOR_ID' and game_id='NYSSC30S' 
                        and create_date between 'START_DATE' and 'END_DATE' and draw_result IS NOT NULL limit 5000"
  #更換vendor id
  queryString <- gsub("VENDOR_ID",vendorId,queryString)

  #更換vendor id
  queryString <- gsub("START_DATE",startDate,queryString)

  #更換vendor id
  queryString <- gsub("END_DATE",endDate,queryString)
  

  result <- dbGetQuery(conn,queryString)
  return(result)
}

#總表
matrixList <- function(result){
  index <- 0
  loc1 = list()
  loc2 = list()
  loc3 = list()
  loc4 = list()
  loc5 = list()
  
  # R index start with 1
  while(index<10){
    
    loc1[index+1] <- sum(result$loc1== index)
    loc2[index+1] <- sum(result$loc2== index)
    loc3[index+1] <- sum(result$loc3== index)
    loc4[index+1] <- sum(result$loc4== index)
    loc5[index+1] <- sum(result$loc5== index)
    index <- index +1
  }
  
  
  location <- c(loc1_list,loc2_list,loc3_list,loc4_list,loc5_list)
  matrix(location,10,5,dimnames = list(c("球號0","球號1","球號2","球號3","球號4","球號5","球號6","球號7","球號8","球號9"),c("萬位","千位","百位","拾位","個位")))
}

# 均勻分配
# w愈接近1,p-value 0.05 , 95%至少
normalTest<-function(result){
  print(shapiro.test(result$loc1))
  print(shapiro.test(result$loc2))
  print(shapiro.test(result$loc3))
  print(shapiro.test(result$loc4))
  print(shapiro.test(result$loc5))
}

#單雙出現次數
parseSD <- function(result){
  loc <- result$loc1
  s<-0
  d<-0
  for (v in loc) {
    if(v%%2==0){
      d <- d+1
    }else{
      s <- s+1
    }
  }
  
  listSD<-list("single"=s,"double"=d)
  return(listSD)
}


result <- query("18LUCK",'2020-03-02','2020-03-04')

dim(result)

matrixList(result)
normalTest(result)

v <-parseSD(result)
v

sum(result$loc1)

#close connection
closeConnection(conn)
