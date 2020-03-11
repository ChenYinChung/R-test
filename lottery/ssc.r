path<-getwd()
source(paste0(path,"/lottery/db01-sle.r"))

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

#陣列顯示
showMatrix <- function(list){
  location <- c(list[[1]],list[[2]],list[[3]],list[[4]],list[[5]])
  m <- matrix(location,10,5,dimnames = list(c("球號0","球號1","球號2","球號3","球號4","球號5","球號6","球號7","球號8","球號9"),c("萬位","千位","百位","拾位","個位")))
  print(m)
}


#總表計算
count <- function(result){
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
  newList <- list(loc1,loc2,loc3,loc4,loc5)
  return(newList)
}

# 均勻分配檢定
# w愈接近1,p-value 0.05 , 95%至少
normalTest<-function(loc1,loc2,loc3,loc4,loc5){
  
  l1 <- c(loc1[1],loc1[2],loc1[3],loc1[4],loc1[5],loc1[6],loc1[7],loc1[8],loc1[9],loc1[10])
  l2 <- c(loc2[1],loc2[2],loc2[3],loc2[4],loc2[5],loc2[6],loc2[7],loc2[8],loc2[9],loc2[10])
  l3 <- c(loc3[1],loc3[2],loc3[3],loc3[4],loc3[5],loc3[6],loc3[7],loc3[8],loc3[9],loc3[10])
  l4 <- c(loc4[1],loc4[2],loc4[3],loc4[4],loc4[5],loc4[6],loc4[7],loc4[8],loc4[9],loc4[10])
  l5 <- c(loc5[1],loc5[2],loc5[3],loc5[4],loc5[5],loc5[6],loc5[7],loc5[8],loc5[9],loc5[10])
  
  print(summary(l1))
  writeLines("")
  print(summary(l2))
  writeLines("")
  print(summary(l3))
  writeLines("")
  print(summary(l4))
  writeLines("")
  print(summary(l5))
  writeLines("")
  print(shapiro.test(l1))
  print(shapiro.test(l2))
  print(shapiro.test(l3))
  print(shapiro.test(l4))
  print(shapiro.test(l5))
  
}

ttest<-function(loc1,loc2,loc3,loc4,loc5){
  l1 <- c(loc1[1],loc1[2],loc1[3],loc1[4],loc1[5],loc1[6],loc1[7],loc1[8],loc1[9],loc1[10])
  l2 <- c(loc2[1],loc2[2],loc2[3],loc2[4],loc2[5],loc2[6],loc2[7],loc2[8],loc2[9],loc2[10])
  l3 <- c(loc3[1],loc3[2],loc3[3],loc3[4],loc3[5],loc3[6],loc3[7],loc3[8],loc3[9],loc3[10])
  l4 <- c(loc4[1],loc4[2],loc4[3],loc4[4],loc4[5],loc4[6],loc4[7],loc4[8],loc4[9],loc4[10])
  l5 <- c(loc5[1],loc5[2],loc5[3],loc5[4],loc5[5],loc5[6],loc5[7],loc5[8],loc5[9],loc5[10])
  avg <- mean(l1)
  print(t.test(l1,mu=avg))
  print(t.test(l2,mu=avg))
  print(t.test(l3,mu=avg))
  print(t.test(l4,mu=avg))
  print(t.test(l5,mu=avg))
}

singleDouble <- function (loc){
  s<-0
  d<-0
  for (v in loc) {
    if(v%%2==0){
      d <- d+1
    }else{
      s <- s+1
    }
  }
  
  listSD<-list(s,d)
  return(listSD)
}


loc.unique<- function(result){
  loop <- length(result$loc1)
  index <- 1
  pairs <- 0
  triple <- 0
  fokOrfh <-0
  while(index<loop){
    l<-list(result$loc1[index],result$loc2[index],result$loc3[index],result$loc4[index],result$loc5[index])  
    x<-unique(l)
    if(length(x)==4){
      pairs <- pairs+1
    }else if(length(x)==3){
      triple <- triple+1
    }else{
      fokOrfh <- fokOrfh+1
    }
    
    index<-index+1
  }
  
  px <-pairs/loop
  print(paste("總數=",loop))
  print(paste("對子機率值：",px))
  
}

#單雙出現次數
parseSD <- function(result){
  loc1 <- singleDouble(result$loc1)
  loc2 <- singleDouble(result$loc2)
  loc3 <- singleDouble(result$loc3)
  loc4 <- singleDouble(result$loc4)
  loc5 <- singleDouble(result$loc5)
  loc <- list(loc1[[1]],loc1[[2]],loc2[[1]],loc2[[2]],loc3[[1]],loc3[[2]],loc4[[1]],loc4[[2]],loc5[[1]],loc5[[2]])
  m <- matrix(loc,2,5,dimnames = list(c("單","雙"),c("萬位","千位","百位","拾位","個位")))
  print(m)
}

# vendorId : 18LUCK , MANBETX ...
# kind : NYSSC30S 
# startDate : 2020-03-01
# endDate   : 2020-03-09
main <- function (vendorId,gameId,startDate,endDate){
  loc <- list(vendorId,gameId,startDate,endDate)
  m <- matrix(loc,1,4,dimnames = list(c("查詢條件"),c("廠商","玩法","起始","結束")))
  print(m)
  
  
  conn<-createConnection()
  
  
  
  result <- query(conn,vendorId,gameId,startDate,endDate)
  
  print(paste("total count =",length(result$loc1)))
  
  newList = count(result)
  showMatrix(newList)
  
  parseSD(result)
  loc.unique(result)
  
  normalTest(as.numeric(newList[[1]]),as.numeric(newList[[2]]),as.numeric(newList[[3]]),as.numeric(newList[[4]]),as.numeric(newList[[5]]))
  ttest(as.numeric(newList[[1]]),as.numeric(newList[[2]]),as.numeric(newList[[3]]),as.numeric(newList[[4]]),as.numeric(newList[[5]]))
  
  closeConnection(conn)
}

#getReport('18LUCK','NYSSC30S','2020-03-01','2020-03-09')

#for local test
#result <- main("18LUCK",'NYSSC30S','2020-03-01','2020-03-09')
#print(paste("total count =",length(result$loc1)))

#print(length(result$loc1))
#print(length(result$loc2))
#print(length(result$loc3))
#print(length(result$loc4))
#print(length(result$loc5))
