#陣列顯示
frequenciesLocation <- function(list){

  loop <- 0
  for (v in list[[1]]){
    loop <- loop + v
  }  

  plist1 <- list()
  plist2 <- list()
  plist3 <- list()
  plist4 <- list()
  plist5 <- list()
  index <-1
  for (v in list[[1]]){
    plist1[index] <- round(v/loop,digits = 5)*10
    index <- index +1
  }
  index <-1
  for (v in list[[2]]){
    plist2[index] <- round(v/loop,digits = 5)*10
    index <- index +1
  }
  index <-1
  for (v in list[[3]]){
    plist3[index] <- round(v/loop,digits = 5)*10
    index <- index +1
  }
  index <-1
  for (v in list[[4]]){
    plist4[index] <- round(v/loop,digits = 5)*10
    index <- index +1
  }
  index <-1
  for (v in list[[5]]){
    plist5[index] <- round(v/loop,digits = 5)*10
    index <- index +1
  }
  
  location <- c(list[[1]],plist1,list[[2]],plist2,list[[3]],plist3,list[[4]],plist4,list[[5]],plist5)
  m <- matrix(location,10,10,dimnames = list(c("球號0","球號1","球號2","球號3","球號4","球號5","球號6","球號7","球號8","球號9"),
                                            c("萬位","機率","千位","機率","百位","機率","拾位","機率","個位","機率")))
  return(m)
  
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
  
  n <- list(summary(l1),summary(l2),summary(l3),summary(l4),summary(l5),shapiro.test(l1),shapiro.test(l2),shapiro.test(l3),shapiro.test(l4),shapiro.test(l5))
  return(n)
}

ttest<-function(loc1,loc2,loc3,loc4,loc5){
  l1 <- c(loc1[1],loc1[2],loc1[3],loc1[4],loc1[5],loc1[6],loc1[7],loc1[8],loc1[9],loc1[10])
  l2 <- c(loc2[1],loc2[2],loc2[3],loc2[4],loc2[5],loc2[6],loc2[7],loc2[8],loc2[9],loc2[10])
  l3 <- c(loc3[1],loc3[2],loc3[3],loc3[4],loc3[5],loc3[6],loc3[7],loc3[8],loc3[9],loc3[10])
  l4 <- c(loc4[1],loc4[2],loc4[3],loc4[4],loc4[5],loc4[6],loc4[7],loc4[8],loc4[9],loc4[10])
  l5 <- c(loc5[1],loc5[2],loc5[3],loc5[4],loc5[5],loc5[6],loc5[7],loc5[8],loc5[9],loc5[10])
  avg <- mean(l1)
  n<- list(t.test(l1,mu=avg),t.test(l2,mu=avg),t.test(l3,mu=avg),t.test(l4,mu=avg),t.test(l5,mu=avg))
  return(n)
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

#出現次數
frequenciesUnique<- function(result){
  loop <- length(result$loc1)
  index <- 1
  onePairs <- 0
  twoPairs <- 0
  triple <- 0
  fourOfKind <-0
  fullHouse <-0
  fiveOfKind <- 0
  diffFive <-0
  while(index<loop){
    l<-list(result$loc1[index],result$loc2[index],result$loc3[index],result$loc4[index],result$loc5[index])  
    x<-unique(l)
    if(length(x)==5){
      #每個都不同
      diffFive <- diffFive +1
    }else if(length(x)==4){
      # 一對
      onePairs <- onePairs+1
    }else if(length(x)==3){
      #三條或兩對
      
      v1 <- x[1]
      v2 <- x[2]
      v3 <- x[3]
      
      v1c<-0
      v2c<-0
      v3c<-0
      for(v in l){
        if(v == v1){
          v1c <- v1c +1
        }else if(v == v2){
          v2c <- v2c +1
        }else{
          v3c <- v3c +1
        }
      }
      # 
      if(v1c == 3 || v2c==3 || v3c ==3){
        triple <- triple+1
      }else{
        twoPairs <- twoPairs+1
      }
      
    }else if (length(x)==2){
      # 葫蘆或鐵支
      v1 <-l[1]
      c <- 0
      
      for(v in l){
        if(v==v1){
          c <- c+1
        }
      }
      
      if(c == 3){
        fullHouse<-fullHouse +1
      }else{
        fourOfKind <- fourOfKind +1
      }
      
    }else{
      fiveOfKind <- fiveOfKind +1
    }
    
    index<-index+1
  }
  
  pOnePairs <-round(onePairs/loop,digits = 4)
  pTwoPairs <-round(twoPairs/loop,digits = 4)
  pTriple <- round(triple/loop,digits = 4)
  pFullHouse <- round(fullHouse/loop,digits = 4)
  pFourOfKind <-round(fourOfKind/loop,digits = 4)
  pFiveOfKind <- round(fiveOfKind/loop,digits = 4)
  l<- list(onePairs,twoPairs,triple,fullHouse,fourOfKind,fiveOfKind,pOnePairs,pTwoPairs,pTriple,pFullHouse,pFourOfKind,pFiveOfKind)
  m <- matrix(l,6,2,dimnames = list(c("一對(Pair)","二對(TwoPair)","三條(Triple)","葫蘆(Full House)","鐵支(Kind of four)","五枚(kind of five)"),c("次數(Frequence)","機率(Probability)")))
  return(m)
}

#單雙出現次數
frequenciesSD <- function(result){
  loc1 <- singleDouble(result$loc1)
  loc2 <- singleDouble(result$loc2)
  loc3 <- singleDouble(result$loc3)
  loc4 <- singleDouble(result$loc4)
  loc5 <- singleDouble(result$loc5)
  loop <- length(result$loc1)
  l11 <- round(loc1[[1]]/loop, digits = 4)
  l12 <- round(loc1[[2]]/loop, digits = 4)
  
  l21 <- round(loc2[[1]]/loop, digits = 4)
  l22 <- round(loc2[[2]]/loop, digits = 4)
  
  l31 <- round(loc3[[1]]/loop, digits = 4)
  l32 <- round(loc3[[2]]/loop, digits = 4)
  
  l41 <- round(loc4[[1]]/loop, digits = 4)
  l42 <- round(loc4[[2]]/loop, digits = 4)
  
  l51 <- round(loc5[[1]]/loop, digits = 4)
  l52 <- round(loc5[[2]]/loop, digits = 4)
  
  loc <- list(loc1[[1]],loc1[[2]],l11,l12,loc2[[1]],loc2[[2]],l21,l22,loc3[[1]],loc3[[2]],l31,l32,loc4[[1]],loc4[[2]],l41,l42,loc5[[1]],loc5[[2]],l51,l52)
  m <- matrix(loc,2,10,dimnames = list(c("單","雙"),c("萬位","機率值","千位","機率值","百位","機率值","拾位","機率值","個位","機率值")))
  return(m)
}

title<- function(game,crawler,startDate,endDate,result){
  # show title
  loc <- list(game,crawler,startDate,endDate,length(result$loc1)," ")
  m <- matrix(loc,1,6,dimnames = list(c("查詢條件"),c(" ","玩法","獎源","開始","結束","筆數")))
  return(m)  
}

report <- function(conn,game,crawler,startDate,endDate){
  result <- query(conn,game,crawler,startDate,endDate)
  
  ttl <- title(game,crawler,startDate,endDate,result)
  
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
  
  fn = paste0(game,"-",startDate,"-",endDate,".csv")
  write.table(x = ttl,file = fn,append = F,sep = ',')
  write.table(x = fl,file = fn,append = T,sep = ',')
  write.table(x = fre,file = fn,append = T,sep = ',')
  write.table(x = fu,file = fn,append = T,sep = ',')
  #write.table(x = n,file = fn,append = T,sep = ',')
  #write.table(x = t,file = fn,append = T,sep = ',')
}
