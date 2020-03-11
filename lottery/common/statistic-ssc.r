#陣列顯示
frequenciesLocation <- function(list){
  
  location <- c(list[[1]],list[[2]],list[[3]],list[[4]],list[[5]])
  m <- matrix(location,10,5,dimnames = list(c("球號0","球號1","球號2","球號3","球號4","球號5","球號6","球號7","球號8","球號9"),c("萬位","千位","百位","拾位","個位")))
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

#出現次數
frequenciesUnique<- function(result){
  loop <- length(result$loc1)
  index <- 1
  pairs <- 0
  triple <- 0
  fourOfKind <-0
  fullHouse <-0
  fiveOfKind <- 0
  while(index<loop){
    l<-list(result$loc1[index],result$loc2[index],result$loc3[index],result$loc4[index],result$loc5[index])  
    x<-unique(l)
    if(length(x)==5){
      #每個都不同 
    }else if(length(x)==4){
      pairs <- pairs+1
    }else if(length(x)==3){
      triple <- triple+1
    }else if (length(x)==2){
      
      v1 <-l[1]
      c <-0
      
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
  
  px <-round(pairs/loop,digits = 4)
  pt <- round(triple/loop,digits = 4)
  pfh <- round(fullHouse/loop,digits = 4)
  pfk <-round(fourOfKind/loop,digits = 4)
  pff <- round(fiveOfKind/loop,digits = 4)
  l<- list(pairs,triple,fullHouse,fourOfKind,fiveOfKind,px,pt,pfh,pfk,pff)
  m <- matrix(l,5,2,dimnames = list(c("對子(Pair)","三條(Triple)","葫蘆(Full House)","鐵支(Kind of four)","五枚(kind of five)"),c("次數(Frequence)","機率(Probability)")))
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
