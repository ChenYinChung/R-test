library(ggplot2)

#讀入csv檔案
# step 1
dt<-read.csv("CVD_ALL.csv",sep = ',')

#step 2 偵測資料中是有遺失值
na.fail(dt)

#step 3 若資料中有遺失值，要先去除遺失值
dt_ok=na.omit(dt)

#步驟4 年齡平均值
mean(dt_ok$年齡)

# 中位數
median(dt_ok$年齡)

# 最小值
min(dt_ok$年齡)

# 最大值
max(dt_ok$年齡)

# 百分位數 
quantile(dt_ok$年齡)

#敘述統計量，包含最小值、第25百分位數、中位數、平均 數、第75百分位數和最大值
summary(dt_ok$年齡)


eason <- sample(0:9, 5000,replace = T)
shapiro.test(eason)


Len <- c(14.3, 15.8, 14.6, 16.1, 12.9, 15.1, 17.3, 14.0, 14.5, 13.9, 16.2, 14.3, 14.6, 13.3, 15.5, 11.8, 14.8, 13.5, 16.3, 15.4, 15.5, 13.9, 10.7, 14.8, 12.9, 15.4)
shapiro.test(Len)




shapiro <- shapiro.test(Len)
fmeOutput<-data.frame(shapiro$statistic, shapiro$p.value)


# Compute P(0< X < 3) for X uniform(a=0,b=10)
punif(3,min = 0,max = 10)

curve(dunif(x, 0, 10), -2, 30)   #畫出uniform(0,10)

make.shadow <- function(xStart,xEnd,xIncr,func = dnorm, ...) #編制填上陰影區域
{
  middle = seq(xStart,xEnd,by=xIncr)
  x0 = c(xStart,middle,xEnd)
  y0 = c(0 , func(middle,...),0)
  return(list(x=x0,y=y0))
}
s=make.shadow(0,1.282,0.05,func = dnorm)                     #令s為由0到1.282的區域
curve(dnorm(x),-3,3)                                         #畫上常態分配
abline(h=0)                                                  #畫上底線
polygon(s$x,s$y,density = 40,angle = 45)                     #畫上陰影




curve(dexp(x,0.5),0,10,col="blue",add=F,ylab="f(x)")    #劃上exp(λ=0.5)圖形
curve(dexp(x,1),0,10,col="red",add=T)                   #加上exp(λ=1)圖形
curve(dexp(x,2),0,10,col="green",add=T)                 #加上exp(λ=2)圖形
legend( "topright",                                     #標記註解，位置在右上
        c("λ=0.5","λ=1","λ=2"),                         #註解內容
        col=c("blue","red","green"),                    #註解顏色
        lty=1)                                          #註解樣式


curve(dt(x,2),-3,3,col="blue",add=F,ylab="f(x)",ylim=c(0,0.5))    #劃上t(df=0.5)圖形
curve(dt(x,10),-3,3,col="red",add=T)                              #加上t(df=1)圖形
curve(dt(x,20),-3,3,col="green",add=T)                            #加上t(df=2)圖形
legend( "topright",                                               #標記註解，位置在右上
        c("df=0.5","df=1","df=2"),                                #註解內容
        col=c("blue","red","green"),                              #註解顏色
        lty=1)   



curve(df(x,10,100),0,6,col="blue",add=F,ylab="f(x)",ylim=c(0,2))  #劃上F(df=10,100)圖形
curve(df(x,100,10),0,6,col="red",add=T)                           #加上F(df=100,10)圖形
curve(df(x,100,100),0,6,col="green",add=T)                        #加上F(df=100,100)圖形
legend( "topright",                                               #標記註解，位置在右上
        c("df1=10,df2=100","df1=100,df2=10","df1=df2=100"),       #註解內容
        col=c("blue","red","green"),                              #註解顏色
        lty=1)   




x<- sample(x=0:9,size=100,replace=T)
# 畫出 x 的直方圖
ggplot(data.frame(x), aes(x)) + geom_histogram()

