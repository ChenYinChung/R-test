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

x <- c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5,6,6,6,6,6,7,7,7,7,7,8,8,8,8,8,9,9,9,9,9,0,0,0,0,0)
shapiro.test(x)
chisq.test(x)
