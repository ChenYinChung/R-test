path<-getwd()
source(paste0(path,"/lottery/ssc.r"))

print("================= 18LUCK")
main("18LUCK",'NYSSC30S','2020-03-01','2020-03-10')
print("================= 18LUCK======================")


print("================= MANBETX")
main("MANBETX",'NYSSC30S','2020-03-01','2020-03-10')
print("================= MANBETX======================")

