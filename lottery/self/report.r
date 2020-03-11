path<-getwd()
source(paste0(path,"/lottery/ssc.r"))

#
#自開彩獎號驗證，以Production實際內容比對
#
#

print("================= 18LUCK")
main("18LUCK",'NYSSC30S','2020-03-01','2020-03-10')
print("================= 18LUCK======================")


print("================= MANBETX")
main("MANBETX",'NYSSC30S','2020-03-01','2020-03-10')
print("================= MANBETX======================")

