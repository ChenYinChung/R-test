boy<-c(170,175,178,180)
girl<-c(160,165,168,150)

mean(boy)
mean(girl)

hist(boy)
hist(boy,freq = F)

# 常態性檢定
shapiro.test(boy)

barplot(boy)

sd(boy)
sd(girl)

#若X為一個常態分佈，其平均值為5，標準偏差為2。當X=3時，其密度函數曲線之高度為多少?
dnorm(3, mean = 5, sd= 2)

#假設台灣肉雞腹部脂肪(abdominal fat)站活體重的百分比呈常態分佈，其平均值為5%，標準偏差為1.5%。若在市場隨意買一隻肉雞，有多少可能其腹脂佔活體種百分比是
#大於8%
pnorm(8, mean = 5, sd= 1.5,lower.tail = F)
#小於2%
pnorm(2, mean = 5, sd= 1.5)
#剛好5%
pnorm(5, mean = 5, sd= 1.5)
#介於3~6%
pnorm(6, mean = 5, sd= 1.5)-pnorm(3, mean = 5, sd= 1.5)

#若服役年齡男子身高之分布呈常態，平均身高為172公分，標準偏差為6公分。如果即齡役男有50萬人，190公分以上因裝備問題而不徵召，需要徵召30萬兵員，身高幾公分以上的即齡男子需要當兵?
qnorm((pnorm(190, mean= 172, sd= 6)-30/50),mean=172, sd=6)

#蛋種雞每日平均採食量為98g，標準偏差為10g。若雞場有10000隻蛋種雞，每日至多供給多少飼料，才有90%機率讓飼槽不剩飼料?
#10000隻，mean=98*10000=980kg,sd^2=10*10000,sd=1kg
qnorm((1-0.9),mean= 980 ,sd= 1)

#蛋殼強度之平均值為3.8kg,變異係數為19.5%。若希望估計平均值與實際平均值的差異超過0.5kg的機率超過(a)1% (b)5%，需要測量幾枚蛋? 
#(a)1%
#C.V.=sd/mean, sd=19.5%*3.8=0.741
(qnorm(0.01/2)*0.741/0.5)^2
#(b)5%
(qnorm(0.05/2)*0.741/0.5)^2
#如果測量10枚蛋，估計平均值與實際平均值差異超過0.3kg的機率有多少?
pnorm((10*0.3^2/(0.741^2))^0.5,lower.tail = F)*2

#牧場圍籬工作，每公里平均需400工時，標準偏差為40工時。若牧場需圍籬1公里，不超過300工時就能完成圍籬的完成機率有多少?
pnorm(300,mean = 400,sd= 40)
#以350-490工時完成工作的機率
pnorm(490,mean = 400, sd= 40)-pnorm(350,mean = 400, sd= 40)
#2公里，以720小時完成之機率?
pnorm(720,mean=400*2,sd=(2^0.5)*40)

#自動販賣飲料機賣出之飲料量呈常態分佈，可以調整每杯飲料量的平均值，而根據以往經驗，其標準偏差為6ml，但無法調整。目前使用300ml的紙杯，請你調整平均飲料量使只有0.5%的機率飲料會溢出來(超過300ml)。
qnorm(0.005/2,mean=300, sd=6)

#在美國幾乎所有商學院研究所都要求入學申請者都必須繳交管理學研究所入學測試(GMAT)。其公司希望了解此測試的平均成績。逢機採樣100個測驗成績樣本，平均估計值為564，標準差估計值為70。
#GMPT平均值97%信賴區間
xbar <-564; sd<- 70; n<- 100
alpha = .03
tscore <- qt(1-alpha/2, df=n-1)
SE<- sd/sqrt(n)
c(xbar-tscore*SE, xbar+tscore*SE)

#若希望GMAT之平均成績97%信賴區間在10分以內，標準偏差為90分，則需取多少樣本已達需求?
#2*Zscore*sd/sqrt(n)<10
Zscore<-qnorm(0.03/2,lower.tail = F)
sd<-90
(2*Zscore*sd/10)^2

#假使肉豬出售時的體重呈常態分佈，標準偏差為5公斤。不希望超過10%的肉豬出售體重低於90公斤，肉豬平均要飼養至僅公斤才可出售?
qnorm(0.1,mean = 90,sd=5,lower.tail = F)
#出售10000頭豬，需要磅多少頭豬才可以估計這群豬的平均體重與實際平均體重差異超過0.5公斤的機率小於5%?
n<- 10000 ;alpha<-0.05; L<-0.5; sd<-5
N<- (qnorm(1-alpha/2)*sd/L)^2

#20個成年人血清鈣濃度資料如下
Ca<- c(10.46, 10.20, 12.46, 10.21, 11.39, 10.77, 9.49, 10.08, 11.37, 9.37, 11.68, 11.39, 10.63, 11.28, 9.74, 9.46, 9.72, 10.42, 7.99, 9.56)
mean.range<- function(x, alpha=0.1) {
  n = length(x) # n = 樣本數
  mx = mean(x) # mx 即為平均值 mu 的點估計
  S = sqrt(var(x)) # S 即為標準差的點估計
  r1 = qt(alpha/2, df=n-1) # 信賴區間，下半截掉 alpha/2
  r2 = qt(1-alpha/2, df=n-1) # 信賴區間，上半截掉 alpha/2
  L1 = mx+r1*S/sqrt(n) # 信賴區間下限
  L2 = mx+r2*S/sqrt(n) # 信賴區間上限
  range = c(L1, mx, L2)
}
r<- mean.range(Ca,alpha=0.1)
r
#炸雞店要求肉雞屠體規格需在1.2-1.36kg之間。若A電宰廠出售一批肉雞屠體，平均重1.28kg,亦呈常態分佈，標準差需多少以下才可使95%的肉雞符合炸雞店的要求?
mx<-1.28
1.2<-mx+qnorm(0.05/2)*sd
sd<- (1.2-mx)/qnorm(0.05/2)
sd

#試驗自五週齡開始使用1000之小公雞，今買進初生小公雞1500隻，若飼養至五周齡隻育成率為95%，而五週齡挺種呈常態分佈，平均為400g，標準偏差為40g。若要以體重適中者進行試驗請估計試驗開始時小工雞體重的上下限

livability<- 1500*0.95
Retention<-1000/livability
culling<- 1-Retention
R1<-qnorm(1-culling/2,mean=400,sd=40)
R2<-qnorm(culling/2,mean=400,sd=40)

#一般肉雞飼養標準要求上市體重之整齊度為偏離平均體重10%之雞之數目不超過20%。請問:以此標準而言，若雞群體重呈常態分佈，其變異係數應在多少以下?
"mean/sd"<-qnorm(1-0.2/2)/0.1
CV<-1/`mean/sd`

#如果雛雞孵化時間呈常態分佈，其平均值與標準偏差分別為500.5小時與5.5小時，我們入孵21.5日(516小時)後出雛，還未孵出都不要，則有多少部分的雛雞可能來不及孵出?
pnorm(516,mean=500.5,sd=5.5,lower.tail = F)
