library("readxl")
data102<-as.data.frame(t(read_excel("C:/Users/林欣怡/OneDrive/桌面/102.xlsx")))
data103<-as.data.frame(t(read_excel("C:/Users/林欣怡/OneDrive/桌面/103.xlsx")))
data104<-as.data.frame(t(read_excel("C:/Users/林欣怡/OneDrive/桌面/104.xlsx")))
data105<-as.data.frame(t(read_excel("C:/Users/林欣怡/OneDrive/桌面/105.xlsx")))

data102<-data102[-c(1,2),-1]
ID<-c("1","2","3","4","5","6")
data102$ID<-ID
data102$year<-102

data103<-data103[-c(1,2),-1]
ID<-c("1","2","3","4","5","6")
data103$ID<-ID
data103$year<-103

data104<-data104[-c(1,2),-1]
ID<-c("1","2","3","4","5","6")
data104$ID<-ID
data104$year<-104

data105<-data105[-c(1,2),]
ID<-c("1","2","3","4","5","6")
data105$ID<-ID
data105$year<-105

colnames(data105)<-colnames(data104)

data<-rbind(data102,data103,data104,data105)

data2<-cbind(data$ID,data$year,data[,1:26])

rownames(data2)<-NULL

for(i in 3:28){
  data2[,i]<-as.numeric(data2[,i])
}

data2$score<-(data2$V21*-2+data2$V22*-1+data2$V23*0+data2$V24*1+data2$V25*2)/100

#PDS
library(glmnet)
fit <- glmnet(x = as.matrix(data2[,-c(1,2,22:26,28)]), y = data2[,28], alpha = 0.08)
coef(fit,s=0.5)
#V7+V8+V9+V10+V12+V20

#forward selection
library(MASS)
null <- lm(V27 ~1, data = data2)
full <- lm(V27 ~V7+V8+V9+V10+V12+V20, data = data2)
fwmodel <- step(full, scope=list(lower=null, upper=full), direction = "forward")
summary(fwmodel)

#V7+V8+V9+V12+V20
#V7+V9+V12

data3<-as.data.frame(cbind(data2[,1],data2[,2],data2$V7,data2$V9,data2$V12,data2$V26,data2$V27))
colnames(data3)<-c("ID","Year","Leisure","Self","Child","House","Rate")

#DiD
for(i in 1:7){
  data3[,i]<-as.numeric(data3[,i])
}

data3$treated<-ifelse(data3$ID>3,1,0)
data3$time<-ifelse(data3$Year>103,1,0)

didmodel = lm(Rate ~ treated + time + treated*time + House + Leisure + Self + Child, data = data3)

summary(didmodel)

#residual test
shapiro.test(didmodel$residual) #normality
library(car)
durbinWatsonTest(didmodel) #independence
ncvTest(didmodel) #Non-constant Variance

#plot
data3$predicted <- predict(didmodel)
ggplot(data3, aes(House, Rate)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "green") +
  theme_minimal()
             