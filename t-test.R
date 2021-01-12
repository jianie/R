getwd()
setwd("C:/Users/손현진/Desktop/R")
View()
listfiles()


ex<-read.csv("acc.csv",head=TRUE)
ex[1:3 , 1:10]

#Test of normal distribution
qqnorm(ex$사상자수)
qqline(ex$사상자수)

dim(ex)
aa<-ex[1:5000,]
shapiro.test(aa$사상자수)  

#Test for equality of two variances
ex$코드<- ifelse(ex$사고유형코드==5,1,0)
var.test(사상자수 ~ 코드 , data=ex , conf.level = 0.95)

#T-Test
t.test(사상자수 ~ 코드, data=ex,var.equal=FALSE, conf.level = 0.95 )

#anova
out=aov ( 사상자수 ~  factor (사고유형구분) , data=ex )
summary(out) 

#pearson correlation
head(ex)
plot (위도 ~ 사상자수 , data=ex )

#사후검정
install.packages('agricolae')
library(agricolae)
TukeyHSD(out)
plot ( TukeyHSD(out) )
