getwd()
setwd("C:/Users/손지안/Desktop/R")
View()
listfiles()

a<- read.table("acc.txt" , header=TRUE ,sep="\t") 
dim(a)
head(a)

ex<-read.csv("acc.csv",head=TRUE)

res = lm(사상자수 ~ 사고유형코드, data=ex)
res
summary(res)  #r-squared 값:  17 퍼센트 설명한다  

res = lm(사상자수 ~ 사고연도, data=ex)
res
summary(res)  #0.8퍼센트


res = lm(사상자수 ~ 위도, data=ex)
res
summary(res)   #0.7퍼센트 설명한다 


res = lm(사상자수 ~ 경도, data=ex)
res
summary(res)  

 #0.03퍼센트 설명한다 

step(res)



#multivariate analysis
res = lm ( 사상자수 ~ 사고유형코드 + 사고연도 + 위도 + 경도+위치코드 + 경상자수 , data=ex)
res
summary(res)   

  #17퍼센트를 설명한다 

step(res)

#########################################################################

#chi-test

#어린이사고인지 아닌지
ex$acci<- ifelse(ex$사고유형코드>2,1,0)
ex$num<- ifelse(ex$사상자수>5,2,3)
chisq.test ( ex$acci, ex$num ) 

a3<- table ( ex$acci , ex$num )
a3

install.packages("Rcmdr")
library(Rcmdr)
rowPercents(a3)   
colPercents(a3)

#무단횡단사고인지 아닌지
ex$acci<- ifelse(ex$사고유형코드==5,1,0)
ex$num<- ifelse(ex$사상자수>5,2,3)
chisq.test ( ex$acci, ex$num ) 

a4<- table ( ex$acci , ex$num )
a4

install.packages("Rcmdr")
library(Rcmdr)
rowPercents(a4)   
colPercents(a4)
