getwd()
setwd("C:/Users/손/Desktop/R")
View()
listfiles()

o1<- read.table("1_EMR data_sex.txt" , header=TRUE ,sep="\t") 
dim(o1)
a1<-o1
a1<- a1[complete.cases(a1) , ] 
dim(a1)
a1[1:10 , ]        # a1 ( pid-sex)

o2<- read.table("2_APP data_e.txt" , header=TRUE ,sep="\t")
dim(o2)
data2<- o2[ , c(1,3)  ] 
dim(data2)
data2[1:10 , ]    # data2 ( id - steps - pid)

data2$pid=data2$A_myhealth_id
data2[ 1:10 ,  ] 
data2_1<-data2[  , c(2:3)  ]
a2<- data2_1
a2 [ 1:10 ,  ]  # a2 ( steps - pid )
dim(a2) 

#merge 이용해서 합치기
r2<-merge ( a1, a2, key=pid )  
r2[1:20 , ]  
dim(r2)

#한사람당 한줄씩 나오게 만들기
r4 <- aggregate (r2$A_steps , list ( r2$pid ) , mean )
r4 [1:20 , ]   #한사람당 step 의 평균

r5<-aggregate ( r2$sex, list ( r2$pid) , mean )
r5 [1:20 , ]  #한사람당 sex 의 평균
r5$sex=r5$x   # sex평균x 를 sex로복제
r5[1:20 , ]
r7<-r5 [ , c(1,3) ] #r5의 첫번째, 3번째 줄만 select
r7[1:10, ]

r8<- merge ( r4, r7, key = Group.1 )
r8[1:10 , ]  # 최종 합쳐진 데이터 ( Group.1-x / Group1-sex)
dim(r8)

r9<- aggregate ( r8$x , list ( r8$sex), mean)
r9
r4<-aggregate( r2$A_steps , list (r2$sex) , mean )
r4

#정규성검정 중심극한정리에의해 한 그룹당 30건 이상이 되면 정규분포를 따른다 
qqnorm(r8$x)  
qqline(r8$x)

hist(r8$x)

shapiro.test(r8$x)  #p-value가 0.05보다 커야 정규분포 =>작으므로 정규분포따르지 않음 
                   #중심극한정리에따라 모수적인 방법 이용한다 

#t-test - pvalue가 0.05보다 크니까 두 성별간의 평균 step차이가 통계적으로 유의미한 차이가 없다 
t.test(x ~ sex, data=r8,var.equal=FALSE, conf.level = 0.95 )

#등분산검정 0.05보다 작으니 두 군의 steps 수 퍼진 정도(분산) 가 다르다 
var.test(x ~ sex , data=r8 , conf.level = 0.95)
