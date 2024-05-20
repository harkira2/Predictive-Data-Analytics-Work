d<-read_csv("salesbreakdown2.csv")
d_clean<-d


any(is.na(d_clean$CU_GENDER))
any(is.na(d_clean$CU_AGE_RANGE))

#Removing NAs in subdepartment columns
no_hotfood<-is.na(d_clean$hotfood)
d_clean$hotfood[no_hotfood]<-0

no_horticulture<-is.na(d_clean$horticulture)
d_clean$horticulture[no_horticulture]<-0

no_housewares<-is.na(d_clean$housewares)
d_clean$housewares[no_housewares]<-0

no_bakery<-is.na(d_clean$bakery)
d_clean$bakery[no_bakery]<-0

no_water<-is.na(d_clean$water)
d_clean$water[no_water]<-0

no_pizza<-is.na(d_clean$pizza)
d_clean$pizza[no_pizza]<-0

no_sandwichmeat<-is.na(d_clean$sandwichmeat)
d_clean$sandwichmeat[no_sandwichmeat]<-0

no_20<-is.na(d_clean$sub20)
d_clean$sub20[no_20]<-0

no_4<-is.na(d_clean$sub4)
d_clean$sub4[no_4]<-0

no_97<-is.na(d_clean$sub97)
d_clean$sub97[no_97]<-0

no_22<-is.na(d_clean$sub22)
d_clean$sub22[no_22]<-0

no_28<-is.na(d_clean$sub28)
d_clean$sub28[no_28]<-0

no_21<-is.na(d_clean$sub21)
d_clean$sub21[no_21]<-0

#Factoring the Gender and Age Range Categories
d_clean$CU_GENDER<-factor(d_clean$CU_GENDER)
d_clean$CU_AGE_RANGE<-factor(d_clean$CU_AGE_RANGE)

#Create a new category to find people over 50 and under 50
d_clean<-mutate(d_clean,over50=ifelse(d_clean$CU_AGE_RANGE=="50-59" |d_clean$CU_AGE_RANGE=="60+", "Over", "Under"))
d_clean$over50<-factor(d_clean$over50)

rowSample<-sample(nrow(d_clean),nrow(d_clean)*2/3)
d_clean.train<-d_clean[rowSample,]
d_clean.rest<-d_clean[-rowSample,]

rowSample2<-sample(nrow(d_clean.rest),0.5*nrow(d_clean.rest))
d_clean.val<-d_clean.rest[rowSample2,]
d_clean.test<-d_clean.rest[-rowSample2,]


##KNN SECTION
#Gender
#k=5 and only Sales and Quantity predicting Gender
pred.train_1<-knn(d_clean.train[,c(2,4)],d_clean.val[,c(2,4)],d_clean.train$CU_GENDER,k=5)
table(d_clean.val$CU_GENDER,pred.train_1)

#k=5 and only Sales,Quantity, Hot Food, Housewares, Horticulture,Bakery,Pizza, Meat predicting Gender
pred.train_1<-knn(d_clean.train[,c(2,4,8,9,11,13,14)],d_clean.val[,c(2,4,8,9,11,13,14)],d_clean.train$CU_GENDER,k=5)
table(d_clean.val$CU_GENDER,pred.train_1)

#k=5 and only Sales,Quantity and all subdepartments predicting Gender
pred.train_1<-knn(d_clean.train[,c(2,4,8,9,10,11,12,13,14,15,16,17,18,19,20)],d_clean.val[,c(2,4,8,9,10,11,12,13,14,15,16,17,18,19,20)],d_clean.train$CU_GENDER,k=5)
table(d_clean.val$CU_GENDER,pred.train_1)

#k=5 and only no Sales and Quantity and all subdepartments predicting Gender
pred.train_1<-knn(d_clean.train[,c(8,9,10,11,12,13,14,15,16,17,18,19,20)],d_clean.val[,c(8,9,10,11,12,13,14,15,16,17,18,19,20)],d_clean.train$CU_GENDER,k=5)
table(d_clean.val$CU_GENDER,pred.train_1)

#Age range
#k=5 and only Sales and Quantity predicting Age
pred.train_1<-knn(d_clean.train[,c(2,4)],d_clean.val[,c(2,4)],d_clean.train$over50,k=5)
table(d_clean.val$over50,pred.train_1)

#k=5 and only Sales,Quantity and some subdepartments predicting Age
pred.train_1<-knn(d_clean.train[,c(2,4,8,9,10,11,12,13,14)],d_clean.val[,c(2,4,8,9,10,11,12,13,14)],d_clean.train$over50,k=5)
table(d_clean.val$over50,pred.train_1)

#k=5 and only Sales,Quantity and all subdepartments predicting Age
pred.train_1<-knn(d_clean.train[,c(2,4,8,9,10,11,12,13,14,15,16,17,18,19,20)],d_clean.val[,c(2,4,8,9,10,11,12,13,14,15,16,17,18,19,20)],d_clean.train$over50,k=5)
table(d_clean.val$over50,pred.train_1)

#k=5 and only no Sales and Quantity and all subdepartments predicting Age
pred.train_1<-knn(d_clean.train[,c(8,9,10,11,12,13,14,15,16,17,18,19,20)],d_clean.val[,c(8,9,10,11,12,13,14,15,16,17,18,19,20)],d_clean.train$over50,k=5)
table(d_clean.val$over50,pred.train_1)

##LDA SECTION

attach(d_clean.train)
#GENDER
#LDA with sales and quantity only
lda.fit<-lda(CU_GENDER~sales+quantity,data=d_clean.train)
lda.pred<-predict(lda.fit,d_clean.val)
lda.class<-lda.pred$class
table(d_clean.val$CU_GENDER,lda.class)

#LDA with sales, quanity and subdepartments
lda.fit<-lda(CU_GENDER~sales+quantity+hotfood+horticulture+housewares+bakery+water+pizza+sandwichmeat+sub20+sub4+sub97+sub22+sub28+sub21,data=d_clean.train)
lda.pred<-predict(lda.fit,d_clean.val)
lda.class<-lda.pred$class
table(d_clean.val$CU_GENDER,lda.class)

#LDA with subdepartments
lda.fit<-lda(CU_GENDER~hotfood+horticulture+housewares+bakery+water+pizza+sandwichmeat+sub20+sub4+sub97+sub22+sub28+sub21,data=d_clean.train)
lda.pred<-predict(lda.fit,d_clean.val)
lda.class<-lda.pred$class
table(d_clean.val$CU_GENDER,lda.class)

#AGE RANGE
lda.fit<-lda(over50~sales+quantity,data=d_clean.train)
lda.pred<-predict(lda.fit,d_clean.val)
lda.class<-lda.pred$class
table(d_clean.val$over50,lda.class)

#LDA with sales, quanity and subdepartments
lda.fit<-lda(over50~sales+quantity+hotfood+horticulture+housewares+bakery+water+pizza+sandwichmeat+sub20+sub4+sub97+sub22+sub28+sub21,data=d_clean.train)
lda.pred<-predict(lda.fit,d_clean.val)
lda.class<-lda.pred$class
table(d_clean.val$over50,lda.class)

#LDA with subdepartments
lda.fit<-lda(over50~hotfood+horticulture+housewares+bakery+water+pizza+sandwichmeat+sub20+sub4+sub97+sub22+sub28+sub21,data=d_clean.train)
lda.pred<-predict(lda.fit,d_clean.val)
lda.class<-lda.pred$class
table(d_clean.val$over50,lda.class)
