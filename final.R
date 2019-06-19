mda<-read.csv("C:/Users/AKHIL/Desktop/R csv/modeldata.csv",na.strings = c("","NA"))
colnames(mda)
View(mda)
dim(mda)
mda1<-mda[,c(-1,-2,-6,-8,-9,-11,-13,-17)]
View(mda1)
colnames(mda1)
library(dplyr)
levels(mda1$signup_category)
levels(mda1$business_group)
colnames(mda1)[5]<-"signup"
colnames(mda1)[6]<-"country"
colnames(mda1)[4]<-"business"
colnames(mda1)[7]<-"download"
##########################################################################################

library(dplyr)
mda1<-mutate(mda1,skill1=case_when(skill=="BEGINNER"~0,skill=="EXPERIENCED"~1,skill=="INTERMEDIATE"~2,skill=="MIXED"~3,skill=="UNKNOWN"~4))
mda1<-mutate(mda1,signup1=case_when(signup=="`CREATIVE SDK`"~0,signup=="DESKTOP"~1,signup=="MAPPING TBD"~2,signup=="MOBILE"~3,signup=="SERVICE"~4))
View(mda1)
mda1$skill1<-factor(mda1$skill1)
mda1$signup1<-factor(mda1$signup1)
colnames(mda1)
mda2<-mda1[,c(-1,-5)]
View(mda2)


#Removing Duplicate
################################
table(mda2$label)
m1<-filter(mda2,label==0)
nrow(g1)
m3<-unique(g1)
nrow(g3)
m2<-filter(mda2,label==1)
nrow(g2)
mda2<-rbind(m2,m3)
nrow(data)
table(mda2$label)
########################################################################################################################

mda2f<-filter(mda2,product_platforms=="MAC")
View(mda2f)
nrow(mda2f)
mda2wm<-filter(mda2,product_platforms=="MAC:WIN")
nrow(mda2wm)
mda2w<-filter(mda2,product_platforms=="WIN")
nrow(mda2w)
sum(is.na(mda2$product_platforms))#####count of na value##########################
mda2$product_platforms[is.na(mda2$product_platforms)] <- "WIN" ####replacing na with WIN
View(mda2)
######################################################################################################################

mda2a<-filter(mda2,job=="ACADEMIC")
nrow(mda2a)
mda2b<-filter(mda2,job=="BIZ_PRO")
nrow(mda2b)
mda2d<-filter(mda2,job=="DEVELOPER")
nrow(mda2d)
mda2h<-filter(mda2,job=="HOBBYIST")
nrow(mda2h)
mda2i<-filter(mda2,job=="IT_PRO")
nrow(mda2i)
mda2bz<-filter(mda2,job=="BIZ_PRO")
nrow(mda2bz)
mda2m<-filter(mda2,job=="MARKETING")
nrow(mda2m)
mda2o<-filter(mda2,job=="OTHER")
nrow(mda2o)
mda2p<-filter(mda2,job=="PHOTOGRAPHER")
nrow(mda2p)
mda2pr<-filter(mda2,job=="PRINTGRAPHICDESIGN")
nrow(mda2pr)
mda2pu<-filter(mda2,job=="PURCHASEMANAGER")
nrow(mda2pu)
mda2st<-filter(mda2,job=="STUDENT")
nrow(mda2st)
mda2un<-filter(mda2,job=="UNEXPECTEDVALUE")
nrow(mda2un)
mda2v<-filter(mda2,job=="VIDEO")
nrow(mda2v)
mda2we<-filter(mda2,job=="WEBMOBILEDESIGN")
nrow(mda2we)
sum(is.na(mda2$job))
mda2$job[is.na(mda2$job)] <- "STUDENT" ####replacing na with STUDENT
View(mda2)

#######################################################################################################################

mda2me<-filter(mda2,purpose=="ME_NONPROFESSIONAL")
nrow(mda2me)
mda2mep<-filter(mda2,purpose=="ME_PROFESSIONAL")
nrow(mda2mep)
mda2org<-filter(mda2,purpose=="ORG_INVOLVED")
nrow(mda2org)
mda2or<-filter(mda2,purpose=="ORG_NOTINVOLVED")
nrow(mda2or)
mda2une<-filter(mda2,purpose=="UNEXPECTEDVALUE")
nrow(mda2une)
sum(is.na(mda2$purpose))
mda2$purpose[is.na(mda2$purpose)] <- "ME_PROFESSIONAL" ####replacing na with me professional
View(mda2)
##################################################################################################################

mda2na<-filter(mda2,machines=="1")
nrow(mda2na)
mda2$machines[is.na(mda2$machines)] <- "1" ####replacing na with 1
View(mda2)
##################################################################################################################

mda2d<-filter(mda2,download=="1")
nrow(mda2d)
mda2$download[is.na(mda2$download)] <- "1" ####replacing na with 1
View(mda2)

###################################################################################################################
mda2$label[is.na(mda2$label)] <- "0"
mda2$label<-factor(mda2$label)
################################################################################################################
mda2$job1<-as.numeric(as.factor(mda2$job))
View(mda2)
mda2$purpose1<-as.numeric(as.factor(mda2$purpose))
View(mda2)
mda2$business1<-as.numeric(as.factor(mda2$business))
View(mda2)
mda2$country1<-as.numeric(as.factor(mda2$country))
View(mda2)
################################################################################################################
mda2$product_platforms1<-as.numeric(as.factor(mda2$product_platforms))
levels(factor(mda2$machines))
mda2$machines<-as.numeric(as.factor(mda2$machines))
View(mda2)
mda2$download<-factor(mda2$download)
levels(factor(mda2$job))
mda2<-mutate(mda2,job2=case_when((job1<=7~1),(job1>7~2)))
View(mda2)
levels(factor(mda2$business))
mda2<-mutate(mda2,business2=case_when(business1<=3~1,(business1>3 & business1<=6)~2,(business1>6~3)))
View(mda2)
levels(factor(mda2$download))
mda2$download<-as.numeric(as.factor(mda2$download))
mda2<-mutate(mda2,download1=case_when(download<=49~1,(download>49 & download<=98)~2,(download>98~3)))
View(mda2)
mda2<-mutate(mda2,country2=case_when(country1 %in% c(1,3,4,5,7,8,9,10,12,13,14,15,16,17,18,20,21,22,23,24,25,32,33,35,37,40,41)~1,
                                     country1 %in% c(6,11,29,30,31,36,38,39)~2,
                                     country1 %in% c(27,28,2,42)~3))

colnames(mda2)
mda3<-mda2[,c(-1,-2,-3,-13,-4,-14,-5,-11,-6)]
colnames(mda3)
View(mda3)
######################################################################################################################

set.seed(121)
mdasa<-sample(2,nrow(mda3),replace=TRUE,prob=c(0.90,0.10))
mdasa_Train<-mda3[mdasa==1,]
mdasa_Test<-mda3[mdasa==2,]
###################################################################################################################

mdaa_s<-filter(mdasa_Train,label==1)
mdaa_tr_s<-rbind(mdasa_Train,mdaa_s,mdaa_s)
mdaa_tr_s<-na.omit(mdaa_tr_s)
ne<-table(mdaa_tr_s$label)
ne
###################################################################################################################
library(randomForest)
mod_md<-randomForest(label~.,data=mdaa_tr_s,ntree=64)
prd_md<-predict(mod_md,mdasa_Test,type="response")
prd_df_md<-data.frame(prd_md,mdasa_Test$label)
View(prd_df_md)
colnames(prd_df_md)<-c("predict","actual")
tnew<-table(prd_df_md$predict,prd_df_md$actual)
tnew
acc_neww<-sum(diag(tnew))/sum(tnew)
acc_neww

library(pROC)
roc<-roc(prd_df_md$actual,as.ordered(prd_df_md$predict))
plot(roc,col='red')
auc(roc)

#################################################################################################################

#    0     1
#  0 2215  353      AUROC=72.97  
#  1 1116 1364      ACCURACY=70.89937 
#################################################################################################################

mdmd1<-glm(label~.,family=binomial,data=mdaa_tr_s)
prd_md1<-predict(mdmd1,mdasa_Test,type="response")
prd_df1<-data.frame(prd_md1,mdasa_Test$label)
prd_df1<-mutate(prd_df1,prd_md1=ifelse(prd_md1>0.5,1,0))
colnames(prd_df1)<-c("predict","actual")
tne<-table(prd_df1$predict,prd_df1$actual)
tne
acc_11<-sum(diag(tne))/sum(tne)
acc_11

roc<-roc(prd_df1$actual,as.ordered(prd_df1$predict))
plot(roc,col='red')
auc(roc)
################################################################################################################

#      0    1
#  0   1838  306       AUROC=68.68
#  1   1493 1411       ACCURACY=64.36 
##############################################################################################################
library(party)
mod1<-ctree(label~.,data=mdaa_tr_s,controls = ctree_control((mincriterion = .10,minsplit = 70)))
newpred1<-predict(mod1,mdasa_Test,type="response")
new_df1<-data.frame(newpred1,mdasa_Test$label)
colnames(new_df1)<-c("predict","actual")
tab=table(new_df1$predict,new_df1$actual)
tab
acc<-sum(diag(tab))/sum(tab)*100
acc


roc<-roc(new_df1$actual,as.ordered(new_df1$predict))
plot(roc,col='red')
auc(roc)
#############################################################################################################
#      0     1 
#  0  2277  388     AUROC=72.83      
#  1  1059  1329    ACCURACY=71.36 
#############################################################################################################
#library(e1071)
#mo3<-svm(label~.,data=mdaa_tr_s)
#pr3<-predict(mo3,mdasa_test)
#mo_df3<-data.frame(pr3,mdasa_test$label)
#tab_new1=table(mo_pr3,mdasa_test$label)
#tab_new1
#ran_acc1<-sum(diag(tab_new1))/sum(tab_new1)*100
#ran_acc1
