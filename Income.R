library(dplyr)
library(sqldf)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(tidyverse)
library(VIM)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

#Call the Data
adult=read.csv(file.choose(),header = TRUE)
adult
view(adult)
str(adult)
summary(adult)

##
adult$income<-ifelse(adult$income=='>50K',1,0)
adult$income

##Exploring Categorical Workclass Variable.

adult$workclass<-ifelse(adult$workclass=='?','Unknown',as.character(adult$workclass))
adult$workclass
Work_class<-sqldf('SELECT workclass, count(workclass) as Count 
                  ,sum(income) as Above from adult group by workclass')
Work_class
table<-data.frame(Class=Work_class$workclass, Proportion=Work_class$Above/Work_class$Count)
table
Work_class$Below<-Work_class$Count-Work_class$Above
Work_class$Below
Work_class<-Work_class[,c(1,3,4)]
Work_class
Workclass<-melt(Work_class,id.vars = 'workclass')
Workclass

gg<-ggplot(Workclass,aes(x=workclass,y=value,fill=variable))+
  geom_bar(stat = 'identity',position = 'stack')+
  theme_bw()+
  scale_fill_manual(values = c('red','green'))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle('Proportions of above-paid within different classes')
tbl <- tableGrob(t(table), rows=NULL)

grid.arrange(tbl, gg,
             nrow=2,
             as.table=TRUE,
             heights=c(1,4))

##Exploring Categorical education Variable.

education<-sqldf('SELECT education, count(education) as Count 
                  ,sum(income) as Above from adult group by education')
education$Below<-education$Count-education$Above
table<-data.frame(Class=education$education, Proportion=education$Above/education$Count)
education<-education[,c(1,3,4)]
edu<-melt(education,id.vars = 'education')
gg<-ggplot(edu,aes(x=education,y=value,fill=variable))+
  geom_bar(stat = 'identity',position = 'stack')+
  theme_bw()+
  scale_fill_manual(values = c('red','green'))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle('Proportions of above-paid within different education level')
tbl <- tableGrob(t(table), rows=NULL)
grid.arrange(tbl, gg,
             nrow=2,
             as.table=TRUE,
             heights=c(1,4))


##Exploring Categorical Marital_Status Variable.

colnames(adult)[6]<-'Marital'
marital<-sqldf('SELECT Marital, count(Marital) as Count 
                  ,sum(income) as Above from adult group by Marital')
marital$Below<-marital$Count-marital$Above
table<-data.frame(Marital=marital$Marital, Proportion=marital$Above/marital$Count)
marital<-marital[,c(1,3,4)]
mar<-melt(marital,id.vars = 'Marital')
gg<-ggplot(mar,aes(x=Marital,y=value,fill=variable))+geom_bar(stat = 'identity',position = 'stack')+theme_bw()+scale_fill_manual(values = c('red','green'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of above-paid within different marital status')
tbl <- tableGrob(t(table), rows=NULL)
grid.arrange(tbl, gg,
             nrow=2,
             as.table=TRUE,
             heights=c(1,4))

##Exploring Categorical race Variable.

race<-sqldf('SELECT race, count(race) as Count 
                  ,sum(income) as Above from adult group by race')
race$Below<-race$Count-race$Above
table<-data.frame(race=race$race, Proportion=race$Above/race$Count)
race<-race[,c(1,3,4)]
rac<-melt(race,id.vars = 'race')
gg<-ggplot(rac,aes(x=race,y=value,fill=variable))+geom_bar(stat = 'identity',position = 'stack')+theme_bw()+scale_fill_manual(values = c('red','green'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of above-paid within different races')
tbl <- tableGrob(t(table), rows=NULL)
grid.arrange(tbl, gg,
             nrow=2,
             as.table=TRUE,
             heights=c(1,4))

##Exploring Categorical gender Variable.

gender<-sqldf('SELECT gender, count(gender) as Count 
                  ,sum(income) as Above from adult group by gender')
gender$Below<-gender$Count-gender$Above
table<-data.frame(gender=gender$gender, Proportion=gender$Above/gender$Count)
gender<-gender[,c(1,3,4)]
se<-melt(gender,id.vars = 'gender')
gg<-ggplot(se,aes(x=gender,y=value,fill=variable))+
  geom_bar(stat = 'identity',position = 'stack')+
  theme_bw()+scale_fill_manual(values = c('red','green'))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle('Proportions of above-paid within different sexes')
tbl <- tableGrob(t(table), rows=NULL)
grid.arrange(tbl, gg,
             nrow=2,
             as.table=TRUE,
             heights=c(1,4))

##Exploring continuous capital.loss Variable.

colnames(adult)[12]<-'capital.loss'
gg<-qplot(capital.loss, data=adult, geom="histogram")+
  theme_bw()+
  ggtitle('Histogram of Capital Loss')
gg

##Exploring continuous Hours Variable.

colnames(adult)[13]<-'Hours'
gg<-qplot(Hours, data=adult, geom="histogram")+
  theme_bw()+
  ggtitle('Histogram of Working Hours')
gg

##Exploring Categorical WorkLoad Variable.
adult1<-adult
adult1$HourJ<-ifelse(adult1$Hours<=40,'NormalWorkLoad','HugeWorkLoad')
wl<-sqldf('SELECT HourJ as WorkLoad, count(HourJ) as Count, sum(income) as Above from adult1 group by HourJ')
wl$Below<-wl$Count-wl$Above
Percentage<-wl$Above/wl$Count
wl<-wl[,c(1,3,4)]
wlt<-melt(wl,id.vars = 'WorkLoad')
wl<-cbind(wl,Percentage)
gg<-ggplot(wlt,aes(x=WorkLoad,y=value,fill=variable))+
  geom_bar(stat = 'identity',position = 'stack')+
  theme_bw()+scale_fill_manual(values = c('red','green'))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle('Proportions of above-paid with different Work Load')
tbl <- tableGrob(t(wl[,c(1,4)]), rows=NULL)
grid.arrange(tbl, gg,
             nrow=2,
             as.table=TRUE,
             heights=c(1,4))


colnames(adult)[14]<-'Country'
df3<-adult
cato<-c('education','Marital','occupation','relationship','race','gender','Country')
for(each in cato){
  df3[,each]<-as.integer(df3[,each])
}

class<-unique(adult$workclass)
for(i in 1:length(df3$age)){
  df3$workclass[i]<-which(class %in% df3$workclass[i])
}
df3$workclass<-as.numeric(df3$workclass)
benchmark<-1- sum(df3$income)/length(df3[,1])
benchmark
###So: 0.76 will be the accuracy of classificationwhen I guess all people cannot earn a salary more than 50K. 
##This would be the benchmark for model evaluation.



###lets gofor the Decision tree.

set.seed(1000)
df3$income<-as.factor(as.character(df3$income))
intrain<- createDataPartition(df3$income,p=0.7,list = FALSE)
train<- df3[intrain,]
test <- df3[-intrain,]

treeFit<- rpart(income~.,data=train,method = 'class')
print(treeFit)
rpart.plot(treeFit, box.col=c("red", "blue"))
Prediction1<- predict(treeFit,newdata=test[-15],type = 'class')
TreeAcu<-confusionMatrix(Prediction1,test$income)$overall[1]
TreeAcu

###Lets go for the Logistic Regression. 

lg<-glm(income ~.,family=binomial(link='logit'),data=train)
Prediction2<- predict(lg,newdata=test[-15],type = 'response')
Pred<- ifelse(Prediction2>0.5,1,0)
lgAcu<-confusionMatrix(Pred,test$income)$overall[1]
lgAcu

###Lets go for the Random FoRest

set.seed(32423)
rfFit<- randomForest(income~.,data= train)
print(rfFit)
Prediction3<- predict(rfFit,newdata = test[,-15],type = 'class')
rfAcu<-confusionMatrix(Prediction3,test$income)$overall[1]
rfAcu

###Lets go for the Staked Model

predDF<- data.frame(Prediction1,Prediction2,Prediction3,outcome=test$income)
combModFit<- train(outcome~.,method='gbm',data=predDF,verbose=FALSE)
Prediction_Comb<- predict(combModFit,predDF)
stAcu<- confusionMatrix(Prediction_Comb,test$income)$overall[1]
stAcu

###Comparison between Decision Tree, Logistic Model, Random Forest and Staked Model

Accuracy<-data.frame(Model=c('Decision Tree','Logistic Regression','Random Forest','Stacked Model'),
                     Accuracy=c(TreeAcu,lgAcu,rfAcu,stAcu))
gg<-ggplot(Accuracy,aes(x=Model,y=Accuracy,fill=Model))+
  geom_bar(stat = 'identity')+theme_bw()+
  ggtitle('Accuracies of Models')+
  geom_hline(yintercept = benchmark,color='red')
gg

summary(lg)


###In this analysis we made some categorical exploration in the data and see the comparison between less than or more than 50k.  
###We use the logistic regression model to see which features contribute more to the income.
###Here we can see that Marital status, Working hours and gender are really matters if you wanna earn more than 50k per year.
###And we can also see that the private sector's employees contribution is most to earn more than 50k per year. 
