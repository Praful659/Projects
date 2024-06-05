titanic=read.csv('C:\\Users\\Pallavi\\Desktop\\6380 Exercises\\Exercise 7\\titanic.csv')
dim(titanic)
head(titanic)
meanage=sum(na.omit(titanic$Age))/length(na.omit(titanic$Age))
meanage
titanic$Age[is.na(titanic$Age)]=meanage
titanic$Age=round(titanic$Age)
titanic$AgeCat[titanic$Age>=0&titanic$Age<=16]="0-16"
titanic$AgeCat[titanic$Age>=17&titanic$Age<=32]="17-32"
titanic$AgeCat[titanic$Age>=33&titanic$Age<=48]="33-48"
titanic$AgeCat[titanic$Age>=49&titanic$Age<=64]="49-64"
titanic$AgeCat[titanic$Age>=65]="65 and above"
titanic$Survived[titanic$Survived==0]="Not Survived"
titanic$Survived[titanic$Survived==1]="Survived"
titanic$Pclass=factor(titanic$Pclass)
titanic$AgeCat=factor(titanic$AgeCat)
titanic$Survived=factor(titanic$Survived)
titanic$Embarked=as.character(titanic$Embarked)
titanic$Embarked[titanic$Embarked=="S"]="Southampton"
titanic$Embarked[titanic$Embarked=="C"]="Cherbourg"
titanic$Embarked[titanic$Embarked=="Q"]="Queenstown"
titanic$Embarked=factor(titanic$Embarked)
titanic=titanic[c(-9,-11)]
View(titanic)
write.csv (titanic, file = "C:\\Users\\Pallavi\\Desktop\\6380 Exercises\\Exercise 7\\titanicNew.csv")
decision_tree=titanic
sibspcat=ifelse(titanic$SibSp>=3,">=3","<3")
decision_tree=data.frame(decision_tree, sibspcat)
decision_tree$sibspcat=as.factor(decision_tree$sibspcat)
parchcat=ifelse(titanic$Parch>=3,">=3","<3")
decision_tree=data.frame(decision_tree, parchcat)
decision_tree$parchcat=as.factor(decision_tree$parchcat)
set.seed(1)
test=sample(1:nrow(decision_tree), nrow(decision_tree)/3)
train=-test
training_data=decision_tree[train,]
testing_data=decision_tree[test,]
testing_survived=decision_tree$Survived[test]
library(rpart)
library(rattle)
tree_model=rpart(Survived~Pclass+Sex+AgeCat+Embarked+sibspcat+parchcat, data=training_data, method="class", control = rpart.control(minsplit = 10, cp=0.00))
fancyRpartPlot(tree_model, sub = "decision_tree")
tree_predict=predict(tree_model, testing_data, type = "class")
mean(tree_predict!=testing_survived)
titanicNew=read.csv("C:\\Users\\Pallavi\\Desktop\\6380 Exercises\\Exercise 7\\titanicNew.csv")
titanicUpdated=titanicNew
SurvivedNum=ifelse(titanicUpdated$Survived=="Not Survived",0,1)
titanicUpdated=data.frame(titanicUpdated,SurvivedNum)
SexN=ifelse(titanicUpdated$Sex=="male",1,0)
titanicUpdated=data.frame(titanicUpdated, SexN)

EmbarkedN=ifelse(titanicUpdated$Embarked=="Southampton",1,ifelse(titanicUpdated$Embarked=="Cherbourg",2,0))
titanicUpdated <-data.frame(titanicUpdated, EmbarkedN)

write.csv(titanicUpdated,file = "C:\\Users\\Pallavi\\Desktop\\6380 Exercises\\Exercise 7\\titanicUpdated.csv")
titanic.scaled=scale(data.frame(titanic$Age, titanic$Parch, titanic$SibSp, titanic$Fare))
colnames(titanic.scaled)
totwss=vector()
btwss=vector()
for(i in 2:15)
{
  set.seed(1234)
  temp=kmeans(titanic.scaled, centers = i)
  totwss[i]=temp$tot.withinss
  btwss[i]=temp$betweenss
}
plot(totwss,xlab = "Number of Cluster", type="b",
     ylab= "total within sum of squares")
plot(btwss, xlab = "Number of Clusters", type="b",
     ylab = "Total between sum of squares")


install.packages("Rserve")
library(Rserve)
Rserve()

