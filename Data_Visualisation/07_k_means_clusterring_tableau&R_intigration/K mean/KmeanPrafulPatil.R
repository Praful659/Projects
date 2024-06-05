#Data Retrieval

#7
titanic <- read.csv('titanic.csv')

#8
dim(titanic)

#Data Pre-processing

#1
meanAge <- sum(na.omit(titanic$Age))/length(na.omit(titanic$Age))
meanAge


#3
titanic$Age[is.na(titanic$Age)] <- meanAge
titanic$Age <- round(titanic$Age)

#4
titanic$AgeCat[titanic$Age>=0 & titanic$Age<=16] <- "0-16"
titanic$AgeCat[titanic$Age>=17 & titanic$Age<=32] <- "17-32"
titanic$AgeCat[titanic$Age>=33 & titanic$Age<=48] <- "33-48"
titanic$AgeCat[titanic$Age>=49 & titanic$Age<=64] <- "49-64"
titanic$AgeCat[titanic$Age>=65] <- "65 and Above"

#5
titanic$Survived[titanic$Survived==0] <- "Not Survived"
titanic$Survived[titanic$Survived==1] <- "Survived"


#6
titanic$Pclass <- factor(titanic$Pclass)
titanic$AgeCat <- factor(titanic$AgeCat)
titanic$Survived <- factor(titanic$Survived)
titanic$Embarked <- as.character(titanic$Embarked)
titanic$Embarked[titanic$Embarked=="S"] <- "Southampton"
titanic$Embarked[titanic$Embarked=="C"] <- "Cherbourg"
titanic$Embarked[titanic$Embarked=="Q"] <- "Queenstown"
titanic$Embarked <- factor(titanic$Embarked)

#7
titanic <- titanic[c(-9,-11)]

#8
View(titanic)

write.csv(titanic, file="/Users/prafulpatil/SPRING 2023/MIS 6380 DATA VIZ/Assignments/Assignment_07/K mean/titanicNew.csv")


#K-means Clustering

#1

titanicNew<-read.csv("/Users/prafulpatil/SPRING 2023/MIS 6380 DATA VIZ/Assignments/Assignment_07/K mean/titanicNew.csv")
titanicUpdated<-titanicNew
SurvivedNum<-ifelse(titanicUpdated$Survived=="Not Survived",0,1)
titanicUpdated <-data.frame(titanicUpdated,SurvivedNum)

SexN<-ifelse(titanicUpdated $Sex=="male",1,0)
titanicUpdated <-data.frame(titanicUpdated, SexN)

EmbarkedN<-ifelse(titanicUpdated$Embarked=="Southampton",1,ifelse(titanicUpdated $Embarked=="Cherbourg",2,0))
titanicUpdated <-data.frame(titanicUpdated, EmbarkedN)

write.csv(titanicUpdated,file = "/Users/prafulpatil/SPRING 2023/MIS 6380 DATA VIZ/Assignments/Assignment_07/K mean/titanicUpdated.csv")

#2
titanic.scaled <- scale(data.frame(titanic$Age, titanic$Parch, titanic$SibSp, titanic$Fare))
colnames(titanic.scaled)
totwss <- vector()
btwss <- vector()
for (i in 2:15)
{
  set.seed(1234)
  temp <- kmeans(titanic.scaled, centers=i)
  totwss[i] <- temp$tot.withinss
  btwss[i] <- temp$betweenss
}

plot(totwss, xlab = "Number of Cluster", type = "b", ylab = "Total Within Sum of Square")

plot(btwss, xlab = "Number of Cluster", type = "b", ylab = "Total Between Sum of Square")

pkg_url <- "http://cran.r-project.org/bin/macosx/mavericks/contrib/3.1/Rserve_1.7-3.tgz"
install.packages(pkg_url, repos = NULL)
library(Rserve)
Rserve(args="--vanilla")




