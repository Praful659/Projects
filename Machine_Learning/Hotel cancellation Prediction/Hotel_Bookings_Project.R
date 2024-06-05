# Setup

# common:
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
library(rpart)
library(rpart.plot)
#install.packages("countrycode")
library(countrycode) #used for converting country names and codes from one format to another
library(lubridate) #Date Manipulation
library(pROC)
#install.packages("ROCR")
library(ROCR)
#install.packages("mice")
library(mice)
# for ML:
library(caret)
library(randomForest)
library(xgboost)


# load data:
df <- read.csv("hotel_bookings.csv",stringsAsFactors = TRUE)
head(df)

#Understanding the data #Converts character columns to factors
df[sapply(df, is.character)] <-
  lapply(df[sapply(df, is.character)], as.factor)
str(df)
summary(df)

# Let us eliminate NA and other undefined value
sum(is.na(df))
#There are totally 4 missing value in the dataset
missfun<- function(x){ #create a function to list each column with the total number of missing values in each
  sum(is.na(x))
}
apply(df,2, missfun) #Call the function
summary(df)
#We can observe that children feature consists of 4 missing value, so let us drop the four records
df<-na.omit(df)
# The 4 records which had NA values for children were all part of the city hotel and have been dropped

# "meal" contains values "Undefined", which is equal to SC.
df$meal <- ifelse(df$meal == "Undefined", "SC", df$meal)

# Some rows contain entreis with 0 adults, 0 children and 0 babies. 
# I'm dropping these entries with no guests.
zero_guests <- which(df$adults + df$children + df$babies == 0)
zero_guests
df <- df[-zero_guests, ]

#Undefined values in market_segment and distribution_channel
df <- subset(df, market_segment!='Undefined')
df<- subset(df, distribution_channel!='Undefined')

#Remaining Data
dim(df)

#1) EDA
#We have two levels for Hotel, before moving further for Exploratory Data analysis, 
#it is a good idea to have some information about the ratio of preference for City and Resort Hotels
# Check the number of booking at respective hotels
table(df$hotel)

# Visualize the distribution
ggplot(data = df, aes(x = hotel)) +
  geom_bar(stat = "count") +
  labs(title = "Booking Request by Hotel type",
       x = "Hotel type",
       y = "No. of bookings") +
  theme_classic() + scale_color_brewer(palette = "Set2")
# Check the distribution of hotel type for cancellation
table(df$is_canceled, df$hotel)


# After cleaning, separate Resort and City hotel
# To know the acutal visitor numbers, only bookings that were not canceled are included. 
rh <- df[df$hotel == "Resort Hotel" & df$is_canceled == 0, ]
ch <- df[df$hotel == "City Hotel" & df$is_canceled == 0, ]

######################.   Where do guests come from
# get number of acutal guests by country
origin <- df[df$reservation_status == "Check-Out",]
# Subset the data to include the countries which has more than 1500 reservation request
# otherwise including all the country with few or occassional request to avoid the graph
# from being clumsy
type_hotel <- origin %>% 
  group_by(country) %>% 
  filter(n() > 1500)

# Visualize the Traveller by Country.
type_hotel$county_name <- countrycode(type_hotel$country, 
                                     origin = "iso3c",
                                     destination = "country.name")

# Traveller by Country per hotel wise
ggplot(type_hotel, aes(county_name, fill = hotel)) + 
  geom_bar(stat = "count", position = position_dodge()) + 
  labs(title = "Booking Status by Country",
       x = "Country",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_blank())

#People from all over the world are staying in these two hotels. Most guests are 
#from Portugal and other countries in Europe. Hence, we can say that this dataset is from a hotels
#lcated in Portugal

#How much do guests pay for a room per night?
  
#Since no currency information is given, but Portugal is part of the European Monetary Union, 
#I assume that all prices are in EUR.
# Counting adults and children as paying guests only, not babies.
# Average daily rate by Hotel Type
ggplot(type_hotel, aes(x = adr, fill = hotel, color = hotel)) + 
  geom_histogram(aes(y = ..density..), position = position_dodge(), binwidth = 20 ) +
  geom_density(alpha = 0.2) + 
  labs(title = "Average Daily rate by Hotel",
       x = "Hotel Price(in Euro)",
       y = "Count") + scale_color_brewer(palette = "Paired") + 
  theme_classic() + theme(legend.position = "top")
# boxplot:
ggplot(type_hotel, aes(customer_type, fill = hotel)) + 
  geom_bar(stat = "count", position = position_dodge()) + 
  labs(title = "Hotel Preference by Customer Type",
       x = "Customer Type",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_blank())
#Does the hotel charged differently for different customer type
ggplot(type_hotel, aes(x = customer_type, y = adr, fill = hotel)) + 
  geom_boxplot(position = position_dodge()) + 
  labs(title = "Price Charged by Hotel Type",
       subtitle = "for Customer Type",
       x = "Customer Type",
       y = "Price per night(in Euro)") + theme_classic()


#How does the price per night vary over the year?
# Organize the Month in proper order
# load necessary packages
library(dplyr)
library(ggplot2)
library(lubridate)

# subset data and calculate mean room prices by hotel and month
hotel_data <- df %>% 
  filter(is_canceled == 0) %>% 
  group_by(hotel, arrival_date_month) %>% 
  summarise(avg_price = mean(adr)) %>% 
  mutate(arrival_date_month = factor(arrival_date_month, levels = month.name))

# create line plot with standard deviation error bars
ggplot(hotel_data, aes(x = arrival_date_month, y = avg_price, color = hotel, group = hotel)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "errorbar", 
               width = 0.2, size = 1, color = "black") +
  labs(title = "Room price per night and person over the year",
       x = "Month",
       y = "Price [EUR]") +
  scale_color_manual(values = c("Resort Hotel" = "red", "City Hotel" = "blue")) +
  theme_classic() +
  theme(legend.position = "top")


df$arrival_date_month <-
  factor(df$arrival_date_month, levels = month.name)
# Visualize Hotel traffic on Monthly basis
ggplot(data = df, aes(x = arrival_date_month)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = "count", aes(label = ..count..), hjust = 1) +
  coord_flip() + labs(title = "Month Wise Booking Request",
                      x = "Month",
                      y = "Count") +
  theme_classic()
# note this data use the booking confirmation and not the check ins, so this graph shows the
# booking made for particular month and not the confirmed check ins.

ggplot(df, aes(arrival_date_month, fill = factor(is_canceled))) +
  geom_bar() + geom_text(stat = "count", aes(label = ..count..), hjust = 1) +
  coord_flip() + scale_fill_discrete(
    name = "Booking Status",
    breaks = c("0", "1"),
    label = c("Cancelled", "Not Cancelled")
  ) +
  labs(title = "Booking Status by Month",
       x = "Month",
       y = "Count") + theme_bw()

# note this data use the booking confirmation and not the check ins, so this graph shows the
# booking made for particular month and not the confirmed check ins.

ggplot(df, aes(arrival_date_month, fill = hotel)) +
  geom_bar(position = position_dodge()) +
  labs(title = "Booking Status by Month",
       x = "Month",
       y = "Count") + theme_bw()


#############Which are the most busy month?

# Organize the Month in proper order
df$arrival_date_month <-
  factor(df$arrival_date_month, levels = month.name)
# Visualize Hotel traffic on Monthly basis
ggplot(data = df, aes(x = arrival_date_month)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = "count", aes(label = ..count..), hjust = 1) +
  coord_flip() + labs(title = "Month Wise Booking Request",
                      x = "Month",
                      y = "Count") +
  theme_classic()

# note this data use the booking confirmation and not the check ins, so this graph shows the
# booking made for particular month and not the confirmed check ins.

ggplot(df, aes(arrival_date_month, fill = factor(is_canceled))) +
  geom_bar() + geom_text(stat = "count", aes(label = ..count..), hjust = 1) +
  coord_flip() + scale_fill_discrete(
    name = "Booking Status",
    breaks = c("0", "1"),
    label = c("Cancelled", "Not Cancelled")
  ) +
  labs(title = "Booking Status by Month",
       x = "Month",
       y = "Count") + theme_bw()

# note this data use the booking confirmation and not the check ins, so this graph shows the
# booking made for particular month and not the confirmed check ins.

ggplot(df, aes(arrival_date_month, fill = hotel)) +
  geom_bar(position = position_dodge()) +
  labs(title = "Booking Status by Month",
       x = "Month",
       y = "Count") + theme_bw()

#The City hotel has more guests during spring and autumn, when the prices are also highest.
#In July and August there are less visitors, although prices are lower.

#Guest numbers for the Resort hotel go down slighty from June to September, which is also when the prices are highest.
#Both hotels have the fewest guests during the winter.

####################How long do people stay at the hotels?

# Total Stay Duration
# Filter data where is_canceled is equal to 0
filter <- df$is_canceled == 0
data <- df[filter,]
head(data)

data$total_nights <- data$stays_in_weekend_nights + data$stays_in_week_nights
head(data)

stay <- aggregate(is_canceled ~ total_nights + hotel, data = data, FUN = length)
stay <- stay[, c("total_nights", "hotel", "is_canceled")]
stay <- rename(stay, `Number of stays` = is_canceled)

fig <- plot_ly(data = stay, x = ~total_nights, y = ~`Number of stays`, color = ~hotel, type = 'bar') %>%
  layout(barmode = 'group', title = 'Number of Stays by Total Nights and Hotel',
         xaxis = list(title = 'Total Nights',range = c(0, 20)), yaxis = list(title = 'Number of Stays'))

fig


#########################Bookings by Market Segment
library(plotly)

# total bookings per market segment (incl. canceled)
segments <- table(df$market_segment)

# pie plot
fig <- plot_ly(labels = names(segments), values = segments, type = "pie", 
               title = "Bookings per market segment", textinfo = "percent+label",
               rotation = -90)
fig <- fig %>% layout(template = "seaborn")
fig

#################How many bookings were canceled?
total_cancelations <- sum(df$is_canceled)
rh_cancelations <- sum(df$hotel == "Resort Hotel" & df$is_canceled == 1)
ch_cancelations <- sum(df$hotel == "City Hotel" & df$is_canceled == 1)

rel_cancel <- total_cancelations / nrow(df) * 100
rh_rel_cancel <- rh_cancelations / sum(df$hotel == "Resort Hotel") * 100
ch_rel_cancel <- ch_cancelations / sum(df$hotel == "City Hotel") * 100

cat(paste("Total bookings canceled: ", format(total_cancelations, big.mark=","), " (", round(rel_cancel), "%)\n", sep=""))
cat(paste("Resort hotel bookings canceled: ", format(rh_cancelations, big.mark=","), " (", round(rh_rel_cancel), "%)\n", sep=""))
cat(paste("City hotel bookings canceled: ", format(ch_cancelations, big.mark=","), " (", round(ch_rel_cancel), "%)\n", sep=""))

#Total bookings canceled: 44,199 (37 %)
#Resort hotel bookings canceled: 11,120 (28 %)
#City hotel bookings canceled: 33,079 (42 %)

#Which month have the highest number of cancelations?
# Create a DateFrame with the relevant data:
res_book_per_month <- df %>%
  filter(hotel == "Resort Hotel") %>%
  group_by(arrival_date_month) %>%
  summarize(Bookings = n())

res_cancel_per_month <- df %>%
  filter(hotel == "Resort Hotel") %>%
  group_by(arrival_date_month) %>%
  summarize(Cancelations = sum(is_canceled))

cty_book_per_month <- df %>%
  filter(hotel == "City Hotel") %>%
  group_by(arrival_date_month) %>%
  summarize(Bookings = n())

cty_cancel_per_month <- df %>%
  filter(hotel == "City Hotel") %>%
  group_by(arrival_date_month) %>%
  summarize(Cancelations = sum(is_canceled))

res_cancel_data <- data.frame(Hotel = "Resort Hotel",
                              Month = res_book_per_month$arrival_date_month,
                              Bookings = res_book_per_month$Bookings,
                              Cancelations = res_cancel_per_month$Cancelations)

cty_cancel_data <- data.frame(Hotel = "City Hotel",
                              Month = cty_book_per_month$arrival_date_month,
                              Bookings = cty_book_per_month$Bookings,
                              Cancelations = cty_cancel_per_month$Cancelations)

full_cancel_data <- rbind(res_cancel_data, cty_cancel_data)
full_cancel_data$cancel_percent <- full_cancel_data$Cancelations / full_cancel_data$Bookings * 100

# order by month:
ordered_months <- c("January", "February", "March", "April", "May", "June", 
                    "July", "August", "September", "October", "November", "December")
full_cancel_data$Month <- factor(full_cancel_data$Month, levels=ordered_months)

# show figure:
ggplot(full_cancel_data, aes(x = Month, y = cancel_percent, fill = Hotel)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Cancelations per month", x="Month", y="Cancelations [%]") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

#For the City hotel the relative number of cancelations is around 40 % throughout the year.
#For the Resort hotel it is highest in the summer and lowest during the winter.

# select numeric columns only
df_numeric <- df[, sapply(df, is.numeric)]

# compute correlation matrix
corr <- cor(df_numeric)

# plot heatmap
# create heatmap
heatmap(
  corr,
  Rowv = NA,
  Colv = NA,
  col = cm.colors(256),
  scale = "none",
  margins = c(5, 10),
  xlab = "Variables",
  ylab = "Variables"
)

df <- df %>% 
  mutate(stay_nights_total = stays_in_weekend_nights + stays_in_week_nights,
         stay_cost_total = adr * stay_nights_total)
###########Modelling
#PCA
prop.table(table(df$is_canceled))
#Dataset is kind of Imbalanced (Not completely imbalanced dataset)

#Let us impute the missing values
#Let us extract numeric data
n_data<- df %>% select_if(is.numeric)
n_data<- n_data[-1] #Droping Response Variable
head(n_data)
#Imputing
#Since we have missing values in children feature, so imputing missing values with effective "pmm" method
library(mice)
impute<- mice(n_data,
              m=3,
              method = "pmm",
              maxit = 10)

n_data <- complete(impute,2)
#Now there are no missing values
sum(is.na(n_data))
#Let us perform PCA to reduce dimensions
pc<- prcomp(n_data, scale. = T, center = T)
summary(pc)
# PC1, PC2, PC3 have high proportion of variance & low cumulative frequency
pca <- predict(pc, newdata = n_data)
pca_data<- as.data.frame(pca)
head(pca_data)
#Selecting PC1, PC2, PC3 (They have high proportion of variance & low cumulative frequency)
pca_data<- pca_data[, 1:3]
head(pca_data)

#Extracting Categorical data
c_data<- select_if(df,is.factor)
head(c_data)

#Combining Categorical Variables with our respose variable for feature selection
response_variable<- df[2]
final_data <- cbind(response_variable,c_data,pca_data)
head(final_data)


#Building Model Using RandomForest

#"""Since we have imbalanced kind of dataset, Ensemble methods will avoid overfitting problems

#We dont required 'reservation status date, company(94% missing values) & agent ID'
#Randomforest wont work for variable which have more than 52 Levels. since 'country'
#feature has 178 level, doing 'one hot encoding' for all values will result curse in dimensionality"""

str(final_data)
final_data<- final_data[ ,-c(11,12,15)]
head(final_data)

#One hot encoding for country variable
dmy <- dummyVars(" ~ country", data = final_data)
dummy_data <- data.frame(predict(dmy, newdata = final_data))
head(dummy_data)

#PCA for dummy variables (since it has many columns)
pc<- prcomp(dummy_data, scale. = T, center = T)
pca_dummy<- predict(pc, newdata = dummy_data)
pca_dummy_data <- as.data.frame(pca_dummy)
pca_dummy_final<- pca_dummy_data[1:2]
head(pca_dummy_final)

#Combining dataframes
new_data<- cbind(final_data, pca_dummy_final)
new_final <- new_data[-5]
str(new_final)
new_final$is_canceled<- as.factor(new_final$is_canceled)









#----------------------------------------------------------------------
#Partition the data
index <- sample(2,size = nrow(new_final), replace = T , prob=c(0.7,0.3))
train <- new_final[index == 1, ]
test <- new_final[index == 2, ]

###Random Forest on Train data
library(randomForest)
rf_model <- randomForest(is_canceled~.,
                         data = train,
                         ntree = 500)
rf_model

predict_train<- predict(rf_model,train)
confusionMatrix(predict_train, train$is_canceled)
#Dropping Country Variable
df_o_country<- new_final[1:14]
#RandomForest without Country Feature
sind<- sample(2,nrow(df_o_country),
              replace = T,
              prob = c(0.7,0.3))

train_data2<- new_final[sind == 1,]
test_data2<- new_final[sind == 2,]

rf_model2 <- randomForest(is_canceled~.,
                          data = train_data2,
                          ntree = 500)

predict_test<- predict(rf_model,test)
confusionMatrix(predict_test, test$is_canceled,positive = "1")

###Random Forest on test data
rf_model <- randomForest(is_canceled~.,
                         data = test,
                         ntree = 500)
rf_model
predict_test<- predict(rf_model,test)
confusionMatrix(predict_test, test$is_canceled)
#Dropping Country Variable
df_o_country<- new_final[1:14]
#RandomForest without Country Feature
sind<- sample(2,nrow(df_o_country),
              replace = T,
              prob = c(0.7,0.3))

train_data2<- new_final[sind == 1,]
test_data2<- new_final[sind == 2,]

rf_model2 <- randomForest(is_canceled~.,
                          data = test_data2,
                          ntree = 500)

predict_test<- predict(rf_model,test)
confusionMatrix(predict_test, test$is_canceled,positive = "1")


#################Precision, Recall and Cross Validation
library(caret)

# Define the model
model <- train(is_canceled ~ ., data = train, method = "glm", trControl = trainControl(method = "cv", number = 10))

# Make predictions on the test set
pred <- predict(model, newdata = test)

# Compute precision, recall, and F1-score
conf_matrix <- confusionMatrix(data = pred, reference = test$is_canceled);conf_matrix
precision <- conf_matrix$byClass[1];precision
recall <- conf_matrix$byClass[2];recall
f1_score <- 2 * precision * recall / (precision + recall);f1_score

# Perform cross-validation
cv_results <- train(is_canceled ~ ., data = new_final, method = "glm", trControl = trainControl(method = "cv", number = 10))
cv_results































##################Logistic regression
#Logistic Regression
#Partition the data
index <- sample(2,size = nrow(df), replace = T , prob=c(0.7,0.3))
train <- df[index == 1, ]
test <- df[index == 2, ]
set.seed(1234)
glm.fit <- glm(is_canceled ~ ., 
               data = train , family = "binomial")
summary(glm.fit)

#Training data
#Prediction on Training Data

# Make sure that the levels of market_segment in the train data frame match those in the original data set
train$market_segment <- factor(train$market_segment, levels = levels(df$market_segment))

# Predict using the updated train data frame
train_pred <- predict(glm.fit, train)
library(ROCR)
pred <- prediction(train_pred,train$is_canceled)
perform <- performance(pred,"acc")
max <- which.max(slot(perform,"y.values")[[1]])
prob <- slot(perform,"x.values")[[1]][max]
prob

#Accuracy of Training Data
train_pred1 <- ifelse(train_pred >  prob, 1,0)
mean(train$is_canceled == train_pred1)

#Confusion Matrix of Training data
tble <- table(Actual = train$is_canceled,Predicted = train_pred1 );tble
library(knitr)

#Classification Table of Training Data
TN <- tble[1,1]
FN <- tble[2,1]
FP <- tble[1,2]
TP <- tble[2,2]
N <- sum(tble[1,])
P <- sum(tble[2,])
Specificity <- FP/N
Sensitivity <- TP/N
df <- data.frame(Specificity,Sensitivity)
kable(df)

#Missclassification Error on training data
1 - sum(diag(tble))/sum(tble)

#AUC & ROC on training data
#ROC Train

#Predictions for logistic regression
#Predictions for Logistic Regression
train$logit_pred_prob<-predict(glm.fit,train,type="response")
train$logit_pred_class<-ifelse(train$logit_pred_prob>0.5,"1","0") 

logit_roc<-roc(train$is_canceled,train$logit_pred_prob,auc=TRUE)
plot(logit_roc,print.auc=TRUE,print.auc.y=.4, col="green")

#AUC for training dataset
auc <- performance(pred,"auc")
auc <- unlist(slot(auc,"y.values"))
auc

#Testing data
#Prediction on Testing Data

# Predict using the updated test data frame
test_pred <- predict(glm.fit, test, type = 'response')

pred1 <- prediction(test_pred,test$is_canceled)
perform1 <- performance(pred1,"acc")
max <- which.max(slot(perform1,"y.values")[[1]])
prob <- slot(perform,"x.values")[[1]][max]
prob

#Accuracy of testing Data
test_pred1 <- ifelse(test_pred >  prob, 1,0)
mean(test$is_canceled == test_pred1)

#Confusion Matrix of testing data
tble1 <- table(Actual = test$is_canceled,Predicted = test_pred1 );tble1
library(knitr)

#Classification Table of testing Data
TN <- tble1[1,1]
FN <- tble1[2,1]
FP <- tble1[1,2]
TP <- tble1[2,2]
N <- sum(tble[1,])
P <- sum(tble[2,])
Specificity <- FP/N
Sensitivity <- TP/N
df <- data.frame(Specificity,Sensitivity)
kable(df)

#Missclassification Error on testing data
1 - sum(diag(tble1))/sum(tble1)

#AUC & ROC on training data
#ROC Train

#Predictions for logistic regression
#Predictions for Logistic Regression
test$logit_pred_prob<-predict(glm.fit,test,type="response")
test$logit_pred_class<-ifelse(test$logit_pred_prob>0.5,"1","0") 

logit_roc<-roc(test$is_canceled,test$logit_pred_prob,auc=TRUE)
plot(logit_roc,print.auc=TRUE,print.auc.y=.4, col="red")

#AUC for training dataset
auc <- performance(pred1,"auc")
auc <- unlist(slot(auc,"y.values"))
auc
