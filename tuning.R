# Prepare data for model tuning -------------------------------------

# Libraries

library(dplyr)
library(ggplot2)
library(lubridate)
library(Hmisc)
library(caret)
library(ROSE)
library(rpart)

# Prepare data

## Read data

clean_data <- read.csv("clean_data.csv")
clean_data <- clean_data %>% select(-X)
clean_data <- data.frame(clean_data)
clean_data$date <- ymd(clean_data$date)
clean_data$time <- as.POSIXct(clean_data$time)
clean_data <- clean_data %>% mutate(hour = hour(clean_data$time),
                                    casualties = fatal + injury)
clean_data$month <- month(clean_data$date, label = TRUE)
clean_data$day <- wday(clean_data$date, label = TRUE)
clean_data <- clean_data %>% mutate(cas_type = case_when(
  fatal > 0 ~ "fatal",
  injury > 0 ~ "injury",
  TRUE ~ "non-injury"
))


## Add severity rate and severity class to dataset

sev_rate <- clean_data %>% 
  mutate(kts_kuch = kts + kuch, 
         severity = casualties / kts_kuch) %>% 
  mutate(year = year(date),
         mmonth = month(date),
         mday = mday(date),
         hhour = hour(time),
         mminute = minute(time)) %>% 
  mutate(timeline = make_datetime(year, mmonth, mday, hhour, mminute))

sev_rate <- sev_rate %>% mutate(sev_class = case_when(
  severity > 0.35 ~ "serious",
  severity <= 0.35 ~ "moderate"
))

sev_rate$sev_class <- as.factor(sev_rate$sev_class)
sev_rate$cas_type <- as.factor(sev_rate$cas_type)

sev_rate <- sev_rate %>% select(-c(driving_mode, hour, year, mmonth, mday,
                                   hhour, mminute))

rm(clean_data)

## Choose predictors and split to training and testing

long_list <- sev_rate %>% select(dtpv, month, date, time, district,
                                 kts, kuch, road_cond, sev_class)

long_list <- long_list %>% 
  mutate(d = mday(date), h = hour(time)) %>% select(-c(date, time))

long_list$dtpv <- as.factor(long_list$dtpv)
long_list$district <- as.factor(long_list$district)
long_list$road_cond <- as.factor(long_list$road_cond)
long_list$month <- as.numeric(long_list$month)

str(long_list)

set.seed(595)
in_train <- createDataPartition(y = long_list$sev_class, p = 0.7, list = FALSE)
training <- long_list[in_train,]
testing <- long_list[-in_train,]

dim(training)

# Compare factor levels of training and testing datasets

## Comparison of datasets

str(training)
str(testing)

## Check the imbalance of the dataset

prop.table(table(training$sev_class))
prop.table(table(testing$sev_class))

# Make balanced dataset

data_rose <- ROSE(sev_class ~., data = training, seed = 1)$data
prop.table(table(data_rose$sev_class))

# Tuning decision tree 1 ----------------------------------------------------

# Make prediction with caret package

set.seed(1)
mod_rpart <- caret::train(sev_class ~.,
                   method = "rpart",
                   data = data_rose,
                   tuneLength = 10,
                   metric = "ROC",
                   trControl = trainControl(method = "repeatedcv",
                                            number = 10,
                                            repeats = 10,
                                            summaryFunction = twoClassSummary,
                                            classProbs = TRUE))

pred_rpart <- predict(mod_rpart, testing)
confusionMatrix(pred_rpart, testing$sev_class, positive = "serious")
roc.curve(testing$sev_class, pred_rpart, plotit = FALSE)
rm(mod_rpart, pred_rpart)
rm(in_train, long_list, sev_rate, training)

# Check contribution of features to classificator

task <- mlr::makeClassifTask(id = "sev_class",
                        data = data_rose,
                        target = "sev_class",
                        positive = "serious")

learner <- mlr::makeLearner("classif.rpart", 
                       predict.type = "prob")

ig <- mlr::generateFilterValuesData(task,method = "information.gain")
gr <- mlr::generateFilterValuesData(task,method = "gain.ratio")

ig
gr

# Check model performance with valuable predictors only

set.seed(1)
mod_rpart <- caret::train(sev_class ~ kuch + dtpv + h + district + road_cond + month + kts,
                          method = "rpart",
                          data = data_rose,
                          tuneLength = 10,
                          metric = "ROC",
                          trControl = trainControl(method = "repeatedcv",
                                                   number = 10,
                                                   repeats = 10,
                                                   summaryFunction = twoClassSummary,
                                                   classProbs = TRUE))

pred_rpart <- predict(mod_rpart, testing)
confusionMatrix(pred_rpart, testing$sev_class, positive = "serious")
roc.curve(testing$sev_class, pred_rpart, plotit = FALSE)
rm(mod_rpart, pred_rpart)


# Tuning decision tree 2 --------------------------------------------------------

# Return weather conditions and use timeline instead of hours

clean_data <- read.csv("clean_data.csv")
clean_data <- clean_data %>% select(-X)
clean_data <- data.frame(clean_data)
clean_data$date <- ymd(clean_data$date)
clean_data$time <- as.POSIXct(clean_data$time)
clean_data <- clean_data %>% mutate(casualties = fatal + injury)
clean_data$month <- month(clean_data$date, label = TRUE)
clean_data$day <- wday(clean_data$date, label = TRUE)
clean_data <- clean_data %>% mutate(cas_type = case_when(
    fatal > 0 ~ "fatal",
    injury > 0 ~ "injury",
    TRUE ~ "non-injury"
))

sev_rate <- clean_data %>% 
    mutate(kts_kuch = kts + kuch, 
           severity = casualties / kts_kuch) %>% 
    mutate(year = year(date),
           mmonth = month(date),
           mday = mday(date),
           hhour = hour(time),
           mminute = minute(time)) %>% 
    mutate(timeline = make_datetime(year, mmonth, mday, hhour, mminute))

sev_rate <- sev_rate %>% mutate(sev_class = case_when(
    severity > 0.35 ~ "serious",
    severity <= 0.35 ~ "moderate"
))

sev_rate$sev_class <- as.factor(sev_rate$sev_class)
sev_rate$cas_type <- as.factor(sev_rate$cas_type)
sev_rate <- sev_rate %>% select(-c(driving_mode, year, mmonth, mday,
                                   hhour, mminute))

rm(clean_data)

long_list <- sev_rate %>% select(dtpv, time, district, kts, 
                                 kuch, latitude,
                                 longitude, weather_cond, road_cond, month,
                                 day, sev_class)
long_list$dtpv <- as.factor(long_list$dtpv)
long_list$district <- as.factor(long_list$district)
long_list$road_cond <- as.factor(long_list$road_cond)
long_list$month <- as.numeric(long_list$month)
long_list$day <- as.numeric(long_list$day)
long_list$hour <- hour(long_list$time)
long_list$minute <- minute(long_list$time)
long_list <- long_list %>% mutate(hour_minute = hour + minute / 60)
long_list$time <- long_list$hour_minute
long_list <- long_list %>% select(-c(hour, minute, hour_minute))

str(long_list)

set.seed(6000)
in_train <- createDataPartition(y = long_list$sev_class, p = 0.7, list = FALSE)
training <- long_list[in_train,]
testing <- long_list[-in_train,]

dim(training)

str(training)
str(testing)

prop.table(table(training$sev_class))
prop.table(table(testing$sev_class))

data_rose <- ROSE(sev_class ~., data = training, seed = 1)$data
prop.table(table(data_rose$sev_class))

rm(in_train, sev_rate, long_list, training)

task <- mlr::makeClassifTask(id = "sev_class",
                             data = data_rose,
                             target = "sev_class",
                             positive = "serious")

learner <- mlr::makeLearner("classif.rpart", 
                            predict.type = "prob")

ig <- mlr::generateFilterValuesData(task,method = "information.gain")
gr <- mlr::generateFilterValuesData(task,method = "gain.ratio")

ig
gr

# Try model on all features

set.seed(1)
mod_rpart <- caret::train(sev_class ~.,
                          method = "rpart",
                          data = data_rose,
                          tuneLength = 15,
                          metric = "ROC",
                          trControl = trainControl(method = "repeatedcv",
                                                   number = 10,
                                                   repeats = 10,
                                                   summaryFunction = twoClassSummary,
                                                   classProbs = TRUE))

pred_rpart <- predict(mod_rpart, testing)
confusionMatrix(pred_rpart, testing$sev_class, positive = "serious")
roc.curve(testing$sev_class, pred_rpart, plotit = FALSE)
rm(mod_rpart, pred_rpart)

set.seed(1)
mod_tree <- rpart(sev_class ~.,
                  data = data_rose,
                  method = "class",
                  control = rpart.control(cp = mod_rpart$bestTune,
                                          minsplit = 100,
                                          maxdepth = 6))

pred_tree <- predict(mod_tree, testing, type = "class")
confusionMatrix(pred_tree, testing$sev_class, positive = "serious")
roc.curve(testing$sev_class, pred_tree, plotit = FALSE)


plotcp(mod_tree)
rpart.plot(mod_tree)
rm(mod_tree, pred_tree)
mod_rpart$bestTune
