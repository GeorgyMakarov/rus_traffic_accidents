# prerequisite packages

library(dplyr)
library(ggplot2)
library(lubridate)
library(Hmisc)
library(corrplot)
library(caret)

# read data

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

# add severity rate and severity class to dataset

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

# long-list predictors
# manually choose those which we can collect from a phone call

long_list <- sev_rate %>% select(dtpv, day, month, date, time, district,
                                 kts, kuch, road_cond, sev_class)

# transform month, day, hour to numbers

long_list <- long_list %>% 
    select(-c(day, month)) %>% mutate(dtpvnum = as.numeric(dtpv),
                                      distrnum = as.numeric(district),
                                      roadnum = as.numeric(road_cond)) %>% 
    mutate(mon = month(date), d = mday(date), h = hour(time))

# create data partitions for training and testing datasets

set.seed(5860)
in_train <- createDataPartition(y = long_list$sev_class, p = 0.6, list = FALSE)
training <- long_list[in_train,]
testing <- long_list[-in_train,]

dim(training)

# make correlation matrix to check if there are correlated variables

cor_data <- training[, c(5, 6, 9, 10, 11, 12, 13, 14)]

res2 <- rcorr(as.matrix(cor_data))
res2

corrplot(res2$r, type = "upper", order = "hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "pch")

# there is slight covariation between kts and kuch and slight covariation
# between accident type and kts - that should not be true, as the levels were
# just marked by alphabet order

# check correlation of kts and kuch with Spearman coefficient

spearman <- training %>% select(kts, kuch) %>% group_by(kts) %>% 
    summarise(kuch = mean(kuch))

cor.test(spearman$kts, spearman$kuch , method = "spearman")

# spearman rho is 0.922 - there is strong correlation between kts and kuch

# make short-list of variables on training dataset

short_list <- training %>% select(dtpv, mon, d, h, district, kts, kuch,
                                  road_cond, sev_class)

# short-list algorithm

# training control for linear methods

ctrl_lm <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)

# train lm

num_list <- training %>% select(dtpvnum, mon, d, h, distrnum, 
                                roadnum, kts, kuch, sev_class)
lm_start <- Sys.time()
lm.fit <- train(sev_class ~.,
                data = short_list,
                method = "glm",
                metric = "ROC",
                trControl = ctrl_lm)
lm_stop <- Sys.time()
lm_diff <- lm_stop - lm_start

# train decision tree

rpart_start <- Sys.time()
rpart.fit <- train(sev_class ~.,
                   data = short_list,
                   method = "rpart",
                   metric = "ROC",
                   trControl = ctrl_lm)
rpart_stop <- Sys.time()
rpart_diff <- rpart_stop - rpart_start

# train random forest

rf_start <- Sys.time()
rf.fit <- train(sev_class ~.,
                data = short_list,
                method = "rf",
                metric = "ROC",
                trControl = ctrl_lm)
rf_stop <- Sys.time()
rf_diff <- rf_stop - rf_start

#make comparison table for models

lm_roc <- as.numeric(round(lm.fit$results[2], 4))
rpart_roc <- as.numeric(round(rpart.fit$results[1, 2], 4))
rf_roc <- as.numeric(round(rf.fit$results[2, 2], 4))

lm_sens <- as.numeric(round(lm.fit$results[4], 4))
rpart_sens <- as.numeric(round(rpart.fit$results[1, 4], 4))
rf_sens <- as.numeric(round(rf.fit$results[2, 4], 4))

compare_models <- tribble(~model, ~roc, ~sens, ~train_time,
                          "glm", lm_roc, lm_sens, lm_diff,
                          "rpart", rpart_roc, rpart_sens, rpart_diff,
                          "rf", rf_roc, rf_sens, rf_diff)

compare_models

#plot decision tree

library(rattle)
fancyRpartPlot(rpart.fit$finalModel)



# test prediction on linear model

testnum_list <- testing %>% select(dtpvnum, mon, d, h, distrnum, 
                                  roadnum, kts, kuch, sev_class)
head(testnum_list)

glm.pred <- predict(lmnum.fit, testnum_list)
lm_cmatrix <- confusionMatrix(glm.pred, testnum_list$sev_class, positive = "serious")
print(lm_cmatrix)

# test prediction on rpart

test_list <- testing %>% select(dtpv, mon, d, h, district, kts, kuch,
                                    road_cond, sev_class)
head(test_list)

rpart.pred <- predict(rpart.fit, test_list)
rpart_cmatrix <- confusionMatrix(rpart.pred, test_list$sev_class, positive = "serious")
print(rpart_cmatrix)

# test prediction on random forest

rf.pred <- predict(rf.fit, test_list)
rf_cmatrix <- confusionMatrix(rf.pred, test_list$sev_class, positive = "serious")
print(rf_cmatrix)

# I choose decision tree to train further and tune the parameters
# basic parameters tuning

print(rpart.fit$finalModel)
print(rpart_cmatrix)





