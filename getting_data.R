# Prerequisite libraries
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(XML)
library(readr)
library(stringi)
library(stringr)

# loop for reading .xml files
files <- list.files("./raw_data")
files <- as.list(files)

setwd("./raw_data")
list_file <- list.files(pattern = "*.xml") %>% 
    lapply(xmlToDataFrame, stringsAsFactors = F) %>% bind_rows
setwd("C:/Users/Георгий/Documents/GitHub/rus_traffic_accidents/")


# filter rows in the top of the table - they contain general info not needed
# for the analysis, because this infor tells us about region - St. Pete, but we
# already have filtered St. Pete.

list_file <- list_file %>% 
    filter(!(is.na(DTPV))) %>% select(-text)
list_file$date <- dmy(list_file$date)
list_file$time <- as.POSIXct(list_file$time, format = "%H:%M")
xmldf <- list_file %>% arrange(date, time, DTPV, district)
rm(list_file)

# check data summary
# need to transfor to num: KTS, KUCH, POG, RAN
summary(xmldf)

# transform columns to numeric
xmldf$KTS <- as.numeric(xmldf$KTS)
xmldf$KUCH <- as.numeric(xmldf$KUCH)
xmldf$POG <- as.numeric(xmldf$POG)
xmldf$RAN <- as.numeric(xmldf$RAN)


# write data description file
# check for correlation between variables

# check for NA values in each column

sum(complete.cases(xmldf))

# split infoDtp to columns

## create column to describe the driving mode after the accident
driving_mode <- xmldf %>% 
    separate(infoDtp, into = c("text1"), 
             sep = "([:digit:]+\\.[:digit:]+\\.[:digit:]+)")
driving_mode <- driving_mode %>% 
    separate(text1, into = c("text1"), 
             sep = "([:digit:]+\\.[:digit:]+)") %>% select(text1)


xmldf <- cbind(xmldf, driving_mode)
xmldf$driving_mode <- xmldf$text1
xmldf <- xmldf %>% select(-text1)

## extract coordinates from infoDtp
coords_dtp <- xmldf %>% 
    select(date, infoDtp) %>% 
    extract(infoDtp, c("text1"), 
            "([:digit:]+\\.[:digit:]+\\.[:digit:]+)", remove = TRUE) %>% 
    select(text1)

xmldf <- cbind(xmldf, coords_dtp)
xmldf$coords_dtp <- xmldf$text1
xmldf <- xmldf %>% select(-text1)

## extract road type from infoDtp

road_type <- xmldf %>% select(infoDtp) %>% 
    separate(infoDtp, into = c("text1", "text2"), 
             sep = "([:digit:]+\\.[:digit:]+\\.[:digit:]+)") %>% 
    select(text2)

road_type <- road_type %>% 
    extract(text2, c("text1"), "([^A-z]+[:punct:])", remove = FALSE)

road_type <- road_type %>% 
    extract(text1, c("text1"), "([:alnum:]+[:alnum:]+)", remove = TRUE)

xmldf$road_type <- road_type$text1

## read .csv files and create second dataset - use this dataset to obtain:
## latitude, longitude, road category, road condition, weather condition
## merging two datasets on artificial key: date time dtpv district

### read all the csv files

setwd("./csv_data")
list_file <- list.files(pattern = "*.csv") %>% 
    lapply(read.csv, sep = ";", encoding = "UTF-8") %>% bind_rows
setwd("C:/Users/Георгий/Documents/GitHub/rus_traffic_accidents/")
raw_data <- data.frame(list_file)

### transform .csv file to split the columns

raw_data <- raw_data %>% filter(!is.na(Номер))
raw_data <- unique(raw_data)
raw_data <- raw_data %>% select(Номер, Дата, Время, Схема, Широта, Вид.ДТП,
                                     Адрес, Дорога, Категория.дороги, Состояние.погоды.1, 
                                     Состояние.проезжей.части, Освещение)

raw_data$weather_cond <- paste(raw_data$Состояние.погоды.1,
                               raw_data$Состояние.проезжей.части)
raw_data$road_cond <- raw_data$Освещение
raw_data$date <- raw_data$Дата
raw_data$time <- raw_data$Время
raw_data$id <- raw_data$Схема
raw_data$latitude <- raw_data$Широта
raw_data$longitude <- raw_data$Вид.ДТП
raw_data$road_cat <- raw_data$Категория.дороги
raw_data$type <- raw_data$Адрес
raw_data$district <- raw_data$Дорога

raw_data <- raw_data %>% select(date, time, type, district, latitude, longitude, 
                                road_cat, road_cond, weather_cond)

raw_data$date <- dmy(raw_data$date)
raw_data <- raw_data %>% arrange(date, time, type, district)
raw_data$time <- as.POSIXct(raw_data$time, format = "%H:%M")
raw_data$key <- paste(raw_data$date, raw_data$time, raw_data$type, raw_data$district)

### make key in xml file

xmldf$key <- paste(xmldf$date, xmldf$time, xmldf$DTPV, xmldf$district)

### check if all rows match by the key

set_raw <- raw_data
set_xml <- xmldf
d1 <- data.frame(set_raw, set_xml)
d1$check <- d1$key == d1$key.1
sum(d1$check == TRUE)
rm(set_raw, set_xml, d1)
rm(files, coords_dtp, driving_mode, road_type)

raw_data_sel <- raw_data %>% select(key.1 = key, latitude, longitude, road_cat,
                                    road_cond, weather_cond)

### merge two datasets

d2 <- cbind(xmldf, raw_data_sel)
rm(list_file, xmldf, raw_data, raw_data_sel)

### kick temporary columns

d2 <- d2 %>% select(dtpv = DTPV, date, time, district, kts = KTS, kuch = KUCH, 
                    fatal = POG, injury = RAN, driving_mode, latitude, 
                    longitude, road_cat, road_cond, weather_cond)

# save raw data

write.csv(d2, "raw_data.csv")
