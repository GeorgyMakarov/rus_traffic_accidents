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


# filter columns in the top of the table - they contain general info not needed
# for the analysis, because this infor tells us about region - St. Pete, but we
# already have filtered St. Pete.

list_file <- list_file %>% 
    filter(!(is.na(DTPV))) %>% select(-text)
list_file$date <- dmy(list_file$date)
list_file$time <- as.POSIXct(list_file$time, format = "%H:%M")
xmldf <- list_file %>% arrange(date, time)
rm(list_file)

# check data summary
# need to transfor to num: KTS, KUCH, POG, RAN
summary(xmldf)

# transform columns to numeric
xmldf$KTS <- as.numeric(xmldf$KTS)
xmldf$KUCH <- as.numeric(xmldf$KUCH)
xmldf$POG <- as.numeric(xmldf$POG)
xmldf$RAN <- as.numeric(xmldf$RAN)


# compare desired and available data
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




### testing regex
text11 <- "Режим движения не изменялся30.2716959.946269Местного значения (дорога местного значения, включая относящиеся к собственности поселений, муниципальных районов, городских округов)Сведения отсутствуют73 23Магистральные дороги00г"
text22 <- "Режим движения не изменялся30.2716959.946269Местного значения (дорога местного значения, включая относящиеся к собственности поселений, муниципальных районов, городских округов)Сведения отсутствуют73 23Магистральные дороги00г"
text3 <- "Движение частично перекрыто30.13045559.999329Местного значения (дорога местного значения, включая относящиеся к собственности поселений, муниципальных районов, городских округов)Сведения отсутствуютПролетарский прМагистральные улицы"
case1 <- 1
case2 <- 2
case3 <- 3

df1 <- tribble(
    ~case, ~info,
    case1, text11,
    case2, text2,
    case3, text3
)

df11 <- df1 %>% extract(info, c("text1"), "([:digit:]+\\.[:digit:]+\\.[:digit:]+)", remove = FALSE)





