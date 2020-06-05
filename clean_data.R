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


# Translate data to English -----------------------------------------------

# read in the data

raw_data <- read.csv("raw_data.csv")
raw_data <- raw_data %>% select(-X)

# check unique values in text columns and subsitute them with English words
str(raw_data)

## check accident types
unique(raw_data$dtpv)

raw_data <- raw_data %>% 
    mutate(dtpv_temp = 
               ifelse(
                   dtpv == "Столкновение", 
                              "collision", 
                   ifelse(dtpv == "Наезд на пешехода",
                                                  "pedestrian_hit", 
                          ifelse(dtpv == "Наезд на препятствие", "obstacle_hit",
                                 ifelse(dtpv == "Наезд на стоящее ТС", "hit_run",
                                        ifelse(dtpv == "Опрокидывание", "rollover",
                                               ifelse(dtpv == "Съезд с дороги", "off_road",
                                                      ifelse(dtpv == "Падение пассажира", "passenger_fall",
                                                             ifelse(dtpv == "Иной вид ДТП", "other",
                                                                    ifelse(dtpv == "Наезд на велосипедиста", "bicycle_hit",
                                                                           ifelse(dtpv == "Наезд на внезапно возникшее препятствие", "immediate_hit",
                                                                                  ifelse(dtpv == "Наезд на лицо, не являющееся участником дорожного движения, осуществляющее несение службы", "policeman_hit",
                                                                                         ifelse(dtpv == "Отбрасывание предмета", "throwing_object", dtpv)))))))))))))


raw_data$dtpv_temp <- as.factor(raw_data$dtpv_temp)
raw_data$dtpv <- raw_data$dtpv_temp
raw_data <- raw_data %>% select(-dtpv_temp)

## convert date to date format
raw_data$date <- ymd(raw_data$date)

## convert time to time
raw_data$time <- as.POSIXct(raw_data$time)

## check districts
unique(raw_data$district)

raw_data$district <- plyr::revalue(raw_data$district,
        c("Василеостровский район" = "vasil", "Калининский район" = "kalin", 
        "Кировский район" = "kirov", "Приморский район" = "primorsk",
        "Адмиралтейский район" = "admiral", "Центральный район" = "center",
        "Красногвардейский район" = "krasnogvard", "Фрунзенский район" = "frunz",
        "Красносельский район" = "krasnosel", "Невский район" = "nevski",
        "Московский район" = "mosk", "Петродворцовый район" = "petrodvorts",
        "Пушкинский район" = "pushkin", "Выборгский район" = "vyborg",
        "Петроградский район"  = "petrograd", "Колпинский район" = "kolpino",
        "Курортный район" = "kurort", "Кронштадтский район" = "kronshtadt"))

## check driving mode
unique(raw_data$driving_mode)

raw_data$driving_mode <- plyr::revalue(raw_data$driving_mode, 
                                       c("Режим движения не изменялся" = "open",
                                         "Движение частично перекрыто" = "part_closed",
                                         "Движение полностью перекрыто" = "closed"))

## check road category
unique(raw_data$road_cat)

raw_data$road_cat <- plyr::revalue(raw_data$road_cat,
                                   c("Местного значения (дорога местного значения, включая относящиеся к собственности поселений, муниципальных районов, городских округов)" = "local",
                                     "Региональная или межмуниципальная (дорога регионального или межмуниципального значения)" = "regional",
                                     "Частная (дорога, относящиеся к частной и иным формам собственности)" = "private",
                                     "Не указано" = "not_specified",
                                     "Федеральная (дорога федерального значения)" = "federal",
                                     "Другие места" = "other"))


## check road conditions
unique(raw_data$road_cond)

raw_data$road_cond <- plyr::revalue(raw_data$road_cond,
                                    c("Обработанное противогололедными материалами" = "wet",
                                      "Заснеженное" = "snow", "Мокрое" = "wet", 
                                      "Загрязненное" = "dirty", "Пасмурно" = "unknown",
                                      "Ясно" = "unknown", "Снегопад" = "snow", "Сухое" = "dry",
                                      "Со снежным накатом" = "snow", "Гололедица" = "sleet",
                                      "Дождь" = "wet", "Пыльное" = "dust"))

## check weather conditions
unique(raw_data$weather_cond)

raw_data$weather_cond <- plyr::revalue(raw_data$weather_cond,
                                       c("Ясно " = "clear", " Пасмурно" = "clouds",
                                         "Пасмурно " = "clouds", "Снегопад " = "snow",
                                         "Сведения отсутствуют " = "unknown",
                                         " Снегопад" = "snow", " Ясно" = "clear",
                                         "Пасмурно Снегопад" = "snow", "Метель " = "snow",
                                         "Сужение проезжей части припаркованным транспортом " = "unknown",
                                         "Дождь " = "rain", "Конструктивное сужение проезжей части вследствие уменьшения количества полос движения " = "unknown",
                                         "Пасмурно Дождь" = "rain", "Пасмурно Метель" = "snow", " Дождь" = "rain", 
                                         "Неправильное применение, плохая видимость дорожных знаков Сведения отсутствуют" = "unknown",
                                         "Сужение проезжей части вследствие проведения работ " = "unknown", "Ясно Дождь" = "rain", "Пасмурно Туман" = "fog",
                                         "Туман " = "fog"))


# Clean data --------------------------------------------------------------

# check unspecified values in factor columns that came from second dataset

levels(raw_data$road_cat)
str(raw_data$road_cat)

levels(raw_data$road_cond) # we have empty rows that do not read as NA
str(raw_data$road_cond)
levels(raw_data$road_cond)[levels(raw_data$road_cond) == ""] <- "unknown"

levels(raw_data$weather_cond)
str(raw_data$weather_cond)

levels(raw_data$driving_mode)
str(raw_data$driving_mode)

levels(raw_data$dtpv)
str(raw_data$dtpv)

# final check on the summary

summary(raw_data)

# write clean data set

write.csv(raw_data, "clean_data.csv")
