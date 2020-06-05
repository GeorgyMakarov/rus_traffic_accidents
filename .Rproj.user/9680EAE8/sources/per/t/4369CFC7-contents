# Exploratory data analysis -----------------------------------------------

# prerequisite packages

library(dplyr)
library(ggplot2)
library(lubridate)

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
head(sev_rate)

# Analysis of frequency ---------------------------------------------------

# histogram of casualties by month and day of week

ggplot(sev_rate) + geom_col(aes(month, casualties, fill = day)) +
    ggtitle("Casualties in 2019 by month and day of week")

# bubble chart of accidents by time and casualties
# exclude dust and dirty road conditions as there are few accidents

nodirt_nodust <- sev_rate %>% filter(!road_cond %in% c("dirty", "dust"))
no_sleet <- nodirt_nodust %>% filter(!road_cond == "sleet")

ggplot(no_sleet) + 
    geom_point(aes(x = time, y = casualties, 
                   size = casualties, color = sev_class), alpha = 0.5) +
    facet_grid(vars(day), vars(road_cond)) +
    #scale_color_gradientn(colors = c("blue", "yellow", "red")) +
    theme(axis.text = element_blank(), axis.ticks = element_blank()) +
    xlab("") + ylab("")

# severity of accident by number of participants and vehicles and types

ggplot(sev_rate, aes(kts, dtpv, fill = severity)) +
    geom_tile(color = "white") + xlab("number of vehicles") +
    scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9)) +
    facet_wrap(~ kuch, nrow = 2) + ylab("")

# map of serious and moderate accidents

library(plotly)
fig <- sev_rate
fig <- fig %>%
    plot_ly(
        type = 'densitymapbox',
        lat = ~ latitude,
        lon = ~ longitude,
        coloraxis = 'severity',
        radius = 10)

fig
