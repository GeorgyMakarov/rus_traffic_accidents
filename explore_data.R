# Exploratory data analysis -----------------------------------------------

# prerequisite packages

library(dplyr)
library(ggplot2)

# read data

clean_data <- read.csv("clean_data.csv")
clean_data <- clean_data %>% select(-X)
head(clean_data, 10)

# histogram of accidents by the type of accident

dtpv <- clean_data %>% select(dtpv) %>% mutate(count = 1) %>% group_by(dtpv) %>% 
    summarise(count = sum(count)) %>% arrange(desc(count))

ggplot(dtpv) +
    theme_classic() +
    geom_col(aes(x = reorder(dtpv, count), y = count), fill = "steelblue") +
    coord_flip() + xlab("type of accident") + ylab("number of accidents") +
    ggtitle("Number of accidents by type")

# histogram of accidents by district

district <- clean_data %>% select(district) %>% mutate(count = 1) %>% 
    group_by(district) %>% summarise(count = sum(count)) %>% arrange(desc(count))

ggplot(district) +
    theme_classic() +
    geom_col(aes(x = reorder(district, count), y = count), fill = "steelblue") +
    coord_flip() + xlab("district") + ylab("number of accidents") +
    ggtitle("Number of accidents by district")

# histogram of accidents by driving mode after accident

driving_mode <- clean_data %>% select(driving_mode) %>% mutate(count = 1) %>% 
    group_by()