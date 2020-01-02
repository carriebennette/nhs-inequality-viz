
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

file_list <- read.csv("nhs_data/file_names_code.csv", stringsAsFactors = FALSE)

file_names <- file_list$all_ages
names <- file_list$disease

LoadData <- function(file_names, names){
  
  mortality <- read.csv(paste0("nhs_data/", file_names), stringsAsFactors = FALSE) %>%
    filter(ORG_TYPE_DESCRIPTION %in% c("LOCAL AUTHORITIES (boundaries as of April 2009)") | 
             ORG_TITLE == "England") %>%
    mutate(year = YEAR,
           local_area = ifelse(ORG_TITLE == "England", "England", NEW_CODE), 
           rate_all = DSR,
           gender = SEX_CODE) %>%
    select(year, local_area, rate_all, gender) %>%
    mutate(cohort = names) %>%
    distinct() 
  
  return(mortality)
  
}

all_data <- map2(file_names, names, LoadData) %>%
  do.call(rbind, .) %>%
  mutate(gender = case_when(cohort %in% c("breast_cancer", "cervical_cancer")~"F", 
                                   TRUE~gender))

# top and bottom 10%
high_low_estimated <- all_data %>% 
  group_by(cohort, gender) %>%
  mutate(national = mean(subset(rate_all, local_area == "England"), na.rm = T),
         rate_all = ifelse(is.na(rate_all), national, rate_all),
         rank = rank(rate_all),
         rank_group = cut(rank, breaks = 10)) %>%
  mutate(low_rate = mean(subset(rate_all, rank < 32), na.rm = T),
         high_rate = mean(subset(rate_all, rank > 284), na.rm = T)) %>%
  ungroup() %>%
  distinct(high_rate, low_rate, cohort, gender, national) %>%
  mutate(abs_diff = high_rate-low_rate,
         rel_diff = high_rate/low_rate) %>%
  gather("estimate", "rate", 3:5) %>%
  arrange(gender, desc(estimate), rate) %>%
  mutate(cohort2 = factor(cohort, levels = .$cohort))

#scrath plot
ggplot(high_low_estimated %>% 
         filter(gender == "F"), aes(rate, cohort2)) +
  geom_line(aes(group = cohort)) +
  geom_point(aes(color = estimate)) + 
  scale_x_continuous(trans='log2') 

