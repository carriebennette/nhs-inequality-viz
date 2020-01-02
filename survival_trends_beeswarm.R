library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(beeswarm)

#### LOAD RAW DATA #####

# load deprivation indices
deprivation_index <- read_csv("nhs_data/File_13_ID_2015_Clinical_Commissioning_Group_Summaries_IMD.csv") %>%
  mutate(ccg_code = `Clinical Commissioning Group Code (2015)`,
         avg_rank = `IMD - Average rank`,
         avg_score = `IMD - Average score`,
         prop_most_deprived = `IMD - Proportion of LSOAs in most deprived 10% nationally`) %>%
  select(ccg_code, avg_rank, avg_score, prop_most_deprived)

deprivation_health_index <- read_csv("nhs_data/File_13_ID_2015_Clinical_Commissioning_Group_Summaries_health.csv") %>%
  mutate(ccg_code = `Clinical Commissioning Group Code (2015)`,
         avg_rank = `Health Deprivation and Disability - Average rank`,
         avg_score = `Health Deprivation and Disability - Average score`,
         prop_most_deprived = `Health Deprivation and Disability - Proportion of LSOAs in most deprived 10% nationally`) %>%
  select(ccg_code, avg_rank, avg_score, prop_most_deprived)

deprivation_income_index <- read_csv("nhs_data/File_13_ID_2015_Clinical_Commissioning_Group_Summaries_income.csv") %>%
  mutate(ccg_code = `Clinical Commissioning Group Code (2015)`,
         avg_rank = `Income - Average rank`,
         avg_score = `Income - Average score`,
         prop_most_deprived = `Income - Proportion of LSOAs in most deprived 10% nationally`) %>%
  select(ccg_code, avg_rank, avg_score, prop_most_deprived)


# Function to load raw survival data (and join with deprivation indices)
LoadData <- function(file_name){
  survival <- read.csv(paste0("nhs_data/", file_name), stringsAsFactors = FALSE) %>%
    gather(starts_with("survival_"), key = year, value = survival) %>%
    mutate(year = case_when(year == "survival_01" ~ "2001", 
                            year == "survival_02" ~ "2002", 
                            year == "survival_03" ~ "2003", 
                            year == "survival_04" ~ "2004", 
                            year == "survival_05" ~ "2005", 
                            year == "survival_06" ~ "2006", 
                            year == "survival_07" ~ "2007", 
                            year == "survival_08" ~ "2008",
                            year == "survival_09" ~ "2009", 
                            year == "survival_10" ~ "2010", 
                            year == "survival_11" ~ "2011", 
                            year == "survival_12" ~ "2012", 
                            year == "survival_13" ~ "2013", 
                            year == "survival_14" ~ "2014", 
                            year == "survival_15" ~ "2015", 
                            year == "survival_16" ~ "2016", 
                            TRUE ~ year)) %>%
    distinct() %>%
    mutate(ccg_code = Geography.code) %>%
    left_join(deprivation_index, by = "ccg_code") %>%
    filter(!is.na(avg_rank)) %>%
    mutate(tertile_rank = as.character(quantileCut(avg_rank, 3))) 
  
  return(survival)
  
}

# load colorectal cancer, breast cancer, and lung cancer datasets:
crc_survival <- LoadData("crc_survival_trends.csv")
breast_survival <- LoadData("breastca_survival_trends.csv")
lung_survival <- LoadData("lungca_survival_trends.csv")


# create scatter plots (with a little jitter so not all points fall along a single line)
ggplot(crc_survival) + 
  geom_jitter(aes(year, survival, color = avg_score), alpha = 0.7) +
  #geom_smooth(aes(year, survival, color = avg_score, group = tertile_rank), method = "gam" ) + 
  theme(legend.position = "none") +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red") +
  ylim(0, 100)

ggplot(breast_survival) + 
  geom_jitter(aes(year, survival, color = avg_score), alpha = 0.7) +
  #geom_smooth(aes(year, survival, color = avg_score, group = tertile_rank), method = "gam" ) + 
  theme(legend.position = "none") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  ylim(0, 100)

ggplot(lung_survival) + 
  geom_jitter(aes(year, survival, color = avg_score), alpha = 0.7) +
  #geom_smooth(aes(year, survival, color = avg_score, group = tertile_rank), method = "gam" ) + 
  theme(legend.position = "none") +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red") +
  ylim(0, 100)

# also look at beeswarm type of plots
ggplot(lung_survival) + 
  geom_beeswarm(aes(year, survival, color = avg_score), alpha = 0.7, dodge.width = -1) +
  theme(legend.position = "none") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  ylim(0, 100)

### ultimatley will need some combination of the two types of plots... 