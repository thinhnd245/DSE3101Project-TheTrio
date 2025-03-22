library(readxl)
library(tidyverse)



raw_data <- read_excel("data/ROUTPUTQvQd.xlsx")
data <- raw_data %>%
  mutate(across(2:239, as.numeric)) %>% 
  drop_na() %>%
  pivot_longer(`ROUTPUT65Q4`:`ROUTPUT25Q1`,names_to = "vintage", values_to = "gdp") %>%
  mutate(year = substr(DATE, start = 1, stop = 4),
         quarter = substr(DATE, start = 7, stop = 7),
         prefix = as.numeric(substr(vintage, start = 8, stop = 9)),
         vin_year = as.numeric(case_when(
           prefix >= 65 ~ paste0(19, prefix), 
           TRUE ~ paste0(20, prefix)
         )),
         vin_quarter = as.numeric(substr(vintage, start = 11, stop = 11))) %>%
  select(year, quarter, vin_year, vin_quarter, gdp)


  