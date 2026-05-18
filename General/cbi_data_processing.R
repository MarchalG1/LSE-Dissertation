setwd("~/Dissertation/")

library(tidyverse)
library(readxl)

cbi_data <- read_excel("General/CBIData_Romelli_2025.xlsx", sheet = 2)

small <- cbi_data %>%
  select(country, year, cbi_)