# Load libraries
library(tidyverse)
library(readxl)
library(writexl)
library(skimr)

# Read files
houses_raw <- read_excel("input/houses.xlsx")
geo_raw <- read_excel("input/geo.xlsx")

# Select column
geo <- geo_raw %>% 
  select(id, kommune_no, kommune_name, fylke_no, fylke_name)

# Merge tables
houses <- houses_raw %>% 
  left_join(geo, by="id")

# Overview of table
skim(houses)

# Operation
houses_output <- houses %>% 
  mutate(debt             = ifelse(is.na(debt), 0, debt),
         expense          = ifelse(is.na(expense), 0, expense),
         tot_price        = price + debt,
         tot_price_pr_sqm = tot_price / sqm) %>% 
  drop_na()

# Plot
houses_output %>% 
  ggplot(aes(x = tot_price/1000000)) +
  geom_histogram(fill = "dodgerblue3", color = "white") +
  labs(x = "Pris [MNOK]",
       y = "Antall") +
  xlim(0, 10) +
  theme_minimal()

#  Write to file
write_xlsx(houses_output, "temp/houses.xlsx")

#Merk: shift-ctrl-m %>% 