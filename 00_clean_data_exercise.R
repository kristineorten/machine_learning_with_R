#################### #
# Attach packages ----
#################### #

library(tidyverse)
library(readxl)
library(writexl)
library(skimr)

#################### #
# Exercise 1 - Import more data sets ----
#################### #

# Insert the appropriate function to import all the data sets from the input folder to R
houses_raw        <- read_excel("input/houses.xlsx")
geo_raw           <- read_excel("input/geo.xlsx")
zip_raw           <- read_excel("input/zip.xlsx")
income_raw        <- read_excel("input/income.xlsx")
attributes_raw    <- read_excel("input/attributes.xlsx")


#################### #
# Exercise 2 - Select relevant variables ----
#################### #

# Run the next lines
geo <- geo_raw %>% 
  select(id, kommune_no, kommune_name, fylke_no, fylke_name)


# Select variables for zip number, average income and average fortune from the income data set
income <- income_raw %>% 
  select(zip_no      = postnr,
         avg_income  = gjsnitt_inntekt,
         avg_fortune = gjsnitt_formue)

# Run the next lines as no changes are needed for zip_raw and attributes_raw
zip        <- zip_raw
attributes <- attributes_raw

#################### #
# Exercise 3 - Merge data ----
#################### #

# Merge all data sets using the left join function to join on the appropriate variable 
houses <- houses_raw %>% 
  left_join(geo, by = "id") %>%
  left_join(zip, by = "id") %>%
  left_join(income, by = "zip_no") %>% 
  left_join(attributes, by = "id")
  
  
##################################### #
# Exercise  4 ---- Prep and plot 
##################################### #

# Run the next lines to replace missing values and calculate total price
houses_output <- houses %>% 
  mutate(debt    = ifelse(is.na(debt), 0, debt),
         expense = ifelse(is.na(expense), 0, expense),
         tot_price         = price + debt, 
         tot_price_per_sqm = tot_price/sqm) %>% 
  drop_na()

# Run the next lines to study the distribution of the price variable
houses_output %>% 
  ggplot(aes(x = tot_price/1000000)) +
  geom_histogram(fill = "dodgerblue3", color = "white") +
  labs(x = "Pris [MNOK]",
       y = "Antall") +
  xlim(0, 10) +
  theme_minimal()

# Run the next lines to study the relation between price and sqm
houses_output %>% 
  ggplot(aes(x = sqm, y = tot_price/1000000)) +
  geom_point(color = "dodgerblue3", alpha = 0.4) +
  labs(x = "Kvadratmeter",
       y = "Pris [MNOK]") +
  theme_minimal() 

# Insert the appropriate function to perform log transformation of both price and sqm
# Study how the relation changes when the effect of outliers is reduced
houses_output %>% 
  ggplot(aes(x = log(sqm), y = log(tot_price/1000000))) +
  geom_point(color = "dodgerblue3", alpha = 0.4) +
  labs(x = "log(Kvadratmeter)",
       y = "log(Pris [MNOK])") +
  theme_minimal()


##################################### #
# Optional Exercise ----
##################################### #

# Optional: Create a new feature of your own choice that you think could have an impact on the model
# For example, create a variable containing the number of houses of within the same zip number

houses_output_w_feature <- houses_output %>% 
  group_by(zip_no) %>% 
  mutate(houses_same_zip_no = n()) %>% 
  ungroup() %>% 
  select(houses_same_zip_no, everything())

#################### #
# Excerise 5 - Write data to excel ----
#################### #

# Insert the appropriate function to save the data set
write_xlsx(houses_output, "temp/houses_exercise.xlsx")