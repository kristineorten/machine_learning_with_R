################### #
# Attach packages ----
################### #

library(tidyverse)
library(tidymodels)
library(readxl)

#################### #
# Exercise 1 - Import data ----
#################### #

# Insert the appropriate function to import the `houses.xlsx` dataset to R
houses_raw <- read_excel("temp/houses.xlsx")

#################### #
# Exercise 2 - Transform data ----
#################### #

# Select variables for sqm, expense, total price, latitude, longitude, and municipality (kommune) from the houses_raw data set
# Also mutate a new column `log_sqm` which is the logarithm of `sqm` 
houses <- houses_raw %>%
  select(sqm, expense, tot_price,
         lat,
         lng,
         kommune_name) %>%
  mutate(kommune_name = fct_lump_n(kommune_name, 220),
         log_sqm      = log(sqm))

#################### #
# Exercise 3 - Split data ----
#################### #

set.seed(42)

# Split 75 % of the data to the training set, and the rest to the test set
split <- initial_split(houses, 4/5)
train <- training(split)
test  <- testing(split)

#################### #
# Exercise 4 - Fit a linear model  ----
#################### #

# Set the target variable as the logarithm of total price and fit the linear model
model <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(log(tot_price) ~ ., data = train)

# Run the next line to view summary of fit
summary(model$fit)

#################### #
# Exercise 5 - Use model for predictions  ----
#################### #

# Use the fitted model to predict the total price for observations in the test set
# Remember to take the exponential of the .pred to return meaningful prediction values
# Also find the appropriate function to rename columns
model_preds <- 
  predict(model, new_data = test) %>%
  bind_cols(test) %>% 
  mutate(.pred = exp(.pred)) %>%     
  rename(estimate     = .pred,
         truth        = tot_price) %>% 
  mutate(abs_dev      = abs(truth - estimate),
         abs_dev_perc = abs_dev/truth)

#################### #
# Exercise 6 - Evaluate model  ----
#################### #

# Evaluate the mean absolute percentage error of our linear model predictions
mape(data     = model_preds,
     truth    = truth,
     estimate = estimate)

##################################### #
# Excerise 7 - Plot results ----
##################################### #

# Run the next lines to study the distribution of the percentage error
model_preds %>%
  ggplot(aes(x = abs_dev_perc)) +
  geom_histogram(fill = "dodgerblue3", color = "white") +
  theme_minimal() +
  labs(x = "Prosentvis feil",
       y = "Antall observasjoner") +
  scale_x_continuous(limits = c(0, 1.5), labels = scales::percent)

