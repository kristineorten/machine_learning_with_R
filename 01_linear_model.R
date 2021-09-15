# Load libraries
library(tidyverse)
library(readxl)
library(tidymodels)

# Read files
houses_raw <- read_excel("temp/houses.xlsx")

# Ser at vi har mange kommuner som forekommer sjeldent
houses_raw %>% 
  count(kommune_name, sort = T) %>% 
  #filter(n < 15)
  filter(n >= 15)

# Select columns
houses <- houses_raw %>% 
  select(id, sqm, expense, tot_price, lat, lng, kommune_name) %>% 
  mutate(kommune_name = fct_lump_n(kommune_name, 220)) #Setter alle unntatt de 220  mest frekvente kommunene til Other

# Split in training and test
set.seed(42) #Kan velge andre nummer, hindrer ny splitting hver gang
split <- initial_split(houses, 3/4) #25 % test, 75 % training

train_raw <- training(split)
test_raw <- testing(split)

# Remove id (vil bli støy siden den ikke skal ha noe å si i prediksjonen) 
train <- train_raw %>% 
  select(-id)

test <- test_raw %>% 
  select(-id)

# Train model
model <- boost_tree(trees=350)
model <- linear_reg() %>% 
  set_engine("lm") %>% #lm: least square methods
  fit(tot_price ~ ., data = train) #prediker tot_price, bruk alle andre variabler for predikering

summary(model$fit)

# Test model
model_preds <- predict(model, test) %>% 
  bind_cols(test_raw) %>% 
  rename(estimate     = .pred, 
         truth        = tot_price) %>% 
  mutate(abs_dev      = abs(truth - estimate),
         abs_dev_prec = abs_dev / truth)

# Evaluate model
mape(model_preds, truth, estimate) #mean abs perc error
