library(tidyverse)
library(pROC)

# 1. Cleaning & feature engineering

# 1.1. Load data 

cleveland_raw <- read_csv("Cleveland_hd.csv")

cleveland <- cleveland_raw %>% 
  select(-index) # index is just a row id

summary(cleveland)

# 1.2. Clean 'ca' and 'thal'

cleveland <- cleveland %>% 
  mutate(
    ca   = na_if(ca,   "Invalid Number"),
    thal = na_if(thal, "Invalid Number"),
    ca   = as.integer(ca),
    thal = as.integer(thal)
  )

# 1.3. Create binary outcome 'hd'

cleveland <- cleveland %>% 
  mutate(
    hd = if_else(class > 0, 1L, 0L)  # 1 = heart disease, 0 = none
  )

# 1.4. Make some variables factors (for plotting / modeling)

cleveland <- cleveland %>% 
  mutate(
    sex    = factor(sex, levels = c(0, 1), labels = c("Female", "Male")),
    cp     = factor(cp),
    fbs    = factor(fbs, levels = c(0, 1), labels = c("<=120", ">120")),
    restecg = factor(restecg),
    exang  = factor(exang, levels = c(0, 1), labels = c("No", "Yes")),
    slope  = factor(slope),
    hd     = factor(hd, levels = c(0, 1), labels = c("No disease", "Disease"))
    # keep 'class' as numeric for severity if needed
  )
