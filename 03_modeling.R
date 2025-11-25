library(tidyverse)
library(pROC)

# 3. Modeling – Logistic regression for heart disease

# 3.1. Train/test split

set.seed(123)

n <- nrow(cleveland)
test_idx <- sample(seq_len(n), size = round(0.3 * n))

cleveland_train <- cleveland %>% slice(-test_idx)
cleveland_test  <- cleveland %>% slice(test_idx)

# 3.2. Fit logistic regression

cleveland_train_num <- cleveland_train %>% 
  mutate(
    ca   = as.numeric(ca),
    thal = as.numeric(thal),
    hd_num = if_else(class > 0, 1, 0)
  )

logit_model <- glm(
  hd_num ~ age + sex + cp + trestbps + chol + fbs + restecg +
    thalach + exang + oldpeak + slope + ca + thal,
  data = cleveland_train_num,
  family = binomial
)

summary(logit_model)

# 3.3. Predictive performance (AUC / ROC)

library(pROC)

cleveland_test_num <- cleveland_test %>%
  mutate(
    ca   = as.numeric(ca),
    thal = as.numeric(thal),
    hd_num = if_else(class > 0, 1, 0)
  )

test_probs <- predict(logit_model, newdata = cleveland_test_num, type = "response")

roc_obj <- roc(cleveland_test_num$hd_num, test_probs)
auc(roc_obj)  # AUC value

plot(roc_obj, main = "ROC curve – logistic model for heart disease")

pred_class <- if_else(test_probs > 0.5, 1, 0)

table(
  Truth = cleveland_test_num$hd_num,
  Pred  = pred_class
)








