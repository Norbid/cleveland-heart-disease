library(tidyverse)

# 2. Exploratory data analysis 

# 2.1. Univariate profile

cleveland %>% 
  select(-class) %>% 
  summary()

ggplot(cleveland, aes(x = age)) +
  geom_histogram(bins = 20) +
  labs(title = "Age distribution", x = "Age (years)", y = "Count")

# 2.2 Heart disease prevalence by sex

cleveland %>% 
  count(sex, hd) %>% 
  group_by(sex) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup()

ggplot(cleveland, aes(x = sex, fill = hd)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Heart disease prevalence by sex",
    x = "Sex",
    y = "Proportion"
  )

# 2.3. Chest pain type vs heart disease

cleveland %>% 
  count(cp, hd) %>% 
  group_by(cp) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup()


ggplot(cleveland, aes(x = cp, fill = hd)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Heart disease proportion by chest pain type",
    x = "Chest pain type",
    y = "Proportion"
  )

# 2.4. Key numeric predictors vs hd

cleveland %>% 
  group_by(hd) %>% 
  summarise(
    mean_thalach = mean(thalach, na.rm = TRUE),
    mean_oldpeak = mean(oldpeak, na.rm = TRUE),
    mean_trestbps = mean(trestbps, na.rm = TRUE),
    mean_chol = mean(chol, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(cleveland, aes(x = hd, y = thalach)) +
  geom_boxplot() +
  labs(title = "Max heart rate by heart disease status", x = "", y = "Max HR (thalach)")

# 2.5. Correlation with hd

cleveland_num <- cleveland_raw %>%
  select(-index) %>%
  mutate(
    ca   = as.numeric(na_if(ca, "Invalid Number")),
    thal = as.numeric(na_if(thal, "Invalid Number")),
    hd_num = if_else(class > 0, 1, 0)
  )

cor(cleveland_num, use = "pairwise.complete.obs")[, "hd_num"] %>%
  sort(decreasing = TRUE)
