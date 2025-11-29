# Load necessary packages
library(tidyverse)      # for data manipulation
library(readr)          # for reading CSV / data files
library(lubridate)      # for dates (if needed)
library(sf)             # for spatial / GIS (if using shapefiles / maps)
library(MatchIt)        # for propensity score matching (optional)
library(broom)          # for tidy model outputs


# View first rows
head(combined_data)

# Summary statistics
summary(combined_data)

# Check missingness
colSums(is.na(combined_data))


### Exploratory data analysis (EDA)

# e.g., scatterplot: provider density vs outcome
ggplot(df, aes(x = x, y = y)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(x = "", y = "")

# Summary stats by e.g. quartiles of provider density
df %>%
  mutate(pd_quart = ntile(x, 4)) %>%
  group_by(pd_quart) %>%
  summarise(mean_outcome = mean(y, na.rm = TRUE),
            sd_outcome = sd(y, na.rm = TRUE),
            n = n())

# 6. Basic multivariable regression (adjusting for confounders)

model1 <- lm(y ~ x + sdoh_index + 
               some_other_covariates, data = df)
summary(model1)
tidy(model1)

# 7. (Optional) Propensity score matching to approximate causal inference

# Define “treatment” as e.g. being in top quartile of provider_density
df <- df %>%
  mutate(treat = if_else(x >= quantile(x, 0.75, na.rm = TRUE), 1, 0))

# Propensity score model
ps_model <- glm(treat ~ sdoh_index + other_covariates, data = df, family = binomial())
df$pscore <- predict(ps_model, type = "response")

# Match treated vs control
m.out <- matchit(treat ~ sdoh_index + other_covariates, 
                 data = df, method = "nearest", distance = "logit")
matched <- match.data(m.out)

# Check balance
summary(m.out)

# Outcome comparison post-match
lm_matched <- lm(y ~ treat, data = matched)
summary(lm_matched)

# 8. (Optional) Stratified / subgroup analyses — e.g. by SDOH index, urban/rural, minority proportion, etc.

df %>%
  group_by(some_subgroup_variable) %>%
  do(tidy(lm(y ~ x + sdoh_index + other_covariates, data = .))) %>%
  unnest(cols = c(.))

# 9. (Optional) Map / spatial visualization (if you have shapefiles or county polygons)

# If shapefile loaded as `counties_sf`
# merged_data <- counties_sf %>% left_join(df, by = "fips")
# ggplot(merged_data) + geom_sf(aes(fill = outcome_var))

# 10. Document limitations & next steps
