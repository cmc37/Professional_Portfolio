library(tidyverse)
df <- read_csv("data/clinic_data.csv")
df_clean <- df %>% mutate(visit_date = lubridate::ymd(visit_date))
print(head(df_clean))
