#Author: Cathea M. Carey, MPH
#Purpose: For Research Consultant 3 Skills Assessment due 10 am 12/12/25

## Data Extraction, Transformation, and final Load for Analysis

# Packages
library(readxl)
library(purrr)
library(dplyr)
library(tidyr)
library(janitor)
library(gt)
library(gridExtra)
library(quantreg)
library(tibble)
library(broom)

# Load files and establish environment (optional)

# NOTE - Data is on external drive

# Check driver for files

list.files("/Volumes/LaCie/DropBox/Github/Skills_Assessment_Dec_2025")

# Check xlsx for sheet names
file_path <- "/Volumes/LaCie/DropBox/Github/Skills_Assessment_Dec_2025/pilot_rct_data.xlsx"  

sheet_names <- excel_sheets(path = file_path)

# Add sheets as list then transform into DF (left join)

datalist <- lapply(sheet_names, function(sheet_name) {
  read_excel(path = file_path, sheet = sheet_name)
})

# Intermediate Data Frames for initial cleaning and final analysis

df_outcomes <- as.data.frame(datalist[[1]])

df_attendance <- as.data.frame(datalist[[2]])

# Check Heads
head(df_attendance)

head(df_outcomes)

# Final Data Frame for final analysis featuring left join of df_attendance to df_outcomes

df <- left_join (df_outcomes, df_attendance, by = "record_id")

# Check Head

head(df) 

# Create Calculated fields (Age, Race/Ethnicity, PHQ-9, UCLA - LS- 20, and creating Dosages as a function of Attendance in Invention Tx arm only)

# Age (bins) - From Continuous Variable to Bins; 

hist(df$age) #review original field

# New Field
df$age_range <-  cut(df$age, # Suggested based on Histogram == 20 - 34, 35 - 49, 50 - 64, 64 and up
                     breaks=c(20, 35, 49, 64, 80),
                     labels=c('20 - 34 years old', '35 - 49 years old', '50 - 64 years old', '64 years old or older'))

table(df$age_range) #spot check

table(df$age) #reference line for above spot check, note best practice would be to create a separate subset or frame with Primary Key (PK), age, and age_range

#Race/Ethnicity (New Variable Names/Labels)

table(df$race_ethnicity) #review original field

table(df$race_nl_specify) #review original field

# New Field
df$raceethnicity_recoded <- case_when(
  df$race_ethnicity == "southeast Asian" ~ "Asian American", #fast hard coding of n = 2 fields to save time, note best practice is to still review in the same fashion as age
  df$race_ethnicity == "Nigerian" ~ "Black/AA",
  TRUE ~ df$race_ethnicity
)
table(df$raceethnicity_recoded)


# PHQ-9 Total (by time points)

# Find column numbers and quick calculate

which(names(df) %in% c("ucla_b_1", "ucla_3_1", "ucla_6_1","phq_b_1", "phq_3_1", "phq_6_1", "attend_s1", "attend_s6")) #Use this hack to file the column numbers to reference below

df$phq_b_total <- rowSums((df[, 68:76])) # Baseline PHQ-9 total

hist(df$phq_b_total) #review original field

df$phq_3_total <- rowSums((df[, 77:85])) # 3 month PHQ-9 total

hist(df$phq_3_total) #review original field

df$phq_6_total <- rowSums((df[, 86:94])) # 6 month PHQ-9 total

hist(df$phq_6_total) #review original field

# PHQ-9 X Sub-categories (by time points) PHQ-9 scores of 5== mild, 10 == moderate, 15 ==moderately severe, and 20 severe depression
df <- df %>% # Baseline PHQ-9 total
  dplyr::mutate(
    phq_b_cat = case_when(
      phq_b_total <= 5 ~ "Mild Depression",
      phq_b_total <= 10 ~ "Moderate Depression",
      phq_b_total <= 15 ~ "Moderate Severe Depression",
      phq_b_total <= 20 ~ "Severe Depression",  
      TRUE ~ NA_character_
    )
  )
df <- df %>% #Factor levels
  mutate(
    phq_b_cat = factor(
      phq_b_cat,
      levels = c("Mild Depression", "Moderate Depression","Moderate Severe Depression","Severe Depression"),
      ordered = TRUE
    )
  )

df <- df %>% # 3 month PHQ-9 total
  dplyr::mutate(
    phq_3_cat = case_when(
      phq_3_total <= 5 ~ "Mild Depression",
      phq_3_total <= 10 ~ "Moderate Depression",
      phq_3_total <= 15 ~ "Moderate Severe Depression",
      phq_3_total <= 20 ~ "Severe Depression",  
      TRUE ~ NA_character_
    )
  )
df <- df %>% #Factor levels
  mutate(
    phq_3_cat = factor(
      phq_3_cat,
      levels = c("Mild Depression", "Moderate Depression","Moderate Severe Depression","Severe Depression"),
      ordered = TRUE
    )
  )

#df$phq_6_cat <- rowSums((df[, 86:94])) 

df <- df %>% # 6 month PHQ-9 total
  dplyr::mutate(
    phq_6_cat = case_when(
      phq_6_total <= 5 ~ "Mild Depression",
      phq_6_total <= 10 ~ "Moderate Depression",
      phq_6_total <= 15 ~ "Moderate Severe Depression",
      phq_6_total <= 20 ~ "Severe Depression",  
      TRUE ~ NA_character_
    )
  )
df <- df %>% #Factor levels
  mutate(
    phq_6_cat = factor(
      phq_6_cat,
      levels = c("Mild Depression", "Moderate Depression","Moderate Severe Depression","Severe Depression"),
      ordered = TRUE
    )
  )

# UCLA-LS-20 Total (by time points) 
df$ucla_b_total <- rowSums((df[, 8:27])) # Baseline UCLA-LS-20 total

hist(df$ucla_b_total) #review original field

df$ucla_3_total <- rowSums((df[, 28:47])) # 3 month UCLA-LS-20 total

hist(df$ucla_3_total) #review original field

df$ucla_6_total <- rowSums((df[, 48:67])) # 6 month UCLA-LS-20 total

hist(df$ucla_6_total) #review original field


# UCLA-LS-20 X Sub-categories (by time points) cutoff at >= 39; >=43; and >=53
df <- df %>% # >= 39; >=43; and >=53
  mutate(  # Baseline UCLA-LS-20 
    uclas_b_cut39 = ifelse(ucla_b_total >= 39, 1, 0),
    uclas_b_cut43 = ifelse(ucla_b_total >= 43, 1, 0),
    uclas_b_cut53 = ifelse(ucla_b_total >= 53, 1, 0)
  )

df <- df %>% # >= 39; >=43; and >=53
  mutate(  # 3 month UCLA-LS-20 
    uclas_3_cut39 = ifelse(ucla_3_total >= 39, 1, 0),
    uclas_3_cut43 = ifelse(ucla_3_total >= 43, 1, 0),
    uclas_3_cut53 = ifelse(ucla_3_total >= 53, 1, 0)
  )

df <- df %>% # >= 39; >=43; and >=53
  mutate(  # 6 month UCLA-LS-20
    uclas_6_cut39 = ifelse(ucla_6_total >= 39, 1, 0),
    uclas_6_cut43 = ifelse(ucla_6_total >= 43, 1, 0),
    uclas_6_cut53 = ifelse(ucla_6_total >= 53, 1, 0)
  )

# Attendance (bins) - prepare for factor analysis of "treatment dosage"

# Test look into Attendance as a continuous variable by re-coding qualitative flags into quantitative to treat dosage as a product of attendance assumed linear

values <- c("no" = 0, "partial" = 0.5, "yes" = 1)

# Dosage Total

df$trt_dosage_total <- rowSums(
  sapply(df[, 95:100], function(x) values[x]),
  na.rm = TRUE
)

hist(df$trt_dosage_total) #review original field

# Dosage Category (labeled) - Sample label that should be vetted

bh_dosage_total <- ncol(df[, 95:100])

df$dosage_prop <- (rowSums(df[, 95:100] == "yes", na.rm = TRUE) + #treats yes as full dosage, partial as half dosage, no as missed dosage
                     0.5 * rowSums(df[, 95:100] == "partial", na.rm = TRUE)) / bh_dosage_total

df$trt_dosage <- cut(
  df$dosage_prop,
  breaks = c(-0.01, 0, 0.33, 0.66, 1),   # include 0 as its own interval
  labels = c("Control", "Minimal Treatment", "Partial Treatment", "Full Treatment")
)

# Make subsets for potential comparison analysis and data quality check

subset_BHarm_df <- subset(df, arm == "Intervention") #spot check to make sure left join was successful = correct attendance for intervention only

subset_TAUarm_df <- subset(df, arm == "Control") #spot check to make sure left join was successful = correct attendance for intervention only



# Suggested: Make a Table 1 (version is for internal but include total and use data suppression for small Ns)

#Overall Dosage for Summary and to answer Q1-3

hist(subset_BHarm_df$trt_dosage_total) #review original field and print
#average
avg_trt_dosage <- mean(subset_BHarm_df$trt_dosage_total)
#standard deviation
sd_trt_dosage <- sd(subset_BHarm_df$trt_dosage_total)
#median
mdn_trt_dosage <- median(subset_BHarm_df$trt_dosage_total)

# Now at the record level for Df summary
subset_BHarm_df$all_dosage <- subset_BHarm_df$dosage_prop == 1 #count full dosage
full_dosage_rate <- mean(subset_BHarm_df$all_dosage) # full dosage rate
full_dosage_n <- sum(subset_BHarm_df$all_dosage) # sum of full dosage for stat
full_dosage_n_total <- nrow(subset_BHarm_df) # another way to count denominator for retention


# Table 1 Var
demo_char_table1 <- df %>%
  select(arm, age_range, gender, raceethnicity_recoded, phq_b_total, phq_3_total, phq_6_total, ucla_b_total, ucla_3_total, ucla_6_total,trt_dosage, trt_dosage_total, income)

# Level with intervention arm
demo_char_table1$arm <- factor(demo_char_table1$arm, levels = c("Intervention", "Control"))

# Prep diff var types
categorical_vars <- c("age_range", "gender", "raceethnicity_recoded", "trt_dosage")
continuous_vars <- c("phq_b_total", "phq_3_total","phq_6_total","ucla_b_total", "ucla_3_total","ucla_6_total","income", "trt_dosage_total")

# Cat variables as N (%)
cat_table <- demo_char_table1 %>%
  select(arm, all_of(categorical_vars)) %>%
  pivot_longer(cols = -arm, names_to = "characteristic", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(characteristic = recode(characteristic,
                                 age_range = "Age (bin)",
                                 gender = "Gender",
                                 raceethnicity_recoded = "Race/Ethnicity",
                                 trt_dosage = "Intervention Dosage")) %>%
  group_by(characteristic, value, arm) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(characteristic, arm) %>%
  mutate(total = sum(n),
         n_perc = paste0(n, " (", round(100*n/total,1), "%)")) %>%
  select(characteristic, value, arm, n_perc) %>%
  pivot_wider(names_from = arm, values_from = n_perc, values_fill = "0 (0%)") %>%
  arrange(characteristic)

# Cont. variables as Mean (SD)
cont_table <- demo_char_table1 %>%
  select(arm, all_of(continuous_vars)) %>%
  pivot_longer(cols = -arm, names_to = "characteristic", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(characteristic = recode(characteristic,
                                 phq_b_total = "PHQ-9 at baseline (Mean(SD))",
                                 phq_3_total = "PHQ-9 at 3 months (Mean(SD))",
                                 phq_6_total = "PHQ-9 at 6 months (Mean(SD))",                                 
                                 ucla_b_total = "UCLA - LS - 20 at baseline (Mean(SD))",
                                 ucla_3_total = "UCLA - LS - 20 at 3 months(Mean(SD))",
                                 ucla_6_total = "UCLA - LS - 20 at 6 months (Mean(SD))",
                                 income = "Annual Household Income in USD (Mean(SD))",
                                 trt_dosage_total = "Total Doseage of Treatment (Mean(SD))"
  )) %>%
  group_by(characteristic, arm) %>%
  summarise(mean_sd = paste0(round(mean(value),1), " (", round(sd(value),1), ")"),
            .groups = "drop") %>%
  pivot_wider(names_from = arm, values_from = mean_sd)

# Combine both type of vars tables
draft_demo_table <- bind_rows(cat_table, cont_table)

# Clean column names
draft_demo_table <- draft_demo_table %>% janitor::clean_names()

# Final Table
draft_demo_table %>%
  gt() %>%
  tab_header(title = "Demographic and Baseline Characteristics by Treatment") %>%
  cols_label(
    value = "Characteristics",
    intervention = "Intervention (N(%))",
    control = "Treatment-as-Usual (N(%))"
  ) %>%
  fmt_missing(columns = everything(), missing_text = "")

# Ungroup if it's grouped — grouped tables sometimes don't print well
#df <- draft_demo_table %>% ungroup()

png("table_1_output.png", width = 1000, height = 800)

grid.table(df)

dev.off()


## Code for Key Research Questions

# Question 1 (part A) How well were participants retained? 


# Question 1 (part B) What is implied about the feasibility of a larger trial?

# Question 2 Does the intervention seem to reduce loneliness (primary) or depression (secondary)?


# Paired Wilcoxon
test1 <- wilcox.test(df$ucla_b_total, df$ucla_6_total, paired = TRUE, conf.int = TRUE, conf.level = 0.95)


# Question 3 Are there participant characteristics (e.g., baseline loneliness or depression severity, attendance, or demographic characteristics) that appear to be associated with outcomes or that may influence how we interpret the intervention’s effects?

#rq(outcome ~ arm + age + gender + income, data = df, tau = 0.5)

