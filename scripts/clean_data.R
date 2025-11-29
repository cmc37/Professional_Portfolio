library(dplyr)
library(stringr)
# Quick look at structure and first few rows
str(ahrf_data)
head(ahrf_data)

str(placescdc_data)
head(placescdc_data)

# Check variable names
names(ahrf_data)
names(placescdc_data)

# Create Crude Linkage
placescdc_data <- placescdc_data %>%
  mutate(county_name = paste0(LocationName, ", ", toupper(StateAbbr)),
         StateAbbr = toupper(StateAbbr))

# Merge AHRF and CDC PLACES on county_name
combined_data <- ahrf_data %>%
  inner_join(placescdc_data, 
             by = c("cnty_name_st_abbrev" = "county_name"))

###END - NOW HAVE A DATASET FOR EXPLORATORY ANALYSIS AT THE COUNTY LEVEL
