###### Cathea M. Carey, MPH #######
##### Exploratory Data Analysis (EDA) on Multi-Racial and Erasure

#### Stems from initial conversations and analysis of 2022 Unsheltered PIT Count
#### data set that contains over 600 qualitative responses to a question asking 
#### for the respondent (individual experiencing unsheltered or HUD defined
#### homelessness in a place not meant for human habitation) self-identification
#### for race/ethnicity (gender was also included), prior to the 2024 Data 
#### Standards Update to include Hispanic/Latino/a/x as a racial category 
#### (still keeping Hispanic/Latino/a/x as an ethnicity) and creating Middle
#### Eastern or North African. NOTE: Middle Eastern or North African was not
#### included in 2020 Census; not projected/estimated in Census tables.

#### Question: How does "more than one race" change demographics? 

#### Aim: Visualize census data using Office of Management and Budget (OMB)
#### data standard and then show the data in combination of 'racialized' groups.


#Create Variables
#labels for race
R_labels <- c(
  AAA   = "B03002_006",
  AIANI = "B03002_005",
  BAAA  = "B03002_004",
  HL    = "B03002_012", 
  #MENA NOT PROJECTED OR ESTIMATED
  NHPI  = "B03002_007",
  W     = "B03002_003",
  SORA  = "B03002_008", #Label for "Some Other Race Alone" 
  TOMR  = "B03002_009"  #Label for "Two or More Races
)
#(Race) Labels for Printing/Visuals
R_var_labels <- c(
  AAA = "Asian or Asian American",
  AIANI = "American Indian, Alaskan Native, or Indigenous",
  BAAA = "Black, African, or African American",
  HL =  "Hispanic or Latino/a/x/e", 
  #MENA NOT PROJECTED OR ESTIMATED 
  NHPI =  "Native Hawaiian or Pacific Islander",
  W =  "White",
  SORA = "Some Other Race Alone",
  TOMR = "Two or More Races"
)

#Call Census Tract Level Data
wa_race <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King",
  variables = R_labels,
  year = 2023
)

#Make a copy of data for objects
king_census_trace_race <- wa_race

#Mapping Data
wa_race_zip <- get_acs(
  geography = "zcta",
  variables = R_labels,
  year = 2020,
  geometry = TRUE
)

#Template: CDC Places Data
wa_table <- wa_places %>%
  select(locationname, 
         measure, 
         data_value, 
         low_confidence_limit, 
         high_confidence_limit)
wa_table


# king_places <- placescdc_data %>%
#   filter(stateabbr == "WA",
#          countyname == "King County") %>%
#   select(
#     countyname,
#     measure,
#     data_value,
#     low_confidence_limit,
#     high_confidence_limit
#   )
# 
# king_places

king_table <- king_places %>%
  st_drop_geometry() %>%
  select(measure, data_value, low_confidence_limit, high_confidence_limit)

#Wide template: CDC Places Data
king_wide <- king_places %>%
  select(measure, data_value) %>%
  pivot_wider(names_from = measure,
              values_from = data_value)

king_wide
