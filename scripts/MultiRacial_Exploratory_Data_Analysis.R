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

wa_race <- get_acs(
  geography = "county",
  state = "WA",
  variables = R_labels,
  year = 2023
)


#racialidentity_combo <-
  
#racialidentity_mutexcl <- 