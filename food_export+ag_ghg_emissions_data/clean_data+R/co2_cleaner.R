# Required packages
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

# Data source: https://www.epa.gov/ghgemissions/methodology-report-inventory-us-greenhouse-gas-emissions-and-sinks-state-1990-2022

# units for ghg: million metric tons

ghg_ag <- read.csv("../raw_data/ghg-ag-Economic-Sectors.csv")

ag_only <- filter(ghg_ag, sector == "Agriculture")
ag_only <- ag_only[ag_only$geo_ref %in% c(
  "AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA",
  "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD",
  "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH",
  "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC",
  "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY"
), ]

length(unique(ag_only$geo_ref))

ag_only_long <- pivot_longer(
  ag_only,
  cols = starts_with("Y"),
  names_to = "Year",
  values_to = "Total_Ag_Emissions"
)

ag_only_sum <- aggregate(
  Total_Ag_Emissions ~ geo_ref + Year, 
  data = ag_only_long, 
  FUN = sum
)

names(ag_only_sum) <- c("State", "Year", "Total_Ag_Emissions_In_MilsMecTon")
ag_only_sum$Year <- as.numeric(gsub("Y", "", ag_only_sum$Year))

# Convert state abbreviations to full names
state_names <- setNames(state.name, state.abb)
ag_only_sum$State <- state_names[ag_only_sum$State]

write.csv(ag_only_sum, "../final_data/state_agricultural_ghg_emissions_1990_2022_MMT_CO2equiv.csv")
