##### CODE INTRODUCTION #####
'
This code is created to 
EXTRACT: population age structure data from the World Data Bank via the WDI package
TRANSFORM: Data into proper shape for analysis
LOAD: Store resulting data into a GitHub Repository for each of access between group members
'

##### EXTRACTION FROM WORLD DATA BANK #####


# Load Packages
library(WDI)
library(tidyverse)


# Import Countries Data (Obtains ISO 2 Code for Country)
countries = read_csv('https://pkgstore.datahub.io/core/country-list/data_csv/data/d7c9d7cfb42cb69f4422dec222dbbaa8/data_csv.csv')


# Import Data from WDI Database
data <- WDI(
  indicator = c(
    'SP.POP.TOTL', 
    'SP.POP.TOTL.FE.ZS', 'SP.POP.TOTL.MA.ZS',
    'SP.POP.0004.FE.5Y', 'SP.POP.0004.MA.5Y',
    'SP.POP.0509.FE.5Y', 'SP.POP.0509.MA.5Y',
    'SP.POP.1014.FE.5Y', 'SP.POP.1014.MA.5Y',
    'SP.POP.1519.FE.5Y', 'SP.POP.1519.MA.5Y',
    'SP.POP.2024.FE.5Y', 'SP.POP.2024.MA.5Y',
    'SP.POP.2529.FE.5Y', 'SP.POP.2529.MA.5Y',
    'SP.POP.3034.FE.5Y', 'SP.POP.3034.MA.5Y',
    'SP.POP.3539.FE.5Y', 'SP.POP.3539.MA.5Y',
    'SP.POP.4044.FE.5Y', 'SP.POP.4044.MA.5Y',
    'SP.POP.4549.FE.5Y', 'SP.POP.4549.MA.5Y',
    'SP.POP.5054.FE.5Y', 'SP.POP.5054.MA.5Y',
    'SP.POP.5559.FE.5Y', 'SP.POP.5559.MA.5Y',
    'SP.POP.6064.FE.5Y', 'SP.POP.6064.MA.5Y',
    'SP.POP.6569.FE.5Y', 'SP.POP.6569.MA.5Y',
    'SP.POP.7074.FE.5Y', 'SP.POP.7074.MA.5Y',
    'SP.POP.7579.FE.5Y', 'SP.POP.7579.MA.5Y',
    'SP.POP.80UP.FE.5Y', 'SP.POP.80UP.MA.5Y'
  ),
  country = countries$Code
)


##### DATA TRANSFORMATIONS #####
data_trsfrm = data

# Convert Percentages to Proportions
data_trsfrm[5:40] = sapply(data_trsfrm[5:40], '/', 100)

# Convert the total female population proportions to population value
data_trsfrm$SP.POP.TOTL.FE.ZS <- (
  data_trsfrm$SP.POP.TOTL.FE.ZS
  * data_trsfrm$SP.POP.TOTL
  )

# Convert the total male population proportions to population value
data_trsfrm$SP.POP.TOTL.MA.ZS <- (
  data_trsfrm$SP.POP.TOTL.MA.ZS
  * data_trsfrm$SP.POP.TOTL
  )

# Takes Each FEMALE age structure proportions Fields and multiplies it by the total female population 
data_trsfrm[grepl('.FE.5Y', colnames(data_trsfrm))] = (
  sapply(
    data_trsfrm[grepl('.FE.5Y', colnames(data_trsfrm))],
    '*',
    data_trsfrm$SP.POP.TOTL.FE.ZS
    )
)

# Takes Each MALE age structure proportions Fields and multiplies it by the total female population 
data_trsfrm[grepl('.MA.5Y', colnames(data_trsfrm))] = 
  sapply(
    data_trsfrm[grepl('.MA.5Y', colnames(data_trsfrm))],
    '*',
    data_trsfrm$SP.POP.TOTL.MA.ZS
    )


# Removes and Renames Fields, Melts the data, and separates variable by Delimiter
data_trsfrm <- data_trsfrm %>%
  select(
    -c('SP.POP.TOTL', 'SP.POP.TOTL.FE.ZS', 'SP.POP.TOTL.MA.ZS')
    ) %>%
  rename(
    CountryISO2Code = `iso2c`,
    CountryName = `country`,
    Year = `year`,
    `Female|0-4` = SP.POP.0004.FE.5Y,
    `Male|0-4` = SP.POP.0004.MA.5Y,
    `Female|5-9` = SP.POP.0509.FE.5Y,
    `Male|5-9` = SP.POP.0509.MA.5Y,
    `Female|10-14` = SP.POP.1014.FE.5Y,
    `Male|10-14` = SP.POP.1014.MA.5Y,
    `Female|15-19` = SP.POP.1519.FE.5Y,
    `Male|15-19` = SP.POP.1519.MA.5Y,
    `Female|20-24` = SP.POP.2024.FE.5Y,
    `Male|20-24` = SP.POP.2024.MA.5Y,
    `Female|25-29` = SP.POP.2529.FE.5Y,
    `Male|25-29` = SP.POP.2529.MA.5Y,
    `Female|30-34` = SP.POP.3034.FE.5Y,
    `Male|30-34` = SP.POP.3034.MA.5Y,
    `Female|35-39` = SP.POP.3539.FE.5Y,
    `Male|35-39` = SP.POP.3539.MA.5Y,
    `Female|40-44` = SP.POP.4044.FE.5Y,
    `Male|40-44` = SP.POP.4044.MA.5Y,
    `Female|45-49` = SP.POP.4549.FE.5Y,
    `Male|45-49` = SP.POP.4549.MA.5Y,
    `Female|50-54` = SP.POP.5054.FE.5Y,
    `Male|50-54` = SP.POP.5054.MA.5Y,
    `Female|55-59` = SP.POP.5559.FE.5Y,
    `Male|55-59` = SP.POP.5559.MA.5Y,
    `Female|60-64` = SP.POP.6064.FE.5Y,
    `Male|60-64` = SP.POP.6064.MA.5Y,
    `Female|65-69` = SP.POP.6569.FE.5Y,
    `Male|65-69` = SP.POP.6569.MA.5Y,
    `Female|70-74` = SP.POP.7074.FE.5Y,
    `Male|70-74` = SP.POP.7074.MA.5Y,
    `Female|75-79` = SP.POP.7579.FE.5Y,
    `Male|75-79` = SP.POP.7579.MA.5Y,
    `Female|80+` = SP.POP.80UP.FE.5Y,
    `Male|80+` = SP.POP.80UP.MA.5Y,
  ) %>%
  pivot_longer(
    cols = 4:37,
    names_to = 'GenderAgeGroup',
    values_to = 'Population'
    ) %>%
  separate(
    col = GenderAgeGroup,
    into = c('Gender', 'Age'),
    sep = "\\|"
    )

# Find Countries that contain missing Population data (we want to remove these)
na_countries <- data_trsfrm %>%
  filter(is.na(Population)) %>%
  group_by(CountryName) %>%
  summarise(
    n = n(),
    Pct_of_Total = n/2074
    )

# Remove counties with missing population data from the data
data_trsfrm <- anti_join(data_trsfrm, na_countries, by="CountryName")


##### EXTRACT TO FOLDER #####
write_csv(
  data_trsfrm,
  file = 'C:\\Users\\Josh Ellis\\OneDrive - University of Nebraska at Omaha\\COURSES\\SPRING_2022\\STAT8416_INTRO_TO_DATA_SCIENCE\\STAT8416 Course Project\\DATASETS\\POPULATION_AGE_STRUCTURE\\POPULATION_AGE_STRUCTURE.csv'
)
