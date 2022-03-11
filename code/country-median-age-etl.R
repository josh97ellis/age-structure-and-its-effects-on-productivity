library(tidyverse)
library(readxl)

import_path = 'C:\\Users\\Josh Ellis\\OneDrive - University of Nebraska at Omaha\\COURSES\\SPRING_2022\\STAT8416_INTRO_TO_DATA_SCIENCE\\STAT8416 Course Project\\DATASETS\\RAW_DATA\\MEDIAN_AGE_OF_POPULATION\\MEDIAN_AGE_OF_POPULATION.xlsx'
df = read_excel(import_path, skip=16)


# Clean and melt data from source
df  <- df %>%
  filter(Type == 'Country/Area') %>%
  select(-c('Index', 'Notes', 'Variant', 'Parent code', 'Type')) %>%
  mutate_at(vars(3:17), funs(as.numeric)) %>%
  pivot_longer(
    cols = 3:17,
    names_to = 'Year',
    values_to = 'MedianAge') %>%
  rename(
    Country = `Region, subregion, country or area *`,
    CountryCode = `Country code`) %>%
  arrange(Country) %>%
  group_by(Country) %>% 
  mutate(
    MedianAgeChange = c(diff(MedianAge), 0),
    CountryYearGroup = row_number())


# a bit of a Hackish way for Adding data in the gaps in years
df <- data.frame(lapply(df, rep, 5))

df <- df %>%
  arrange(Country, Year) %>%
  group_by(Country, Year) %>%
  mutate(
    RowSequence = row_number()-1,
    MedianAgeChange = case_when(
      RowSequence != 0 ~ MedianAgeChange / 5,
      TRUE ~ 0)
    )%>%
  group_by(CountryYearGroup) %>%
  mutate(
    CumulativeMedianAgeChange = cumsum(MedianAgeChange),
    MedianAge = MedianAge + CumulativeMedianAgeChange,
    Year = as.numeric(Year) + RowSequence
    ) %>%
  ungroup() %>%
  select(-c('MedianAgeChange', 'CountryYearGroup', 'RowSequence',
            'CumulativeMedianAgeChange'))
  

write_csv(df, file = 'C:\\Users\\Josh Ellis\\OneDrive - University of Nebraska at Omaha\\COURSES\\SPRING_2022\\STAT8416_INTRO_TO_DATA_SCIENCE\\STAT8416 Course Project\\DATASETS\\PROCESSED_DATA\\POPULATION_MEDIAN_AGE_CLEANED.csv')
