# Lab 7, March 27th, 2025
library(dplyr)
library(ggplot2)
library(janitor)
setwd("/Users/davidsosa/Code/school/regression-analysis/labs/lab7")

life_exp <- readxl::read_xlsx("data/life_expectancy_insurance_example_data.xlsx", sheet=1)
head(life_exp)

life_exp <- life_exp |> 
  filter(Year == 2010) |> 
  clean_names() |> 
  select(state, county, fips, female_le, male_le)

income <- readxl::read_xlsx("data/life_expectancy_insurance_example_data.xlsx", sheet=2)
income$fips <- income$STATEA*1000 + income$COUNTYA
income <- income |>
  clean_names() |>
  select(fips, median_hh_income)

life_exp <- merge(life_exp, income, by = "fips")
insurance <- readxl::read_xlsx("data/life_expectancy_insurance_example_data.xlsx",sheet =3)