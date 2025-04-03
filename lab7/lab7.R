# Lab 7, March 27th, 2025
library(dplyr)
library(ggplot2)
library(janitor)
library(readxl)
setwd("/Users/davidsosa/Code/school/regression-analysis/labs/lab7")

life_exp <- read_excel("data/life_expectancy_insurance_example_data.xlsx", sheet="Life Expectancy")
head(life_exp)

life_exp <- life_exp |> 
  filter(Year == 2010) |> 
  clean_names() |> 
  select(state, county, fips, female_le, male_le)


income <- read_excel("data/life_expectancy_insurance_example_data.xlsx", sheet="Income")
income$fips <- income$STATEA*1000 + income$COUNTYA
income <- income |>
  clean_names() |>
  select(fips, median_hh_income)

life_exp <- merge(life_exp, income, by = "fips")

# insurance part
insurance <- read_excel("data/life_expectancy_insurance_example_data.xlsx",sheet ="Insurance")

insurance$FIPS = 1000*insurance$STATEA + insurance$COUNTYA

uninsured <- insurance |>
  clean_names() |>
  group_by(fips) |>
  summarize(uninsured_rate = (male_under65_uninsured + female_under65_uninsured)/(male_under65_total + female_under65_total)) 

life_exp <- merge(life_exp, uninsured, by = "fips")

region <- read_excel("data/life_expectancy_insurance_example_data.xlsx",sheet ="Region")
life_exp <- merge(life_exp, region, by.x = "state", by.y = "STATE")
rm(income, insurance, region, uninsured)

cor(life_exp$uninsured_rate, life_exp$female_le)
ggplot(data = life_exp) +
  geom_point(aes(x = uninsured_rate, y = female_le)) +
  xlab("Uninsured Rate") +
  ylab("Female Life Expectancy") +
  theme_classic()

summary(lm(female_le ~ uninsured_rate, data = life_exp))
summary(life_exp$uninsured_rate)
summary(lm(female_le ~ uninsured_rate + median_hh_income, data = life_exp))
summary(lm(female_le ~ uninsured_rate + median_hh_income + Region, data = life_exp))

spending <- read_excel("data/homelessness_data.xlsx", "Federal Aid")
spending$state_code <- substr(spending$coc_number, 1, 2)
state_spending <- spending |>
  group_by(state_code) |>
  summarize(funding = sum(amount))
