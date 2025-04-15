#### Lab 8: Alcohol Deaths and Fixed Effects ####
setwd("/Users/davidsosa/Code/school/regression-analysis/labs/lab8")
rm(list = ls())
library(readxl)
library(tidyverse)
library(ggplot2)
library(stargazer)
library(corrplot)
library(janitor)

deaths <- read_excel("data/alcohol_deaths.xlsx", sheet = "Alcohol-Related Deaths")
unemployment <- read_excel("data/alcohol_deaths.xlsx", sheet = "Unemployment", skip = 3)
marry <- read_excel("data/alcohol_deaths.xlsx", sheet = "Marital Status")
relig <- read_excel("data/alcohol_deaths.xlsx", sheet = "Religiosity")

deaths <- deaths |>
  pivot_longer(
    cols = starts_with("Crude_Rate_"),
    names_to = "year",
    names_prefix = "Crude_Rate_",
    values_to = "death_rate"
  ) |>
  select(State, `State Code`, year, death_rate) |>
  rename(state_fip = `State Code`)

unemployment <- unemployment |>
  clean_names() |>
  pivot_longer(
    cols = -series_id,
    names_to = "year",
    values_to = "unemployment_rate"
  )

unemployment <- unemployment |>
  mutate(year = as.numeric(str_remove(year, "annual_")))

state_fip <- as.numeric(str_sub(unemployment$series_id, 6, 7))
unemployment$state_fip <- state_fip

marry <- marry |>
  rowwise() |>
  mutate(year = str_sub(YEAR, -4, -1),
         pct_married = sum(Male_Married, Female_Married) / 
           sum(across(c(starts_with("Male_"), starts_with("Female_")))),
         pct_divorced = sum(Male_Divorced, Female_Divorced) /
           sum(across(c(starts_with("Male_"), starts_with("Female_"))))) |>
  ungroup() |>
  select(STATE, year, pct_married, pct_divorced)

deaths <- merge(deaths, unemployment, by = c("state_fip", "year"))
deaths <- merge(deaths, marry, by.x = c("State", "year"), by.y = c("STATE", "year"))
deaths <- merge(deaths, relig, by.x = "State", by.y = "state")
deaths[c(4,5)] <- deaths[c(5, 4)]
names(deaths)[c(4, 5)] <- names(deaths)[c(5, 4)]

corr_table <- cor(deaths[,5:10])
corrplot(corr_table, type = "upper")

# 80th
deaths |> 
  filter(year == "2021") |> # Only use data from 2021
  mutate(death_quartile = percent_rank(death_rate)) |> # Calculate percentile ranks
  filter(death_quartile > 0.80) |> # Filter to top 20 percentile death rates
  ggplot(aes(x = reorder(State, -death_rate), y = death_rate)) + # Plot!
  geom_col() +
  xlab("State") +
  ylab("Alcohol-Related Death Rate") +
  theme_classic() 

# 20th percentile
deaths |> 
  filter(year == "2021") |> # Only use data from 2021
  mutate(death_quartile = percent_rank(death_rate)) |> # Calculate percentile ranks
  filter(death_quartile < 0.80) |> # Filter to top 20 percentile death rates
  ggplot(aes(x = reorder(State, -death_rate), y = death_rate)) + # Plot!
  geom_col() +
  xlab("State") +
  ylab("Alcohol-Related Death Rate") +
  theme_classic() 

ggplot(deaths, aes(x = unemployment_rate, y = death_rate)) +
  geom_point() +
  xlab("Unemployment Rate") +
  ylab("Alcohol-Related Death Rate") +
  theme_classic()

ggplot(deaths, aes(x = pct_highly_religious, y = death_rate)) +
  geom_point() +
  xlab("Percent Highly Religious") +
  ylab("Alcohol-Related Death Rate") +
  theme_classic()


reg1 <- lm(death_rate ~ unemployment_rate + pct_highly_religious + pct_divorced, data = deaths)
summary(reg1)

reg2 <- lm(death_rate ~ unemployment_rate + pct_highly_religious + pct_divorced + year, data = deaths)
summary(reg2)

reg3 <- lm(death_rate ~ unemployment_rate + pct_highly_religious + pct_divorced + year + State, data = deaths)
summary(reg3)


