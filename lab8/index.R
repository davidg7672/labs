#### Lab 8: Alcohol Deaths and Fixed Effects ####
setwd("/Users/davidsosa/Code/school/regression-analysis/labs/lab8")
rm(list = ls())
library(readxl)
library(tidyverse)
library(ggplot2)
library(stargazer)
library(corrplot)
library(janitor)

deaths = read_excel("data/alcohol_deaths.xlsx", sheet = "Alcohol-Related Deaths")

deaths <- deaths |>
  pivot_longer(
    cols = starts_with("Crude_Rate_"),
    names_to = "year",
    names_prefix = "Crude_Rate_",
    values_to = "death_rate"
  ) |>
  select(State, `State Code`, year, death_rate) |>
  rename(state_fip = `State Code`)

unemployment <- read_excel("data/alcohol_deaths.xlsx", sheet = "Unemployment", skip = 3)
unemployment <- unemployment |>
  clean_names() |>
  pivot_longer(
    cols = -series_id,
    names_to = "year",
    values_to = "unemployment_rate"
  )

