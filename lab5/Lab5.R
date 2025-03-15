setwd("/Users/davidsosa/Code/school/regression-analysis/labs/lab5")

rm(list = ls())
library(tidyverse)
library(ggplot2)
pop <- read.csv("data/population_by_country.csv")
gdp <- read.csv("data/gdp_by_country.csv") 

pop <- pop[pop$Year > 1989, ]
pop <- filter(pop, Year > 1989)

pop <- pop |>
  filter(Year > 1989)

gdp <- gdp |>
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    names_prefix = "X",
    values_to = "GDP_per_cap",
    values_drop_na = TRUE
  )

gdp <- gdp |>
  mutate(Year = as.numeric(Year))

result <- gdp[gdp$Year == 2023,]
result <- pop[pop$Year == 2023,]

world <- merge(gdp, pop, by.x = c("Year", "Country.Code"), by.y = c("Year", "iso3_code"))


year_data <- world |>
  group_by(Year) |>
  summarize(n_countries = n(),
            avg_le = mean(as.numeric(life_exp_total)))

year_data[year_data$Year > 2019,]

ggplot(data = year_data) + 
  geom_line(aes(x = Year, y = avg_le)) +
  xlab("Year") +
  ylab("Average Life Exp") + 
  theme_classic() + 
  labs(alt = "A line graph with year on the x axis and life expectancy on the y axis. Years range from 1990 to 2023, and the average life expecatancy increases from about 65 to about 73, with a dip around 2020.") # It's a good idea to add alternative text for visually impaired users!


# exercises
world <- world |>
  mutate(life_exp_total = as.numeric(life_exp_total))

pop <- pop |> 
  mutate(total_pop = as.numeric(total_pop))

world <- merge(gdp, pop, by.x = c("Year", "Country.Code"), by.y = c("Year", "iso3_code"))

world_2021 <- world |> filter(Year == 2021)
world_2023 <- world |> filter(Year == 2023)

continent_summary <- world_2021 |> 
  group_by(Continent) |> 
  summarize(
    avg_life_exp = mean(as.numeric(life_exp_total), na.rm = TRUE),
    avg_gdp_per_cap = mean(GDP_per_cap, na.rm = TRUE),
    total_population = sum(total_pop, na.rm = TRUE)
  )

world_2021 <- world_2021 |> mutate(log_gdp = log(GDP_per_cap))


ggplot(data = world_2021, aes(x = log_gdp, y = as.numeric(life_exp_total), color = Continent, size = total_pop)) + 
  geom_point(alpha = 0.7) +
  xlab("Log of GDP per Capita") +
  ylab("Life Expectancy") +
  theme_minimal() +
  labs(size = "Total Population", color = "Continent") +
  ggtitle("Life Expectancy vs. Log GDP per Capita (2021)")

fertility_life_correlation <- cor(as.numeric(world_2021$fertility_rate), as.numeric(world_2021$life_exp_total), use="complete.obs")
print(paste("Correlation between fertility rate and life expectancy: ", round(fertility_life_correlation, 3)))

gdp_correlation <- cor(as.numeric(world_2021$med_age), as.numeric(world_2021$log_gdp))

ggplot(data = world_2021, aes(x = log_gdp, y = as.numeric(med_age), color = Continent, size = total_pop)) +
  geom_point() +
  xlab("Logged GDP") +
  ylab("Median Age") +
  theme_minimal() +
  ggtitle("Median Age vs vs Logged GDP Per Capita")

mean(as.numeric(world_2021$life_exp_total[world_2021$Continent == "Africa"]))
sum()