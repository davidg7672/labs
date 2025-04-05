library(dplyr)
library(ggplot2)

setwd("/Users/davidsosa/Code/school/regression-analysis/labs/regression")
cps <- readxl::read_excel("Multiple Regression Example Data.xlsx", sheet = "CPS")
pwl <- readxl::read_excel("Multiple Regression Example Data.xlsx", sheet = "PWL")
cpi <- readxl::read_excel("Multiple Regression Example Data.xlsx", sheet = "CPI")

cps <- merge(cps, pwl, by.x = c("year", "statefip"), by.y = c("year", "fips"))
cps <- merge(cps, cpi, by.x = "year", by.y = "Year")
rm(cpi, pwl)

# turn nominal income into real income
cps <- cps |>
  mutate(real_income = incwage*CPI/0.652)

# check I made it correct
graph_data <- cps |>
  group_by(year) |>
  summarize(real_income=median(real_income),
            nominal_income = median(incwage))

ggplot(graph_data) + 
  geom_line(aes(x = year, y = nominal_income)) +
  geom_line(aes(x = year, y = real_income), color = "blue") +
  theme_classic()

# filter to just blue collar jobs and create new variables

cps_bc <- cps |>
  filter(occ50ly > 499 & occ50ly < 700) |>
  mutate(construction = ifelse(ind50ly == 246, 1, 0),
         race_new = ifelse(race == 100, "1 - White",
                           ifelse(race == 200, "2 - Black",
                                  ifelse(race == 300, "3 - Native America",
                                         "4. - Other")))
         )

sum(cps_bc$construction/length(cps_bc$construction))

pwd_reg <- lm(real_income ~ pw, cps_bc)
summary(pwd_reg)

# multiple
pw_reg2 <- lm(real_income ~ pw + construction + pw*construction + age +
                as.factor(sex) + race_new + state + as.factor(year), data =cps_bc)
summary(pw_reg2)
