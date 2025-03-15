# David Sosa
# Professor Carlston
# ECON 355 - Regression Analysis
# Lab 3

# setting directory and reading data
setwd("~/Code/school/regression-analysis/labs/lab3")
election <- read.csv("data/County_Election.csv")

# Assignment
head(election, n = 1)
summary(election)
head(election)

# statistic 1
max_poverty <- max(election$poor_share2010)
election$county_name[election$poor_share2010 == max_poverty]

# statistic 2
min_poverty <- min(election$poor_share2010)
low_poverty_county <- election$state_name[election$poor_share2010 == min_poverty]

# statistic 3
# most education
max_education <- max(election$frac_coll_plus2010)
highest_education <- election$county_name[election$frac_coll_plus2010 == max_education]
republican_of_highest <- election$pct_republican_2016[election$county_name == highest_education]

# statistics for republican voters
mean(election$pct_republican_2016)
sd(election$pct_republican_2016)
quantile(election$pct_republican_2016)

plot(
  election$pct_republican_2016,
  election$poor_share2010,
  main = "Republican Voters by Poverty",
  ylab = "Percentage of Poverty",
  xlab = "Percentage Republican Voters",
  cex = .5
)

