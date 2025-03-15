UsArrests <- data("USArrests")

# 1973
# .62
# a = 0.05

# H0: µ = .62
# HA: µ ≠ .62
head(USArrests)

t.test(USArrests$UrbanPop, alternative = "two.sided", mu = 62, conf.level = 0.95)

# H0: µ = 0.00135
# HA: µ ≠ 0.00135

USArrests$west <- ifelse(rownames(USArrests) %in% c("California", "Washington", "Oregon", "Idaho", "Utah", "Nevada", "Arizona", "New Mexico", "Montana", "Wyoming", "Colorado"), 1, 0)
USArrests$murdery <- ifelse(USArrests$Murder > 7.25, 1, 0)

summary(USArrests$UrbanPop)




# example
# H0: µ west ≥ µ non-west
# HA: µ west < µ non-west

# H0: µ west - µ non-west ≤ 0
# H1: µ west - µ non-wets > 0

mean(USArrests$Murder[USArrests$west == 1])
sd(USArrests$Murder[USArrests$west] == 1)
length(USArrests$Murder[USArrests$west == 1])

t.test(USArrests$Murder[USArrests$west == 1], 
       USArrests$Murder[USArrests$west == 0],
       "less", conf.level = 0.95)

# Exercise
# Part 1
mean(USArrests$Assault)
t.test(USArrests$Assault, alternative = "two.sided", mu = mean(USArrests$Assault), conf.level = 0.99)

# Part 2
median_urban_pop <- median(USArrests$UrbanPop)
USArrests$high_urban_pop <- ifelse(USArrests$UrbanPop > median_urban_pop, 1, 0)
USArrests$low_urban_pop <- ifelse(USArrests$UrbanPop <= median_urban_pop, 1, 0)
t.test(USArrests$Murder[USArrests$high_urban_pop == 1],
       USArrests$Murder[USArrests$high_urban_pop == 0],
       alternative = "greater", conf.level = 0.95)

