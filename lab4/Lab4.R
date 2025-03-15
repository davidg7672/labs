setwd("~/Code/school/regression-analysis/labs/lab4")
install.packages("ggplot2")
install.packages("viridis")

library(ggplot2)
library(viridis)

NBA <- read.csv("data/NBA_Data.csv")
Five38 <- read.csv("data/Five38_Data.csv")

head(NBA)
head(Five38)

NBA$win_pct = NBA$Wins/NBA$Games_Played

summary(NBA$win_pct)

NBA$average_ppg = NBA$Points/NBA$Games_Played
summary(NBA$average_ppg)
cor(NBA$average_ppg, NBA$win_pct)

Five38$Team <- paste(Five38$Place, Five38$Name, sep=" ")
BBall <- merge(NBA, Five38, by = "Team")

BBall[,1:5]
west <- BBall[BBall$Conference == "West",]
mean(BBall$Rebounds[BBall$win_pct > 0.5])
mean(BBall$Rebounds[BBall$win_pct <= 0.5])

mean(BBall$Turnovers[BBall$Conference == "East" & BBall$average_ppg >= 113])
mean(BBall$Turnovers[BBall$Conference == "East" & BBall$average_ppg < 113])

new = BBall[,c("Team", "win_pct", "average_ppg")]
BBall$Team[which.max(BBall$win_pct)]

# exercises 2

BBall[3, 9]
mean(BBall$win_pct[BBall$Conference == "West"])
mean(BBall$Free_Throw_Pct[BBall$Personal_Fouls >= 1120])
BBall$Team[which.max(BBall$Times_in_Playoffs)]

# Step 4: Create Visualizations
ggplot(data=BBall) + 
  geom_point(
    mapping = aes(x = Field_Goal_Pct, y = Making_Playoffs)
)

# Exercises
playoffs_plot <- ggplot(data = BBall) + geom_point(mapping = aes(x = win_pct, y = Times_in_Playoffs)) + labs(title = "Win Percentage vs. Times in Playoffs", x = "Win Percentage", y = "Times in Playoffs")
playoffs_plot

playoffs_plot <- ggplot(data = BBall) + geom_point(mapping = aes(x = win_pct, y = Making_Playoffs)) + labs(title = "Win Percentage vs. Probablity of Making Playoffs", x = "Win Percentage", y = "Probability of Making Playoffs")

playoffs_plot

# Aesthetics
ggplot(data = BBall) +
  geom_point(
    mapping = aes(x = Field_Goal_Pct, y = Making_Playoffs, color = Last_Year)
  )

ggplot(data = BBall) + 
  geom_point(
    mapping = aes(x = Field_Goal_Pct, y = Making_Playoffs),
    shape = 21,
    size = 4,
    color = "darkred",
    fill = "darkgoldenrod1"
  )

ggplot(data = BBall, aes(x = Games_Played, y = Wins, fill = Games_Played)) +
  geom_col() + 
  scale_fill_viridis(option= "magma")+
  theme_minimal() +
  labs(
    title = "Games Played vs Wins",
    x = "Games Played",
    y = "Wins"
  )
  
# Exercises
# 1
ggplot(data = BBall) + geom_point(
  mapping = aes(x = Field_Goal_Pct, y = Making_Playoffs, color = Last_Year)
)

# 3
ggplot(data = BBall) + 
  geom_point(
    mapping = aes(x = Field_Goal_Pct, y = Making_Playoffs, color = Last_Year, shape = Last_Year)
    
  )

# 4
ggplot(data = BBall) + 
  geom_point(
    mapping = aes(x = Field_Goal_Pct, y = Making_Playoffs, color = Last_Year, size = Last_Year)
  )

# 5
ggplot(data = BBall) +
  geom_point(
    mapping = aes(x = Field_Goal_Pct, y = Making_Playoffs),
    color = "blue",
    size = 2,
    shape = 15
  )

# Geometries

ggplot(data = BBall) +
  geom_point(
    mapping = aes(x = Field_Goal_Pct, y = Three_pt_pct)
  )


ggplot(data = BBall) + 
  geom_point(
    mapping = aes(x = Field_Goal_Pct, y = Three_pt_pct)
  )

ggplot(data = BBall) + 
  geom_smooth(
    mapping = aes(x = Field_Goal_Pct, y = Three_pt_pct, linetype = Last_Year),
  )
