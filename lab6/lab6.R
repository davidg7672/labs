library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyquant)

stocks <- tq_get(c("AAPL", "DIS", "^IXIC"),
                 get = "stock.prices",
                 from = "2019-01-01",
                 to = "2024-01-01")

stocks <- stocks |> 
  select(symbol, date, open) |> 
  pivot_wider(names_from = symbol,
              values_from = open) |> 
  rename(nasdaq =`^IXIC`)

stocks <- stocks |> 
  mutate(apple_return = (AAPL/lag(AAPL)-1)*100,
         disney_return = (DIS/lag(DIS)-1)*100,
         nasdaq_return = (nasdaq/lag(nasdaq)-1)*100)

summary(stocks$disney_return)
summary(stocks$apple_return)
summary(stocks$nasdaq_return)

mean(stocks$disney_return, na.rm = TRUE)
mean(stocks$apple_return, na.rm = TRUE)
mean(stocks$nasdaq_return, na.rm = TRUE)

stocks$date[which.min(stocks$nasdaq_return)]
(tail(stocks$nasdaq, 1)/head(stocks$nasdaq, 1)-1)* 100
(tail(stocks$DIS, 1)/head(stocks$DIS, 1)-1)* 100
(tail(stocks$AAPL, 1)/head(stocks$AAPL, 1)-1)* 100

ggplot(stocks, aes(x = nasdaq_return, y = apple_return)) +
  geom_point() + 
  xlab("NASDAQ Return") + 
  ylab("Apple Return") +
  theme_minimal()


ggplot(stocks, aes(x = nasdaq_return, y = disney_return)) +
  geom_point() + 
  xlab("NASDAQ Return") + 
  ylab("Disney Return") +
  theme_minimal()

apple_model <- lm(apple_return ~ nasdaq_return, data = stocks)
summary(apple_model)
# a = 0.0627
# b = 1.0912

disney_model <- lm(disney_return ~ nasdaq_return, data = stocks)
summary(disney_model)
# a = -0.06
# b = 0.867



stat_smooth(mapping = aes(x = nasdaq_return, y = apple_return), data = stocks,
            method = "lm", geom = "smooth")

ggplot(stocks, aes(x = nasdaq_return, y = apple_return)) +
  geom_point() + 
  stat_smooth(mapping = aes(x = nasdaq_return, y = apple_return), data = stocks,
              method = "lm", geom = "smooth")
  xlab("NASDAQ Return") + 
  ylab("Apple Return") +
  theme_minimal()


ggplot(stocks, aes(x = nasdaq_return, y = disney_return)) +
  geom_point() + 
  xlab("NASDAQ Return") + 
  ylab("Disney Return") +
  theme_minimal()