nycflights13 <- read.csv("Data/nycflights13.csv")
library(tidyverse)
library(dplyr)
f1 <- nycflights13 |>
  group_by(carrier) |>
  summarise(proportionDelay = mean(dep_delay > 0 | arr_delay > 0, na.rm = TRUE),
    n = n())
f1
x <- f1 |> group_by(carrier) |> arrange(desc(proportionDelay))
x

x$carrier <- factor(x$carrier, levels = x$carrier)
ggplot(x, aes(x = carrier, y= proportionDelay))+ geom_col(fill="black")


flights2 <- nycflights13 |> filter(!is.na(dep_delay), !is.na(arr_delay)) |>
  mutate(delayed = arr_delay > 5)
flights2$delayed <- as.numeric(flights2$delayed)
flights2 <- flights2 |> mutate(combinesMinutes = hour*60+minute)

mod1 <- glm(delayed ~ dep_delay + carrier + origin + dest + distance + combinesMinutes,
            data = flights2,family = binomial)
summary(mod1)
# models that are important and have a huge diff F9, FL as we are doing e^
## next is to make a model adding some variables and see if they are needed

mod1Reduced <- glm(delayed ~ dep_delay + carrier + origin + combinesMinutes + dest,
                   data = flights2, family = binomial)
summary(mod1Reduced)

anova(mod1, mod1Reduced)
# p-val - 0.6781, model tells us that the reduced model is not siginificantly worse, can be excluded

# since dep_delay is the most significant predictor with a z value of 225 we make a plot
ggplot(flights2, aes(x = dep_delay, y = delayed)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "red") +
    ggtitle("Delay vs Departure Delay") +
    xlab("Departure Delay") +
    ylab("prob of being Delayed")


ggplot(flights2, aes(x = combinesMinutes, y = delayed)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "red") +
  ggtitle("Delay vs flight time")

