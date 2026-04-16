nycflights13 <- read.csv("Data/nycflights13.csv")
library(tidyverse)
library(dplyr)
library(viridis)
f1 <- nycflights13 |>
  group_by(carrier) |>
  summarise(proportionDelay = mean(dep_delay > 0 | arr_delay > 0, na.rm = TRUE),
            n = n())
f1
x <- f1 |> group_by(carrier) |> arrange(desc(proportionDelay))
x

x$carrier <- factor(x$carrier, levels = x$carrier,)
ggplot(x, aes(x = carrier, y= proportionDelay, label = n))+ geom_col(aes(fill = n))  + coord_flip()+
  geom_text(
    aes(label = n),
    hjust = 0, col="red") + ylim(0,0.8) + ggtitle("Proportion delay by carrier with number of counts associated") +
  scale_fill_viridis_b()


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
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}
ggplot(flights2, aes(x = dep_delay, y = delayed)) +
  geom_jitter(alpha = 0.05) +
  binomial_smooth() +
  ggtitle("Delay vs Departure Delay") +
  xlab("Departure Delay") +
  ylab("prob of being Delayed")

# departure delay logistic regression curve

#fit = glm(delayed ~ dep_delay, data=flights2, family=binomial)
#newdat <- data.frame(dep_delay=seq(min(flights2$dep_delay), max(flights2$dep_delay),len=100))
#newdat$delayed = predict(fit, newdata=newdat, type="response")
#plot(delayed ~ dep_delay, data=flights2, col="red4")
#lines(delayed ~ dep_delay, newdat, col="green4", lwd=2)
# key plot as we figure out as dep_delay increases so does the probability of being late


ggplot(flights2, aes(dep_delay, delayed)) +
  geom_jitter(height = 0.05, alpha = 0.01) +
  binomial_smooth() +
  facet_wrap(~carrier, scales = "free_y") +
  ggtitle("Probability of delay vs departure delay by each Carrier") +
  xlab("Departure Delay") +
  ylab("Probability of being delayed")


