
nycflights13 <- read.csv("Data/nycflights13.csv")
library(tidyverse)
library(dplyr)
library(viridis)

colnames(nycflights13)
colnames(nycflights13)
f1 <- nycflights13 |>
  group_by(carrier) |>
  summarise(proportionDelay = mean(dep_delay >= 15 | arr_delay >= 15, na.rm = TRUE),
            n = n(), nOfDelayed = round(proportionDelay*n))
f1
x <- f1 |> group_by(carrier) |> arrange(desc(proportionDelay))
x

x$carrier <- factor(x$carrier, levels = x$carrier)
ggplot(x, aes(x = carrier, y= proportionDelay, label = n))+ geom_col(aes(fill = n))  + coord_flip()+
  geom_text(
    aes(label = n),
    hjust = 0, col="red") + ylim(0,0.8) + ggtitle("Proportion delay by carrier with number of counts associated") +
  scale_fill_viridis_b()


ggplot(x, aes(x = carrier, y= proportionDelay, label = n))+ geom_col(aes(fill = n))  + coord_flip()+
  geom_text(
    aes(label = n),
    hjust = 0, col="red") + ylim(0,0.8) + ggtitle("Proportion delay by carrier with number of counts associated") +
  scale_fill_viridis_b()



flights2 <- nycflights13 |> filter(!is.na(dep_delay), !is.na(arr_delay)) |>
  mutate(delayed = as.numeric(dep_delay >= 15 | arr_delay >= 15), is_arr_delayed = as.numeric(arr_delay >= 15))
flights2 <- flights2 |> mutate(combinesMinutes = hour*60+minute)
max(flights2$dep_delay)

mod1 <- glm(delayed ~ dep_delay + carrier + origin + dest + distance + combinesMinutes,
            data = flights2,family = binomial)
summary(mod1)
# models that are important and have a huge diff F9, FL as we are doing e^
## next is to make a model adding some variables and see if they are needed

mod1Reduced <- glm(delayed ~ dep_delay + carrier + origin + combinesMinutes + dest,
                   data = flights2, family = binomial)
summary(mod1Reduced)

anova(mod1Reduced, mod1, test = "Chisq")
# p-val - 0.6781, model tells us that the reduced model is not siginificantly worse, can be excluded

# since dep_delay is the most significant predictor with a z value of 225 we make a plot
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

# departure delay logistic regression curve

#fit = glm(delayed ~ dep_delay, data=flights2, family=binomial)
#newdat <- data.frame(dep_delay=seq(min(flights2$dep_delay), max(flights2$dep_delay),len=100))
#newdat$delayed = predict(fit, newdata=newdat, type="response")
#plot(delayed ~ dep_delay, data=flights2, col="red4")
#lines(delayed ~ dep_delay, newdat, col="green4", lwd=2)
# key plot as we figure out as dep_delay increases so does the probability of being late



ggplot(flights2, aes((dep_delay), is_arr_delayed)) +
  geom_jitter(height = 0.05, alpha = 0.025) +
  binomial_smooth() +
  facet_wrap(~carrier) +
  ggtitle("Probability of arrival delay vs departure delay by carrier") +
  xlab("Departure Delay") +
  ylab("Probability of being arrival delay") +
  coord_cartesian(xlim = c(0, 300)) # needed so geom smooth uses full data
# large departure delays perfectly predict arrival delay so warnings


f2 <- (flights2) %>%
  mutate(
    Status = ifelse(dep_delay >= 15 | arr_delay >= 15, "Delayed", "On Time")
  )

nycflights13$arr_delay
ggplot(f2, aes(x=origin, fill=Status))+
  geom_bar()

newlabel <- f2 %>%
  group_by(carrier) %>%
  summarise(proportionDelay = mean(Status == "Delayed", na.rm = TRUE),
    n = n()
  )

ggplot((f2), aes(x = carrier, fill = Status)) +
  geom_bar() +
  coord_flip() +
  geom_text(
    data = newlabel,
    aes(x = carrier, y = n,
      label = scales::percent(proportionDelay),
      color = "Proportion Delayed"),
    inherit.aes = FALSE,
    hjust = -0.15,
    size = 4
  ) +
  scale_color_manual(
    name="Percentage",
    values = c("Proportion Delayed" = "black")) +
  ggtitle("Number of flights by carrier with delayed proportion") +
  xlab("Carrier") +
  ylab("Count") + ylim(0,62000)
