nycflights13 <- read_csv("Data/nycflights13.csv")
library(tidyverse)
library(dplyr)
f1 <- nycflights13 |>
  group_by(carrier) |>
  summarise(proportionDelay = mean(dep_delay > 0 | arr_delay > 0, na.rm = TRUE),
    n = n())
f1
x <- f1 |> group_by(carrier)
x |> group_by(carrier) |> arrange(desc(proportionDelay))
x

flights2 <- nycflights13 |>
  mutate(delayed = dep_delay > 0 | arr_delay > 0)
flights2$delayed <- complete.cases(flights2$delayed)

mod1 <- glm(delayed ~ carrier, data = flights2,family = binomial)
summary(model1)
# models that are important and have a huge diff F9, FL as we are doing e^
## next is to make a model adding some variables and see if they are needed
