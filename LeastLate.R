nycflights13 <- read_csv("Data/nycflights13.csv")
library(tidyverse)
f1 <- flights |>
  group_by(carrier) |>
  mutate(proportionDelay = mean(dep_delay > 0 | arr_delay > 0, na.rm=TRUE)) |>
x <- f1 |> group_by(carrier) |> count(proportionDelay)
x |> group_by(carrier) |> arrange(desc(proportionDelay))
print(x)
unique(airlines$carrier)
