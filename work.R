library(dplyr)
library(lme4)
library(nycflights13)
library(ggplot2)
library(future)

planes <- nycflights13::planes
flights <- nycflights13::flights

## getting all planes that are both in flights and planes datasets
merged_df <- inner_join(planes, flights, by = "tailnum")
merged_df$is_late <- merged_df$arr_delay > 0
kept_features <- c("is_late", "tailnum", "carrier", "dest", "distance",
                   "sched_dep_time", "sched_arr_time", "day", "month", "year.x", "engine",
                   "manufacturer", "type")

## getting rid of flights that have NAs and columns we don't need
planes_flights <- merged_df[, c(1, 18)]
planes_flights <- planes_flights[complete.cases(planes_flights), ]
merged_df <- merged_df[, kept_features]
merged_df <- merged_df[complete.cases(merged_df), ]

## average delay times for each unique plane
average_delay <- planes_flights |> group_by(tailnum) |>
  summarise(average_arrival_delay = mean(arr_delay, na.rm = TRUE))

## testing whether each plane is likely to be late
planes_flights <- planes_flights[1:(0.5*nrow(planes_flights)), ]
delays <- lm(arr_delay ~ 0 + tailnum, data = planes_flights)
delay_summary <- summary(delays)

## use 4 cores to speed up process
plan(multisession, workers = 4)

merged_df <- merged_df[1:(0.5*nrow(merged_df)), ]
fit1 <- glm(is_late ~ ., family = "binomial", data = merged_df)
summary_fit1 <- summary(fit1)

