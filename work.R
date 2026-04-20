library(dplyr)
library(lme4)
library(nycflights13)
library(ggplot2)
library(speedglm)
library(lubridate)

planes <- nycflights13::planes
flights <- nycflights13::flights

## getting all planes that are both in flights and planes datasets
merged_df <- inner_join(planes, flights, by = "tailnum")
merged_df$is_late <- merged_df$arr_delay >= 15 | merged_df$dep_delay >= 15
kept_features <- c("is_late", "tailnum", "carrier", "dest", "distance",
                   "sched_dep_time", "sched_arr_time", "day", "month", "year.x", "engine",
                   "manufacturer", "type", "origin", "carrier")

## getting rid of flights that have NAs and columns we don't need
planes_flights <- merged_df[, c(1, 18)]
planes_flights <- planes_flights[complete.cases(planes_flights), ]
merged_df <- merged_df[, kept_features]
merged_df <- merged_df[complete.cases(merged_df), ]

## average delay times for each unique plane
average_delay <- planes_flights |> group_by(tailnum) |>
  summarise(average_arrival_delay = mean(arr_delay, na.rm = TRUE))

## testing whether each plane is likely to be late
# planes_flights <- planes_flights[1:(0.5*nrow(planes_flights)), ]
# delays <- speedglm(arr_delay ~ 0 + tailnum, family = gaussian(), data = planes_flights)
# delay_summary <- summary(delays)
#
# sum(delay_summary$coefficients$`Pr(>|t|)` < 0.05)
# sum(p.adjust(delay_summary$coefficients$`Pr(>|t|)`, method = "BH") < 0.05)
# sum(p.adjust(delay_summary$coefficients$`Pr(>|t|)`, method = "BH") >= 0.05)


## This gets code only uses 10% of rows. comment this out before running the code above
merged_df <- merged_df[1:round(0.1*nrow(merged_df)), ]


## No need to run these models. They were just used to find the significant predictors
fit0 <- glm(is_late ~ tailnum+sched_dep_time+month+dest+distance, family = "binomial", data = merged_df)
summary_fit1 <- summary(fit1)

fit1 <- glm(is_late ~ tailnum+sched_dep_time+month+dest+day+distance, family = "binomial", data = merged_df)
summary_fit1 <- summary(fit1)

fit2 <- glm(is_late ~ tailnum+sched_dep_time+month+dest+day, family = "binomial", data = merged_df)
summary_fit2 <- summary(fit2)

fit3 <- glm(is_late ~ tailnum+sched_dep_time+month+dest, family = "binomial", data = merged_df)
summary_fit3 <- summary(fit3)

fit4 <- glm(is_late ~ sched_dep_time+month+dest+day+distance, family = "binomial", data = merged_df)
summary_fit4 <- summary(fit4)

fit5 <- glm(is_late ~ tailnum+month+dest+day+distance, family = "binomial", data = merged_df)
summary_fit5 <- summary(fit5)

fit6 <- glm(is_late ~ tailnum+sched_dep_time+dest+day+distance, family = "binomial", data = merged_df)
summary_fit6 <- summary(fit6)

fit7 <- glm(is_late ~ tailnum+sched_dep_time+month+day+distance, family = "binomial", data = merged_df)
summary_fit7 <- summary(fit7)


anova(fit1, fit0) # significant so removing day does not improve model
anova(fit1, fit2) # significant so removing distance does not improve model
anova(fit1, fit4) # significant so removing tailnum does not improve model
anova(fit1, fit5) # significant so removing sched_dep_time does not improve model
anova(fit1, fit6) # significant so removing month does not improve model
anova(fit1, fit7) # significant so removing dest does not improve model

## Model with tailnum, sched_dep_time, month and destination is best at predicting lateness

# Adding other variables

fit8 <- glm(is_late ~ tailnum+sched_dep_time+month+dest+day+distance, family = "binomial", data = merged_df)
summary_fit8 <- summary(fit8)

fit9 <- glm(is_late ~ tailnum+sched_dep_time+month+dest+day+distance+engine, family = "binomial", data = merged_df)
summary_fit9 <- summary(fit9)

fit10 <- glm(is_late ~ tailnum+sched_dep_time+month+dest+day+distance+sched_arr_time, family = "binomial", data = merged_df)
summary_fit10 <- summary(fit10)

fit11 <- glm(is_late ~ tailnum+sched_dep_time+month+dest+day+distance+manufacturer, family = "binomial", data = merged_df)
summary_fit11 <- summary(fit11)

fit12 <- glm(is_late ~ tailnum+sched_dep_time+month+dest+day+distance+carrier, family = "binomial", data = merged_df)
summary_fit12 <- summary(fit12)


anova(fit1, fit8) # significant so we keep the full model, year.x matters
anova(fit8, fit9) # same resid df and deviance is 0
anova(fit8, fit10) # model is insignificant so sched_arr_time does not matter
anova(fit8, fit11) # same values, keep the simpler model
anova(fit8, fit12) # insignificant so carrier doen't matter

## Fit 8 is best

monthly_lates <-subset(merged_df, is_late == TRUE)

seasonal_colors <- c(
  "Jan" = "aliceblue", "Feb" = "antiquewhite", "Dec" = "snow",      # Winter
  "Mar" = "lightgreen", "Apr" = "seagreen3", "May" = "palegreen",  # Spring
  "Jun" = "orange", "Jul" = "gold", "Aug" = "khaki",         # Summer
  "Sep" = "sienna", "Oct" = "saddlebrown", "Nov" = "chocolate"     # Autumn
)

count_flights_by_month <- merged_df[-15] |> group_by(month) |> mutate(n = n())

monthly_lates <- monthly_lates[-15] |> mutate(month = as.character(month(month, label = TRUE, abbr = TRUE)))
# https://stackoverflow.com/questions/57791553/how-to-replace-numeric-month-with-a-months-full-name

monthly_lates$month <- factor(monthly_lates$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
count_flights_by_month$month <- factor(count_flights_by_month$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

delayed_flights_plot <- ggplot(data = monthly_lates, aes(x = month, fill = month)) +
  geom_bar() +
  scale_fill_manual(values = seasonal_colors) +
  labs(y = "Number of Delayed Flights")

annotate("text", x = 7, y = 11500,
         label = "Peak flights in summer",
         color = "black", size = 4)

ggplot(data = count_flights_by_month, aes(x = month, fill = month)) +
  geom_bar() +
  scale_fill_manual(values = seasonal_colors)







