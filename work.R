library(dplyr)
library(lme4)
library(nycflights13)
library(ggplot2)
library(speedglm)

planes <- nycflights13::planes
flights <- nycflights13::flights

## getting all planes that are both in flights and planes datasets
merged_df <- inner_join(planes, flights, by = "tailnum")
merged_df$is_late <- merged_df$arr_delay > 0
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

fit8 <- glm(is_late ~ tailnum+sched_dep_time+month+dest+day+distance +year.x, family = "binomial", data = merged_df)
summary_fit8 <- summary(fit8)

fit9 <- glm(is_late ~ tailnum+sched_dep_time+month+dest+day+distance +year.x+engine, family = "binomial", data = merged_df)
summary_fit9 <- summary(fit9)

fit10 <- glm(is_late ~ tailnum+sched_dep_time+month+dest+day+distance +year.x+sched_arr_time, family = "binomial", data = merged_df)
summary_fit10 <- summary(fit10)

fit11 <- glm(is_late ~ tailnum+sched_dep_time+month+dest+day+distance +year.x+manufacturer, family = "binomial", data = merged_df)
summary_fit11 <- summary(fit11)

fit12 <- glm(is_late ~ tailnum+sched_dep_time+month+dest+day+distance +year.x+carrier, family = "binomial", data = merged_df)
summary_fit12 <- summary(fit12)



anova(fit1, fit8) # significant so we keep the full model, year.x matters
anova(fit8, fit9) # same resid df and deviance is 0
anova(fit8, fit10) # model is insignificant so sched_arr_time does not matter
anova(fit8, fit11) # same values, keep the simpler model
anova(fit8, fit12) # insignificant so carrier doen't matter

## Fit 8 is best

## Run this to see if type is a valid predictor
fit13 <- glm(is_late ~ tailnum+sched_dep_time+month+dest+day+distance +year.x+type, family = "binomial", data = merged_df)
summary_fit13 <- summary(fit13)

## If p-value is significant then type is a good predictor
anova(fit8, fit13)
