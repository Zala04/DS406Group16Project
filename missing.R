colnames(nycflights13)
library(tidyverse)
flightsMissingessTrial <- nycflights13 %>% select(c("sched_dep_time", "dep_delay", "arr_time",
                                                    "sched_arr_time","arr_delay")) %>% filter(n()>1000)

set.seed(77)
mddf <- flightsMissingessTrial
a = sample(1:nrow(mddf), 500)
sampleA = mddf[a, ]

pairs(sampleA)

mddf <- flightsMissingessTrial
head(mddf)
colSums(is.na(mddf))


head(mddf)

missing_plot = function(v1,v2){
  m1 = is.na(v1)
  m2 = is.na(v2)
  plot(v1,v2, xlab = deparse(substitute(v1)),
       ylab = deparse(substitute(v2)))
  points(v1[which(m2)],rep(min(v2, na.rm = 1),sum(m2)),
         pch = 1, col = 'red' , cex = 3, lwd = 2)
  points(rep(min(v1, na.rm = 1),sum(m1)),v2[which(m1)],
         pch = 1, col = 'red' , cex = 3, lwd = 2)
}
missing_plot(v1 = mddf$sched_dep_time, v2 = mddf$arr_delay)


sum(is.na(mddf$dep_delay) & is.na(mddf$arr_delay)) # cancelled flights
sum(is.na(mddf$dep_delay) & !is.na(mddf$arr_delay))
sum(!is.na(mddf$dep_delay) & is.na(mddf$arr_delay)) # dep delay not missing but arr missing <- diversion myb
# means when dep delay is missing so is arrival delay
# and it depends on other variables, so it's not MCAR
sum(is.na(mddf$sched_arr_time) & is.na(mddf$arr_delay))
sum(!is.na(mddf$sched_arr_time) & is.na(mddf$arr_delay))
sum(is.na(mddf$sched_arr_time) & is.na(mddf$arr_delay))
library(dplyr)


missing_table

mddf <- mddf %>%
  mutate(missing_dep = ifelse(is.na(dep_delay), "Missing", "Not missing"))

  ggplot(mddf, aes(x = sched_dep_time, colour = missing_dep)) +
  geom_density() +
  labs(title = "Density of Scheduled Departure Time vs Departure Delays)",
    x = "Scheduled Departure Time",
    y = "Density",
    colour = "Departure Delay")
library(nycflights13)

flights |>
  distinct(faa = dest) |>
  anti_join(airports)
unique(airports$name)

flights |>
  distinct(tailnum) |>
  anti_join(planes)

gg1 <- flights |>
  anti_join(planes, by = "tailnum") |>
  group_by(carrier) |>
  summarise(proportion_missing = n() / nrow(flights %>%
        filter(carrier == first(carrier))))%>% arrange(proportion_missing)

gg <- flights %>% group_by(carrier) %>% summarise(Flights=n())
inner_join(gg, gg1) %>% arrange(desc(proportion_missing))
