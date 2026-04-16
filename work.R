nycflights13::weather
planes <- nycflights13::planes
planes |>  mean(speed, na.rm = TRUE)

unique(nycflights13::planes$speed)

planes[complete.cases(planes),"speed"]$speed
unique(planes$speed)

unique(planes$tailnum)

plot(x$proportionDelay)

unique(nycflights13::airlines)

nycflights13::planes


subset(planes, unique(flights$tailnum) == unique(planes$tailnum))


length(unique(flights$tailnum))
length(unique(planes$tailnum))

grepl(flights$tailnum, planes$tailnum)
