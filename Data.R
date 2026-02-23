#install.packages("nycflights13")
#library(nycflights13)
nycflights13::flights
x <- nycflights13::flights
#write.csv(x, "nycflights13.csv")
nycflights13 <- read_csv("Data/nycflights13.csv")

