# installing packages
install.packages("RSQLite")
install.packages("sqldf")
install.packages("tictoc")
install.packages("biglm")
library(RSQLite)
library(DBI)
library(ggplot2)
library(tictoc)
library(sqldf)
library(biglm)

db.path = 'D:/Fax/Winter 2019 - 2020/Statistical Principles of Data Science/Homework/Homework 7/data/AirlineDB'

# creating connection to the database
delay.con <- dbConnect(RSQLite::SQLite(), dbname=db.path)
# see available tables
dbListTables(delay.con)


# -------------------------
# Exploratory data analysis
# -------------------------

years <- seq(1987, 2008, by=1)
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Avg", "Sep", "Oct", "Nov", "Dec")
days <- c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun")
num.years <- length(years)
flights.by.year <- rep(0, num.years) 
delays.by.year <- rep(0, num.years)
delays.by.month <- rep(0, 12)
total.by.month <- rep(0, 12)
delays.by.day <- rep(0, 7)
total.by.day <- rep(0, 7)

tic()
for (i in seq(1, num.years, by=1)) {
  query.year <- paste("SELECT * FROM 'AirlineData' WHERE Year=", years[i])
  # data for particular year
  data.year <- dbGetQuery(delay.con, query.year) 
  # calculating total number of flights in a particular year
  flights.by.year[i] <- dim(data.year)[1]
  # calculating proportion of delayed flights in one year
  data.year$delayed <- (data.year$ArrDelay > 10) + 0
  delays.by.year[i] <- sum(data.year$delayed) / flights.by.year[i]
  # calculating proportion of delayed flights in a particular month
  for (m in seq(1, 12, by=1)) {
    delays.by.month[m] <- delays.by.month[m] + sum(data.year$delayed[data.year$Month==m]) 
    total.by.month[m] <- total.by.month[m] + dim(data.year[data.year$Month==m,])[1]
  }
  # calculating proportion of delayed flights on a particular day of the week
  for (d in seq(1, 7, by=1)) {
    delays.by.day[d] <- delays.by.day[d] + sum(data.year$delayed[data.year$DayOfWeek==d]) 
    total.by.day[d] <- total.by.day[d] + dim(data.year[data.year$DayOfWeek==d,])[1]
  }
  # calculating proportion of delayed flights on a particular airport
  origins <- sqldf("SELECT Origin, count(*) FROM 'data.year' GROUP BY Origin")
  colnames(origins) <- c("airport", "count")
  origins.delayed <- sqldf("SELECT Origin, count(*) FROM 'data.year' WHERE delayed=1 GROUP BY Origin")
  colnames(origins.delayed) <- c("airport", "count")
  # calculating proportion of delayed flights with a particular airline
  airlines <- sqldf("SELECT UniqueCarrier, count(*) FROM 'data.year' GROUP BY UniqueCarrier")
  colnames(airlines) <- c("airline", "count")
  airlines.delayed <- sqldf("SELECT UniqueCarrier, count(*) FROM 'data.year' WHERE delayed=1 GROUP BY UniqueCarrier")
  colnames(airlines.delayed) <- c("airline", "count")
  if (i != 1) {
    total.by.origin <- merge(total.by.origin, origins, by="airport", all=TRUE)
    delayed.by.origin <- merge(delayed.by.origin, origins.delayed, by="airport", all=TRUE)
    total.by.airline <- merge(total.by.airline, airlines, by="airline", all=TRUE)
    delayed.by.airline <- merge(delayed.by.airline, airlines.delayed, by="airline", all=TRUE)
  } else {
    total.by.origin <- origins
    delayed.by.origin <- origins.delayed
    total.by.airline <- airlines
    delayed.by.airline <- airlines.delayed
  }
  rm(origins)
  rm(origins.delayed)
  rm(airlines)
  rm(airlines.delayed)
  rm(data.year)
  gc()
}

# sum up all values 
total.by.origin$sum <- rowSums(total.by.origin[,2:ncol(total.by.origin)])
delayed.by.origin$sum <- rowSums(delayed.by.origin[,2:ncol(delayed.by.origin)])
total.by.airline$sum <- rowSums(total.by.airline[,2:ncol(total.by.airline)])
delayed.by.airline$sum <- rowSums(delayed.by.airline[,2:ncol(delayed.by.airline)])
# merge data sets with sums
df.delayed.by.origin <- merge(total.by.origin[, c("airport", "sum")], delayed.by.origin[, c("airport", "sum")], by="airport", all=TRUE)
df.delayed.by.airline <- merge(total.by.airline[, c("airline", "sum")], delayed.by.airline[, c("airline", "sum")], by="airline", all=TRUE)
# divide values
df.delayed.by.origin <- transform(df.delayed.by.origin, prop=sum.y/sum.x)
df.delayed.by.airline <- transform(df.delayed.by.airline, prop=sum.y/sum.x)
delays.by.month <- delays.by.month / total.by.month
delays.by.day <- delays.by.day / total.by.day
toc()

# plotting distribution of flights through years
df.flight.by.years <- data.frame(
  name=years ,  
  value=flights.by.year
)
ggplot(df.flight.by.years, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", color="white", fill="blue", alpha=0.4) +
  labs(y= "Number of flights", x = "Year") 

# plotting distribution of proportion delays through years
df.delays.by.year <- data.frame(
  name=years,  
  value=delays.by.year
)
ggplot(df.delays.by.year, aes(x=name, y=value)) + 
  geom_bar(stat = "identity",color="white", fill="blue", alpha=0.4) + 
  labs(y= "Proportion of delayed flights", x = "Year")

# plotting distribution of proportion of delays through months
df.delays.by.month <- data.frame(
  name=factor(months,levels=months),  
  value=delays.by.month
)
ggplot(df.delays.by.month, aes(x=name, y=value)) + 
  geom_bar(stat = "identity",color="white", fill="blue", alpha=0.4) + 
  labs(y= "Proportion of delayed flights", x = "Month")

# Plotting the proportions of delayed flights by weekday
df.delays.by.day <- data.frame(
  name=factor(days, levels=days),  
  value=delays.by.day
)
ggplot(df.delays.by.day, aes(x=name, y=value)) + 
  geom_bar(stat = "identity",color="white", fill="blue", alpha=0.4) + 
  labs(y= "Proportion of delayed flights", x = "Day")

# plotting the proportion of delayed flights by origin airports
# sort values of delayed flights
df.delayed.by.origin <- df.delayed.by.origin[order(-df.delayed.by.origin$prop),]
df.delayed.by.origin$airport <- factor(df.delayed.by.origin$airport , levels=df.delayed.by.origin$airport )
ggplot(df.delayed.by.origin[1:20,], aes(x=airport, y=prop)) + 
  geom_bar(stat = "identity",color="white", fill="blue", alpha=0.4) + 
  labs(y= "Proportion of delayed flights", x = "Airport")

## plotting the proportion of delayed flights by airlines
df.delayed.by.airline <- df.delayed.by.airline[order(-df.delayed.by.airline$prop),]
df.delayed.by.airline$airline <- factor(df.delayed.by.airline$airline , levels=df.delayed.by.airline$airline )
ggplot(df.delayed.by.airline[1:20,], aes(x=airline, y=prop)) + 
  geom_bar(stat = "identity",color="white", fill="blue", alpha=0.4) + 
  labs(y= "Proportion of delayed flights", x = "Airline")

# factors needed for regression
origin.factors <- df.delayed.by.origin$airport
airline.factors <- df.delayed.by.airline$airline

# removing all variables
rm(list=setdiff(ls(), c("origin.factors", "airline.factors", "num.years", "delay.con", "years")))
gc()


# ----------
# Regression
# ----------
tic()
# seed for generating a random sample
set.seed(42)
sample.size = 100000
for (i in seq(1, num.years, by=1)) {
  # data for particular year
  query.year <- paste("SELECT * FROM 'AirlineData' WHERE Year=", years[i])
  data.year <- dbGetQuery(delay.con, query.year) 
  # shuffled data
  shuffled.indices <- sample(sample.size)
  data.year <- data.year[shuffled.indices, ]
  # turn columns into factors
  data.year$Year <- factor(data.year$Year, ordered=TRUE, levels=years)
  data.year$Month <-factor(data.year$Month, levels=c(1,2,3,4,5,6,7,8,9,10,11,12))
  data.year$DayOfWeek <- factor(data.year$DayOfWeek, levels=c(1,2,3,4,5,6,7))
  data.year$Origin <-factor(data.year$Origin, levels=origin.factors)
  data.year$UniqueCarrier <- factor(data.year$UniqueCarrier, levels=airline.factors)
  # changing to numeric
  data.year[,c(5,7,14,15,16,19)] <- lapply(data.year[, c(5,7,14,15,16,19)], as.numeric)
  # adding delayed column
  data.year$delayed <- (data.year$ArrDelay > 10) + 0
  # regression formula
  formula <- delayed~Year+Month+DayOfWeek+DepTime+Distance+Origin+UniqueCarrier
  if (i == 1) {
    model <- biglm(formula, data.year)  
  } else {
    model <- update(model, data.year)
  }
  rm(data.year)
  gc()
}
  
toc()

