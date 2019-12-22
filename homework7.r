# installing packages
install.packages("RSQLite")
library(RSQLite)
library(DBI)
library(ggplot2)
install.packages("tictoc")
library(tictoc)

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
    delays.by.month[m] <- sum(data.year$delayed[data.year$Month==m]) 
    total.by.month[m] <- total.by.month[m] + dim(data.year[data.year$Month==m,])[1]
  }
  # calculating proportion of delayed flights on a particular day of the week
  for (d in seq(1, 7, by=1)) {
    delays.by.day[d] <- sum(data.year$delayed[data.year$DayOfWeek==d]) 
    total.by.day[d] <- total.by.day[d] + dim(data.year[data.year$DayOfWeek==d,])[1]
  }
  rm(data.year)
  gc()
}
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
  labs(title="Distribution of flights",y= "Number of flights", x = "Year") 

# plotting distribution of proportion delays through years
df.delays.by.year <- data.frame(
  name=years ,  
  value=delays.by.year
)
ggplot(df.delays.by.year, aes(x=name, y=value)) + 
  geom_bar(stat = "identity",color="white", fill="blue", alpha=0.4) + 
  labs(title="Distribution of delayed flights by years",y= "Proportion of delayed flights", x = "Year")

# plotting distribution of proportion of delays through months
df.delays.by.month <- data.frame(
  name=months,  
  value=delays.by.months
)
ggplot(df.delays.by.month, aes(x=name, y=value)) + 
  geom_bar(stat = "identity",color="white", fill="blue", alpha=0.4) + 
  labs(title="Distribution of delayed flights by months",y= "Proportion of delayed flights", x = "Month")

# Plotting the proportions of delayed flights by weekday
df.delays.by.day <- data.frame(
  name=days,  
  value=delays.by.day
)
ggplot(df.delays.by.day, aes(x=name, y=value)) + 
  geom_bar(stat = "identity",color="white", fill="blue", alpha=0.4) + 
  labs(title="Distribution of delayed flights by week days",y= "Proportion of delayed flights", x = "Day")

