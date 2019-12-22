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

# number of flights per year
years <- seq(1987, 2008, by=1)
num.years <- length(years)
flights.by.year <- rep(0, num.years) 
delays.by.year <- rep(0, num.years)

tic()
for (i in seq(1, num.years, by=1)) {
  query.year <- paste("SELECT * FROM 'AirlineData' WHERE Year=", years[i])
  data.year <- dbGetQuery(delay.con, query.year) 
  data.year$delayed <- (data.year$ArrDelay > 10) + 0
  flights.by.year[i] <- dim(data.year)[1]
  delays.by.year[i] <- sum(data.year$delayed)
}
toc()

# plotting distribution of flights through years
df.flight.by.years <- data.frame(
  name=years ,  
  value=flights.by.year
)
ggplot(df.flight.by.years, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", color="white", fill="blue", alpha=0.4) +
  labs(title="Distribution of flights",y= "Number of flights", x = "Year") 

# plotting distribution of delays through years
df.delays.by.year <- data.frame(
  name=years ,  
  value=delays.by.year
)
ggplot(df.delays.by.year, aes(x=name, y=value)) + 
  geom_bar(stat = "identity",color="white", fill="blue", alpha=0.4) + 
  labs(title="Distribution of delayed flights",y= "Number of delayed flights", x = "Year")

