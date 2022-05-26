## ODI-Batting cricket data analysis

## Look at the data structure carefully.

## The data is arranged player wise.

## For each player, there are many rows. Each one of these rows have data of a match.

## Each row contains following information in columns:
##     his country of origin    ("Country"
##     which country he played against in each match  ("Versus")
##     Runs he scored in that match  ("Runs")
##     Run rate in the match  ("ScoreRate")
##     Date on which played  ("MatchDate)
##     which day of week he played  ("Weekday")
##     in which playground the match help  ("Ground")

## Note: For a country A playing with B, there will be same entry again
##           with country B playing with A. So, data has double entry!!.

## read data into a data frame
data = read.csv("ODI-Batting_Cricket_Analytics.csv", header=TRUE)


### overview info on data--------------------------------------------

## What is the size of data? -- use "dim" function
print(dim(data))


## What ae the column names?  -- use "colnames" or "names" [both work]
print(colnames(data))


## How many countries are playing? what are their names
## Take the vector of countries and get the unique elements.
num_countries = unique(data$Country)  ## returns a vector of unique elements in the
                                      ## vector given
print(num_countries)


## how many players in these countries
num_players = unique(data$Player)
print(num_players)


## handling dates
## as.Date() function takes a vector of dates according to format given and returns a
##     vector of dates objects.

match_dates <- as.Date(data$MatchDate, "%d/%m/%Y") ## default format returned is yy-mm-dd

## date of latest match in the data?  can use max()
print(max(match_dates))

## date of earliest match in the data?  can use min()
print(min(match_dates))

## data spans how many years?  can use difftime() function to get time difference.
df = difftime(max(match_dates), min(match_dates), unit="days") # “auto”, “secs”, “mins”, “hours”, “days”, “weeks”
print(as.numeric(df/365))
print( paste(as.numeric(df/365), " years") ) 
print( paste(as.integer(df/365), " years") )   ### 40 years crossed.


### what is the highest score ever?  
print(max(data$Runs, na.rm=TRUE)) ## max of Runs
print(which(data$Runs == max(data$Runs, na.rm=TRUE)) ) ## which row has this max runs?
data[ which(data$Runs == max(data$Runs, na.rm=TRUE)), ]  ## give that row number to get the data of that row.

##--------------------------------------------------------------------------------------------------

## Data on India

dat_india = subset(data, data$Country=="India")

## how many matches India has played in these 40 years?
print(dim(dat_india))

## who all have played for India?
print(unique(dat_india$Player))
## sort it and print.
print(unique(dat_india$Player))

## subset matches played by Sunil gavaskar
dat_gavaskar = subset(dat_india, dat_india$Player=="Sunil M Gavaskar")

## how many matches Gavaskar played?
print(dim(dat_gavaskar))

## what is his highest and lowest scores?
print(max(dat_gavaskar$Runs))


## Gavaskar's maximum score is with which country?
print(min(dat_gavaskar$Runs))



## Did he ever scored a duck?. how many times? against which countries?


## What is the maximum score of gavaskar against west indies?


## what is the mean score of gavaskar against australia?


## Ask questions for Tendulkar and answer












