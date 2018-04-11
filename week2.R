
# BUild a table that shows how many cities are the origin
# for the flights throughout 2008
table(myDF$Origin)
# sort the results, and see that Atlanta (ATL) is most often used
# as a city of origin for flights
sort(table(myDF$Origin))


# here are some sample departure times
head(myDF$DepTime)
# we might want to break them up into these categories
?seq
seq(0, 2400, by=100)
# Enumerates the number of flights that departed within
# each hour range during the course of the day
?cut
# cut divides the range of x into intervals 
# and codes the values in x according to which interval they fall. 
# The leftmost interval corresponds to level one,
# the next leftmost to level two and so on.
# cut(x, breaks, labels = NULL, 
# include.lowest = FALSE, right = TRUE, dig.lab = 3, 
# ordered_result = FALSE, ...)
table(cut(myDF$DepTime, breaks=seq(0, 2400, by=100)))
# Here is the corresponding plot
# and we will improve the way the x-axis looks later
plot(table(cut(myDF$DepTime, breaks=seq(0, 2400, by=100))))


plot(table(cut(myDF$DepTime, breaks=seq(0, 2400, by=100), dig.lab = 4)))

head(myDF$Distance)
# las  numeric in {0,1,2,3}; the style of axis labels.
# 0: always parallel to the axis [default],
# 1: always horizontal,
# 2: always perpendicular to the axis,
# 3: always vertical.

plot(table(cut(myDF$Distance, breaks = seq(0, 5000, by=100), dig.lab=4)), las=2)


# Make a table of all the origin-to-destination pairs
# then sort the table, and find which are the most polupar
# such pairs, by examining only the tail
# Here are the most popular 20 such pairs
tail(sort(table(paste(myDF$Origin, "to", myDF$Dest))), 20)
plot(tail(sort(table(paste(myDF$Origin, "to", myDF$Dest))), 20))
plot(tail(sort(table(paste(myDF$Origin, "to", myDF$Dest))), 20), las=2)
?plot
head(sort(table(paste(myDF$Origin, "to", myDF$Dest))), 20)

# Quiz 6
# How many origin-to-destination paths 
# were only flown one time (each) in 2008?
sum(table(paste(myDF$Origin, "to", myDF$Dest)) == 1)


# Introduction of the Tapply Function

# how the Tapply functions works
# 1. the vector of data we want to apply a function to
# 2. the way to break up the data into pieces
# 3. the function we want to apply to the data
# 4. we can put as the 4th element extra information
#    for instance, very commonly, we use na.rm = TRUE

# Find the average departure delay at each airport
tapply(myDF$DepDelay, myDF$Origin, mean, na.rm=TRUE)

# Here are the airports with the largest average departure dalays
tail(sort(tapply(myDF$DepDelay, myDF$Origin, mean, na.rm=TRUE)))

# Here are the airports with the smallest average departure dalays
head(sort(tapply(myDF$DepDelay, myDF$Origin, mean, na.rm=TRUE)))

# Here are the airports with the largest average arrival dalays
tail(sort(tapply(myDF$ArrDelay, myDF$Dest, mean, na.rm=TRUE)))

# Here are the airports with the smallest average arrival dalays
head(sort(tapply(myDF$ArrDelay, myDF$Dest, mean, na.rm=TRUE)))


# Quiz 7

# When considering all flights to an airport, 
# take an average of the distances (in miles) of the flights to that airport. 
# Suppose that we do such an analysis of all airports. 
# Which airport has the longest average distance of flights (in miles) 
# arriving to that airport?

tail(sort(tapply(myDF$Distance, myDF$Dest, mean, na.rm=TRUE)))


# Arrival Delays by day of the week

# Which day of the week should we fly, if we want to minimize
# the expected arrival delay of the flight?
plot(tapply(myDF$ArrDelay, myDF$DayOfWeek, mean, na.rm=TRUE))
# Here 1 denotes Monday, 2 denotes Tuesday, ..., 7 denotes Sunday

# Answering the same question, but restricting attention
# to flights that have IND as the destination airport
# we get the average arrival day of the flights.
# for each day of the week, and restricting (only) to IND arrivals
tapply(myDF$ArrDelay[myDF$Dest == "IND"],
       myDF$DayOfWeek[myDF$Dest == "IND"], mean, na.rm=TRUE )

plot(tapply(myDF$ArrDelay[myDF$Dest == "IND"],
       myDF$DayOfWeek[myDF$Dest == "IND"], mean, na.rm=TRUE ))

# Just double-checking that we are working on two vectors
# that have the same lengths
length(myDF$ArrDelay[myDF$Dest == "IND"])
length(myDF$DayOfWeek[myDF$Dest == "IND"])

# Make a plot of the average departure delays for each airport of origin.
# my answer
plot(tapply(myDF$DepDelay, myDF$Origin, mean, na.rm=TRUE))

sort(tapply(myDF$DepDelay, myDF$Origin, mean, na.rm=TRUE))

plot(sort(tapply(myDF$DepDelay, myDF$Origin, mean, na.rm=TRUE)))

# peer learners' answers
# by John Johal
plot(sort(round(tapply(myDF$DepDelay, myDF$Origin, mean, na.rm=TRUE))))
# producing a curved plot with a definite central tendancy. 
# The above plot assigns an index (from 1 to 303) to each Origin airport 
# on the horizontal axis. 
# This index corresponds to the airport's order in the sort, 
# but is otherwise meaningless, and potentially confusing to interpret.

# I found the plot below more informative. 
# It shows the frequencies for each delay period 
# (rounded to the nearest minute) 
# and displays a reasonably normal distribution for the delay times
# by Joseph Murphy
plot(table(tapply(myDF$DepDelay, myDF$Origin, mean, na.rm=TRUE)))

table(tapply(myDF$DepDelay, myDF$Origin, mean, na.rm=TRUE))

# plot(tapply(myDF$DepDelay, myDF$Origin, mean, na.rm = TRUE)) does the job. 
# But the scatterplot does not give any information about the airports (code).
# You can see that the majority of the airports 
# have an average arrival delay of somewhere between 6 and 10 minutes. 
# But you can see that more clearly with:
# by Maarten Luykx
plot(table(floor(tapply(myDF$DepDelay, myDF$Origin, mean, na.rm=TRUE))))
# floor is a function that takes as input a (serie of) real number(s) 
# and gives as output a(n) (serie of) integer(s) 
# with the figures behind the decimal point deleted. 
# eg. 9.35 -> 9 and 12.934987 -> 12. 
# In plot(table(floor(tapply(myDF$DepDelay, myDF$Origin, mean, na.rm=TRUE)))) 
# the effect is that all the real values for the average departure delays
# are transformed to integers. 
table(floor(tapply(myDF$DepDelay, myDF$Origin, mean, na.rm=TRUE)))


# Which airlines have the best average Arrival Delays?
# Which have the worst average Arrival Delays?
sort(tapply(myDF$ArrDelay, myDF$UniqueCarrier, mean, na.rm=TRUE))

# How many flights occur per month?
table(myDF$Month)
tapply(myDF$Month, myDF$Month, length)


# Quiz 8

# Which airline has the worst average departure delay?
# my answer: United Airlines Inc. with average departure delay of 14.11258 mins

tail(sort(tapply(myDF$DepDelay, myDF$UniqueCarrier, mean, na.rm=TRUE)))


# On which day of the year were the expected Arrival Delays the worst?

mydates <- paste(myDF$DayofMonth, myDF$Month, myDF$Year, sep="/")

# sep="/"  -- sep means seperation, use "/" as seperation between elements

# Just double-checking that the vectors we are working with
# have the same length.
length(myDF$ArrDelay)
length(mydates)

# This is a leap year, so we get 366 values for the result.

length(tapply(myDF$ArrDelay, mydates, mean, na.rm=TRUE))

# Here are all 366 days in 2008.
# Sorted according to the expected ArrDelay on that date
sort(tapply(myDF$ArrDelay, mydates, mean, na.rm=TRUE))

# Answer: the worst expected arrival delay day is 19/12/2008

# The expected ArrDelay for each date,
# but only for the flights that arriving to IND.
sort(tapply(myDF$ArrDelay[myDF$Dest =="IND"], 
            mydates[myDF$Dest == "IND"], mean, na.rm=TRUE))


# Quiz 9

# Q1:On which day of the year were the average departure delays the worst?
sort(tapply(myDF$DepDelay, mydates, mean, na.rm=TRUE))

# Q2:On which day of the year were the average departure times the worst 
#    for flights departing from O¡¯Hare (ORD)?
sort(tapply(myDF$DepDelay[myDF$Origin == "ORD"], 
            mydates[myDF$Origin == "ORD"], mean, na.rm=TRUE))

# Analyzing Arrival Delays according to the flight path

# We can extend that example,
# now specify that the Dest airport is IND
# and that the Origin airport is ORD

head(myDF$Dest=="IND")
head(myDF$Origin=="ORD")

# This is a vector that identifies the flights
# which have IND as Dest and have ORD as the Origin
# It is a vector of 7 million TRUE's AND FALSE's
myDF$Dest=="IND" & myDF$Origin=="ORD"

head(myDF$Dest=="IND" & myDF$Origin=="ORD")

length(myDF$Dest=="IND" & myDF$Origin=="ORD")

ordtoind <- myDF$Dest=="IND" & myDF$Origin=="ORD"

# We find the expected arrival delay for flights
# from ORD to IND, categorized according to the date.

sort(tapply(myDF$ArrDelay[ordtoind], 
            mydates[ordtoind], mean, na.rm=TRUE))


# Quiz 10

# Consider flights that departed from ATL and landed at LAX in 2008. 
# For how many days of the year were the average departure delays
# more than 90 minutes?

head(ATLtoLAX)

ATLtoLAXDates <- paste(ATLtoLAX$DayofMonth, ATLtoLAX$Month, ATLtoLAX$Year, sep="/")

sort(tapply(ATLtoLAX$DepDelay, ATLtoLAXDates, mean, na.rm=TRUE))

table(tapply(ATLtoLAX$DepDelay, ATLtoLAXDates, mean, na.rm=TRUE))

sum(tapply(ATLtoLAX$DepDelay, ATLtoLAXDates, mean, na.rm=TRUE) > 90)
