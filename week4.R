
# Assembling and storing data

# 4.1 Assembling Muiltiple Years of Airline Data

# A fresh start

# to combine multi files in a dataframe

# Load in three seperate files
df1 <- read.csv("2006.csv")
df2 <- read.csv("2007.csv")
df3 <- read.csv("2008.csv")

# check the dimension
dim(df1)
dim(df2)
dim(df3)

# rbind (row combine) three dataframe

?rbind
myDF <- rbind(df1, df2, df3)
# check the dimension of the new data frame myDF
dim(myDF)

# remove the three small dataframe to release memory
rm(df1, df2, df3)

# check the head and the tail of the new data frame with the 2006, 2007, 2008 flights
head(myDF)
tail(myDF)

?unique
# unique returns a vector, data frame or array like x 
# but with duplicate elements/rows removed.

unique(myDF$Year)

# Quiz 17

# How many flights departed altogether from LAX during the period 2006 to 2008?

table(myDF$Origin == "LAX")
# This  returns TRUE and FAlSE numbers

sum(myDF$Origin == "LAX")
# This returns the number of flights departed from LAX from 2006 to 2008



# 4.3 Efficiently Storing Origin-to-Destination flight paths


mytable <- table(list(myDF$Origin, myDF$Dest))

head(mytable)

# We can make a table with 315 rows and 321 coulums,
# so that each entry corresponds to a unique Origin and Destination pair
dim(mytable)

315*321

sum(mytable == 0)
sum(mytable != 0)

mynewtable <- table(paste(myDF$Origin, myDF$Dest))
length(mynewtable)

head(mynewtable)
tail(mynewtable)

# Think about what is the most efficent way to store data,
# and what you want to do with data ahead of time.


# Quiz 18

# Q1 How many flights were there from IND to ORD from 2006 to 2008?

# use sum function
sum(myDF$Origin == "IND" & myDF$Dest == "ORD")

# or use the table we created
mynewtable["IND ORD"]


# Answer: 11254

# Q2 Make a table that counts the number of entries (simultaneously) 
# according to the origin, destination, and year. 
# How many flights were there from BOS to DEN in 2007? 
# Use the same data frame we already built. Do not reload all of the 2007 data.

table(paste(myDF$Origin, myDF$Dest, myDF$Year))["BOS DEN 2007"]

# Answer: 2445

# Visualizing flight paths

# 4.5 Visualizing flight paths

# neither of these plots are very helpful
plot(mynewtable)

dotchart(mynewtable)

# we could try to sort the data first
dotchart(sort(mynewtable))

# we are focusing on the flights with IND as origin

plot(mytable["IND", ])

dotchart(mytable["IND", ])

# save that flight data into a vector

v <- mytable["IND", ]

# now we only plot the flights from IND to airports that at least one flight
v[v != 0]
dotchart(sort(v[v != 0]))


dotchart(sort(v[v > 4000]))

# 4.6 Create a visualization

# Make a visualization (of your choice) 
# related to a flight path of interest to you. 

# I want to know the top 10 cancelled flight path departed from ORD (Chicago)

ORDcancelled <- subset(myDF, myDF$Origin == "ORD" & myDF$Cancelled !=0)
tail(sort(table(paste(ORDcancelled$Origin,"to",ORDcancelled$Dest))), n=10)

plot(tail(sort(table(paste(ORDcancelled$Origin,"to",ORDcancelled$Dest))), n=10))

dotchart(tail(sort(table(paste(ORDcancelled$Origin,"to",ORDcancelled$Dest))), n=10))

# from the chart, we can see that
# the most cancelled path is ORD to LGA


# 4.7 Incorporating Auxiliary data about airports

# Importing the data about the airports themselves (locations, etc.)
airportsDF <- read.csv("E://study/IT/R/airports.csv")

dim(airportsDF)

head(airportsDF, n=100)

airportsDF[airportsDF$iata == "IND", ]

airportsDF[airportsDF$iata %in% c("IND", "ORD", "MDW"), ]

# we made a vector to store the airport name, city, state
w <- paste(airportsDF$airport, airportsDF$city, airportsDF$state, sep=", ")

head(w)
tail(w)

?names

# Functions to get or set the names of an object.
# Usage
# names(x)
# names(x) <- value


# we are going to make the "name" of each entry in the vector
# be the 3 letter airport code itself

names(w) <- airportsDF$iata
head(w)
tail(w)
w[c("IND", "ORD", "MDW")]

w["CMH"]

# Think about the importance of indexing.



# Quiz 19

# According to the data in the airports.csv file, 
# how many airports are located in the City of Chicago?

airportsDF[airportsDF$city == "Chicago", ]
# the answer is not obviously, as there are still NA entries

sum(airportsDF$city == "Chicago", na.rm = T)
# the answer is clear for this

# Answer: there are 3 airports in Chicago.


# 4.9 Revising visualization of flight paths

# remember that this is the data we plotted in the dotchart
v[v > 4000]


# we still do not know where (city, state) these airports are located
# unless we just recognized the airport codes

w[names(v[v > 4000])]

# This is the data that we plotted

myvec <- v[v > 4000]

names(myvec) <- w[names(v[v > 4000])]


myvec
dotchart(myvec)
dotchart((sort(myvec)))

# this is a more insightful version of our earlier dotchart
# because we now have airport names and locations

# Consider the visualization we made, 
# which shows 13 airports that served as destinations 
# for at least 4,000 flights which originated in IND. 
# Revise this visualization to show (only) the name 
# of each such airport but not the city and state. 
# Once you have done this, you may mark this step complete.

# make a vector(wnames) to store airport names
wnames <- airportsDF$airport

# Names the vector(wnames) with its 3 letter airport code
names(wnames) <- airportsDF$iata

# returns the names of the 13 airports
wnames[names(v[v > 4000])]

# make a vector (myvec1) to store the flights number of these 13 airport
myvec1 <- v[v > 4000]

# Names the vector(myvec1) with the airport names
names(myvec1) <- wnames[names(v[v > 4000])]

# check what the new vector (myvec1) looks like
myvec1
# make the dotchart
dotchart(myvec1)
dotchart(sort(myvec1))



# 4.10 Identifying airports with commercial flights

head(airportsDF)
table(airportsDF$state)


subset(airportsDF, state == "IN")

indyairports <- subset(airportsDF, state == "IN")

# we can make a table that shows all of the flight counts
# (as origin) for all airports in the full data set from 2006 to 2008
# (not just Indiana airports)

table(myDF$Origin)
table(myDF$Origin)["IND"]
table(myDF$Origin)["ORD"]



indyairports$iata
# check the class, it shows "factor"
class(indyairports$iata)

# make it as character
# these are the 3-letter airport codes for the airports in Indiana
as.character(indyairports$iata)

# we can check to see which of these show up 
# in our airline data from 2006 to 2008
table(myDF$Origin)[as.character(indyairports$iata)]

# we save that information (many of the entries are NA, which means that
# there were no commercial flights from those airports between 2006 to 2008)
v <- table(myDF$Origin)[as.character(indyairports$iata)]

# find which of them are not NA
v[!is.na(v)]

# get the names
# these are the four airports in Indiana that do have commercial flights
names(v[!is.na(v)])

# check the details of airports with commercial flights
# and here is where those airports are located in the state of Indiana
subset(airportsDF, iata %in% names(v[!is.na(v)]))

# Quiz 20

# Combining the information from the airports.csv file 
# and from the 2006 to 2008 airline data sets, 
# how many airports in California 
# had at least one flight in the data set 
# during this time period (2006-2008)?

# we can see from the table that
# there are 205 airports in CA
table(airportsDF$state)

subset(airportsDF, state == "CA")

CAairports <- subset(airportsDF, state == "CA")


class(CAairports$iata)
 
as.character(CAairports$iata)

table(myDF$Origin)[as.character(CAairports$iata)]

w <- table(myDF$Origin)[as.character(CAairports$iata)]

w[!is.na(w)]

length(names(w[!is.na(w)]))
# Answer: 26


# 4.12 Creating and applying functions built by the learner

# let's build a function that for a state given by the user
# will identify all of the airports in the 2006 to 2008 data set
# that have commercial flights from that state

# start with "IN" as the state example

# Succintly, this summarizes what we did in the previous video
mystate <- "IN"

myairports <- subset(airportsDF, state == mystate)
v <- table(myDF$Origin)[as.character(myairports$iata)]
subset(airportsDF, iata %in% names(v[!is.na(v)]))

# now wrap this into a function that lets us do this for any state

# the name of my function I am creating is called "activeairports"
# because that is the information it finds
# it only needs 1 input, namely, the state to look in
activeairports <- function(mystate){
  myairports <- subset(airportsDF, state == mystate)
  v <- table(myDF$Origin)[as.character(myairports$iata)]
  subset(airportsDF, iata %in% names(v[!is.na(v)]))
}

# seems to work well for the Indiana airports
activeairports("IN")

# how about Illinois?
activeairports("IL")

# what about California?
activeairports("CA")

dim(activeairports("CA"))

dim(activeairports("CA"))[1]



# 4.13 How many airports?

?sapply

# lapply returns a list of the same length as X, 
# each element of which is the result of applying FUN 
# to the corresponding element of X.

# sapply is a user-friendly version and wrapper of lapply
# by default returning a vector, matrix or, 
# if simplify = "array", an array if appropriate

# Usage

# lapply(X, FUN, ...)

# sapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)

# The R platform provides a built-in vector called ¡°state.abb.¡±

# Try the one line of code given below, 
# which will run the function ¡°activeairports¡± that we designed
# on each element of the built-in vector state.abb
# and compare your results 
# with the number of airports in Indiana, Illinois, and California. 
# The results should agree with what we just learned.

sapply(state.abb,function(x) dim(activeairports(x))[1])

# We included the ¡°[1]¡± there 
# so that we could only see the first dimension of the result.

# The sapply function is used to 
# apply a function to every element of a vector. 
# In this case, the function we apply to each state (referred to as ¡°x¡± here) 
# is: dim(activeairports(x))[1]



# 4.14 Incorporating tail numbers from airplanes

# Tail number is the unique code of an plane

# Consider the distances flown by the planes (this is the "data" in the tapply),
# split up according to the tailnum of the planes themselves ( this is the way to split),
# and the function we use, within each group of distances, is the "sum"

tapply(myDF$Distance, myDF$TailNum, sum)

head(sort(tapply(myDF$Distance, myDF$TailNum, sum)))
tail(sort(tapply(myDF$Distance, myDF$TailNum, sum)))

# here is an admittedly terrible plot of this
dotchart(sort(tapply(myDF$Distance, myDF$TailNum, sum)))

# we dive in further

v <- sort(tapply(myDF$Distance, myDF$TailNum, sum))
head(v)

v <- tail(v, 23)

# now v has the information about the 23 flights that flew the most miles
v

# but the last 3 are erroneous, so let's remove them

v <- v[1:20]

# these are the most traveled 20 airplanes
v


planeDF <- read.csv("E://study/IT/R/plane-data.csv")


head(planeDF, n=200)

names(v)

subset(planeDF, tailnum %in% names(v))

# Quiz 21

# How many flights did the airplane 
# with tail number N556AS make during 2006 to 2008?
tapply(myDF$Distance, myDF$TailNum, length)["N556AS"]

# Answer: 5071

# 4.16 Changing month numbers to month names

# reality check: there should be this many days during 2006 to 2008

365+365+366

# remember 2008 is a leap year

# handy: we have the month abbreviations stored in R in month.abb
month.abb

# we can use numbers from 1 to 12 as indices into this vector
month.abb[c(9,9,1,2,3,9,12,1,1,1,6,6,7)]

# the first six flights in our data frame myDF are all
# from Jan (2006, but do not see the year information here)
head(month.abb[myDF$Month])

# the last six flights are from Dec (2008, but we do not see it here)
tail(month.abb[myDF$Month])

# here are the dates of flights, in international format
head(paste(myDF$DayofMonth, month.abb[myDF$Month], myDF$Year))

tail(paste(myDF$DayofMonth, month.abb[myDF$Month], myDF$Year))

mydates <- paste(myDF$DayofMonth, month.abb[myDF$Month], myDF$Year)

# we saved a vector more than 21 million dates 
# corresponding to the 21 million flights
length(mydates)

# use tapply
# with the departure delays as the data
# and split the data up according to the value of mydates
# and the function we take (within each day) on the data is the "mean"
# of course we throw away the NA values

tapply(myDF$DepDelay, mydates, mean, na.rm = TRUE)

# these are the days with the smallest average departure delays
head(sort(tapply(myDF$DepDelay, mydates, mean, na.rm = TRUE)))

# these are the days with the longest average departure delays
tail(sort(tapply(myDF$DepDelay, mydates, mean, na.rm = TRUE)))

# here are the worst 20 days in terms of the average departure delays
tail(sort(tapply(myDF$DepDelay, mydates, mean, na.rm = TRUE)), n=20)



# Quiz 22

# Which date during the period 2006-2008 had the most flights?
tail(sort(tapply(myDF$DepDelay, mydates, length)), n=1)

# Answer: 27 July 2007 with 21780 flights in that day

