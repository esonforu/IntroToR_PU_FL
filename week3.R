
# Week 3

# 3.2 Identifying the most popular airports

# Most popular departure airports

sort(table(myDF$Origin))
?sort

# Look at the origin airports in order
# from most popular to least popular
# by using the decreasing=T parameter when we sort
sort(table(myDF$Origin), decreasing = T)
# the second popular airport ORD
sort(table(myDF$Origin), decreasing = T)[2]
# the third popular airport DFW
sort(table(myDF$Origin), decreasing = T)[3]

1:10
sort(table(myDF$Origin), decreasing = T)[1:10]
# These are  the most popular ten airports,
# according to the number of origins of flights

# Same, for the most popular ten arrival (destination) airports
sort(table(myDF$Dest), decreasing = T)[1:10]

# Remember that we have about 7 million flights altogether in 2008
dim(myDF)
?dim
# dim() Dimension of an object
# Retrieve or set the dimension of an object.
# Usage:
# dim(x)
# dim(x) <- value

# These are the names of the 10 most popular airports in 2008
mostpopular <- names(sort(table(myDF$Dest), decreasing = T)[1:10])

mostpopular

# %in% use to check each element on the left, 
# see if it is a element on the right
# return TRUE or FALSE
# we check each flight to see weather its origin was
# one of these 10 most popular airports

# 2.3 million of the 7 million flights had their origin
# in one of these popular cities
sum(myDF$Origin %in% mostpopular)

# same concept, for the destinations
sum(myDF$Dest %in% mostpopular)

# Find flights for which the origin and the destination
# were among the 10 most popular airports
sum(myDF$Origin %in% mostpopular & myDF$Dest %in% mostpopular)

# Half a million of the flights had origin and destination
# at one of the 10 most popular airports.

0.51/14.4


# 3.3 What is your favorite Airport?
# Why is it your favorite? 

# ORD (Chicago, IL - O'Hare)
# I've never been to american, but my friend lives in Chicago

# How many flights departed from your favorite airport in 2008?
# Do you find this number surprising? 

sum(myDF$Origin =="ORD")
# answer: 350380

# same ORD as destination in 2008 (350452)

sum(myDF$Dest =="ORD")

# How does the number compare to the most popular airport?

# Answer: refer to 3.2
# As ORD is the second popular airpot in 2008,
# compared to the most popular ATL,
# the difference is around 60000 as origin/destination

# A little bit more about ORD in 2008

# what is the monthly flight volume in ORD?

# ORD as origin

# Just double-checking that the vectors we are working with
# have the same length.
length(myDF$Origin[myDF$Origin=="ORD"])
length(myDF$Month[myDF$Origin=="ORD"])
# ORD monthly departure volume in table
table(tapply(myDF$Origin[myDF$Origin=="ORD"],
             myDF$Month[myDF$Origin=="ORD"], na.rm=TRUE))
# Plot the table
plot(table(tapply(myDF$Origin[myDF$Origin=="ORD"],
                  myDF$Month[myDF$Origin=="ORD"], na.rm=TRUE)))

sort(table(tapply(myDF$Origin[myDF$Origin=="ORD"],
                  myDF$Month[myDF$Origin=="ORD"], na.rm=TRUE)))
# The flight volume in ORD may be influenced by weather
# As the volume peak at summer months and bottom at winter months

# ORD as an origin, dig sth about flight cancel
# cancelled - 0 stands for normal, 1 stands for cancelled


# monthly cancelled flights in table
# method without subset
table(tapply(myDF$Cancelled[myDF$Origin=="ORD" & myDF$Cancelled=="1"],
                  myDF$Month[myDF$Origin=="ORD" & myDF$Cancelled=="1"],
             na.rm=TRUE))

sort(table(tapply(myDF$Cancelled[myDF$Origin=="ORD" & myDF$Cancelled=="1"],
                  myDF$Month[myDF$Origin=="ORD" & myDF$Cancelled=="1"],
                  na.rm=TRUE)))

plot(table(tapply(myDF$Cancelled[myDF$Origin=="ORD" & myDF$Cancelled=="1"],
                  myDF$Month[myDF$Origin=="ORD" & myDF$Cancelled=="1"],
                  na.rm=TRUE)))

# make a dataframe of all cancelled flights where ORD is origin
ORDCancelled <- subset(myDF, myDF$Origin=="ORD" & myDF$Cancelled=="1")

# monthly cancelled flights in table
table(tapply(ORDCancelled$Cancelled, ORDCancelled$Month, na.rm=TRUE))

sort(table(tapply(ORDCancelled$Cancelled, ORDCancelled$Month, na.rm=TRUE)))

# cancel reason in table
# A Carrier
# B Weather
# C NAS
# D Security

# The result is group by 2,3,4.. but not A,B,C,D check back later
table(tapply(ORDCancelled$Cancelled, 
             ORDCancelled$CancellationCode, na.rm=TRUE))

# This works as expected
table(ORDCancelled$CancellationCode)

# We can see from the table that 
# no flights cancelled due to security problem

# ORD as destination

# Just double-checking that the vectors we are working with
# have the same length.
# method 1, check the length(return number)
length(myDF$Dest[myDF$Dest=="ORD"])
length(myDF$Month[myDF$Dest=="ORD"])
# method 2, check if the lenghts are equal (return TRUE/FALSE)

length(myDF$Dest[myDF$Dest=="ORD"]) == length(myDF$Month[myDF$Dest=="ORD"])

# ORD monthly arrival volume in table
table(tapply(myDF$Dest[myDF$Dest=="ORD"],
             myDF$Month[myDF$Dest=="ORD"], na.rm=TRUE))
# Plot the table
plot(table(tapply(myDF$Dest[myDF$Dest=="ORD"],
                  myDF$Month[myDF$Dest=="ORD"], na.rm=TRUE)))

sort(table(tapply(myDF$Dest[myDF$Dest=="ORD"],
                  myDF$Month[myDF$Dest=="ORD"], na.rm=TRUE)))


# Quiz 11

# We observed that 2363257 flights departed from the ten most popular airports.
# For the purposes of this question, 
# treat the group of the 200 least popular airports 
# according to the number of flights having these as the origins.

# How many flights had one of these 200 least popular airports as their origin?

1:200
sort(table(myDF$Origin))[1:200]
names(sort(table(myDF$Origin))[1:200])

leastpopular200 <- names(sort(table(myDF$Origin))[1:200])

sum(myDF$Origin %in% leastpopular200)
# Answer:551776

# 3.5 Tools for Verifying

# review: Here is the vector of the ten most popular airports
mostpopular

myDF$Origin[1:100]
(myDF$Origin %in% mostpopular)[1:100]

# It is good to frequently check things like:

dim(myDF) # the dimension of a  data frame

length(mostpopular) # length of a vector

class(mostpopular)  # class of a vector


#3.6 Using airport codes as indices to tables

# Here is the table of how many flights originate at each airport

table(myDF$Origin)

# We can index a vector according to the names of the
# element in the vector
table(myDF$Origin)["IND"]

# We can manually type the names of elements we want to extract
table(myDF$Origin)[c("IND", "ORD")]

table(myDF$Origin)[c("IND", "ORD", "JFK", "EWR", "IAD")]

# or we can save the indices of elements we want to extract
# into a vector, such as mostpopular:
mostpopular

# and use that whole vector as a set of indices into another vector
table(myDF$Origin)[mostpopular]

# we can do this on the fly as well, for instance:
myairports <- c("IND", "ORD", "JFK", "EWR", "IAD")
# and use this as an index into the table of counts
table(myDF$Origin)[myairports]

# Quiz 12

# How many flights landed at Ronald Reagan Washington National 
# or Washington Dulles Airport in 2008? 
# Use just one command to get both of these counts simultaneously.
table(myDF$Dest)[c("DCA", "IAD")]

# Answer: DCA had 86671 and IAD had 76022



# 3.8 Analyzing the percentage of flights that departed on-time and early

# Here are two ways to check the first 20 flights
# and see which ones departed on time (or early)
head(myDF$DepDelay <= 0, n =20)

(myDF$DepDelay <= 0)[1:20]

# This tells us how many flights at each airport
# departed on time or early
tapply(myDF$DepDelay <= 0, myDF$Origin, sum, na.rm=T)

# This is the total number of flights 
# that departed from each airport
table(myDF$Origin)

# we restrict attention to only the 10 most popular airports
tapply(myDF$DepDelay <= 0, myDF$Origin, sum, na.rm=T)[mostpopular]
table(myDF$Origin)[mostpopular]


# We divide each element in the first vector
# by the analogous element in the second vector
# This gives us basically 10 divisions in a row
tapply(myDF$DepDelay <= 0, myDF$Origin, sum, na.rm=T)[mostpopular]/
table(myDF$Origin)[mostpopular]
# and as a result, we know the percentage of the flights
# at each of the 10 most popular airports
# that departed on time or early

# Double check the result for Atlanta ATL (the result looks right)

233718/414513


# Quiz 13

# What percentage of flights departed from IND on time or early?

tapply(myDF$DepDelay <= 0, myDF$Origin, sum, na.rm=T)["IND"]/
  table(myDF$Origin)["IND"]



# 3.10 Revisiting a plot in R

# We had already analyzed (in an earlier session)
# how many flights occur altogether, during each hour of the day

table(cut(myDF$DepTime, breaks=seq(0, 2400, by=100)))

v <- table(cut(myDF$DepTime, breaks=seq(0, 2400, by=100)))

# Here is another way to do that, by dividing each 4-digit time
# by 100, and then rounding the resulting fraction up to the
# next cloest integer
table(ceiling(myDF$DepTime/100))

w <- table(ceiling(myDF$DepTime/100))

# all of the results from the two methods agree, none disagree
sum(v != w) # Disagree 0
sum(v == w) # Agree 24 all 24 hours are same in both methods

# Here is the analogous plot
# and the x-axis looks better than it did the first time
# that we visited this question
plot(w)


# 3.11 Analyzing flights by origin airport and month of departure


# We can break the data in the DepDelay vector
# according to which city of origin or according to the month
tapply(myDF$DepDelay, myDF$Origin, length)

tapply(myDF$DepDelay, myDF$Month, length)

# We now know how many flights occur from each airport in each month

tapply(myDF$DepDelay, list(myDF$Origin, myDF$Month), length)

# We can extract the data from a particular row (i.e., origin airport)
# and from a particular column (i.e., the month)
tapply(myDF$DepDelay, list(myDF$Origin, myDF$Month), length)["IND", 6]
tapply(myDF$DepDelay, list(myDF$Origin, myDF$Month), length)["ATL", 3]

# notice that we need to give 2 dimensions when we extract data from a matrix
# we need to specify both the row and the column

# Here is the number of flights from 3 particular airports
# during the months 7,8,9,10(July through October)
tapply(myDF$DepDelay, list(myDF$Origin, myDF$Month), 
       length)[c("ATL","AUS","BDL"), c(7,8,9,10)]

# Same effect, just writing 7:10 to get the vector c(7,8.9,10)
tapply(myDF$DepDelay, list(myDF$Origin, myDF$Month), 
       length)[c("ATL","AUS","BDL"), 7:10]

# Quiz 14

# How many flights departed altogether from ATL, AUS, and BDL
# during the months of July 2008 through October 2008?

sum(tapply(myDF$DepDelay, list(myDF$Origin, myDF$Month), 
           length)[c("ATL","AUS","BDL"), 7:10])

# Answer: the total number is  165782



# 3.13 Leaving a specification blank


# Here are all the flights, month-by-month, from IND airport
tapply(myDF$DepDelay, list(myDF$Origin, myDF$Month), length)["IND", 1:12]

# same effect, just leave the column specification blank
# but make sure to put a comma between the rows and the columns

tapply(myDF$DepDelay, list(myDF$Origin, myDF$Month), length)["IND", ]


# What about Chicago O'Hare?

tapply(myDF$DepDelay, list(myDF$Origin, myDF$Month), length)["ORD", ]

# what about both IND and ORD at once?

tapply(myDF$DepDelay, list(myDF$Origin, myDF$Month), length)[c("IND","ORD"), ]


# We can check the result is a matrix

class(tapply(myDF$DepDelay, list(myDF$Origin, myDF$Month), 
             length)[c("IND","ORD"), ])


# How big is the matrix? should be 2-by-12

dim(tapply(myDF$DepDelay, list(myDF$Origin, myDF$Month), 
             length)[c("IND","ORD"), ])


# Question for 3.13

# Can you create a table with 3 rows and 12 columns,
# showing the monthly counts for flights departing from ATL, ORD, and DFW?

# make a table
table(list(myDF$Origin, myDF$Month))[c("ATL", "ORD", "DFW"), ]
# and check the class is a table
class(table(list(myDF$Origin, myDF$Month))[c("ATL", "ORD", "DFW"), ])

# same effect here, but the table looks better
table(myDF$Origin, myDF$Month)[c("ATL", "ORD", "DFW"), ]

class(table(myDF$Origin, myDF$Month)[c("ATL", "ORD", "DFW"), ])


# 3.14 Caculating percentage of flights with long delays

# We make a data frame with all of the flights that are
# delayed more than 30 minutes when departing

longdelyDF <- subset(myDF, myDF$DepDelay > 30)

# There are over 800000 such flights

dim(longdelyDF)

# double-check

head(longdelyDF$DepDelay)


# The counts of all of the flights, from ORD or IND, according to month

tapply(myDF$DepDelay, list(myDF$Origin, myDF$Month), length)[c("IND","ORD"), ]

# same thing, but now for the flight with delays more than 30 mins

tapply(longdelyDF$DepDelay, list(longdelyDF$Origin, longdelyDF$Month), 
       length)[c("IND","ORD"), ]

# We can divide entry by entry, to get the percentage of flights
# that have really long delays(more than 30 mins) from IND or ORD
# broken up, month by month

M1 <- tapply(longdelyDF$DepDelay, list(longdelyDF$Origin, longdelyDF$Month), 
       length)[c("IND","ORD"), ]


M2 <- tapply(myDF$DepDelay, list(myDF$Origin, myDF$Month), length)[c("IND","ORD"), ]

# here is the division, it yields the percentage of flights with long delays
M1/M2

# We can plot this with a dotchart
?dotchart

# Recommended books for R graphic

# Cleveland, W. S. (1985) The Elements of Graphing Data. Monterey, CA: Wadsworth.
# Murrell, P. (2005) R Graphics. Chapman & Hall/CRC Press.

dotchart(M1/M2)

# M3 has the percentage of flights with long delays from IND or ORD, month-by-month
M3 <- M1/M2

dotchart(M3)
# There are more delays(in percentage) in large airport like ORD 
# than in small airport like IND
M3["ORD", ] - M3["IND", ] > 0


# Quiz 15

# Q1 How many flights departed altogether from IND or ORD in 2008
# with a delay of more than 30 minutes each?

sum(M1)

# Q2 In which month of 2008 was the percentage of long delays 
# (i.e., flights with more than 30 minute delays) the highest?

M4 <- tapply(longdelyDF$DepDelay, longdelyDF$Month, length)


M5 <- tapply(myDF$DepDelay, myDF$Month, length)

M6 <- M4/M5

sort(M6)

plot(M6)

# Answer:December


# 3.16 Analyzing the flights by time of day for departure

?rep
# rep replicates the values in x. 
# It is a generic function

# Break the day into 4 parts
# early morning (1) corresponds to times midnight to 6 AM
# late morning (2) corresponds to times 6 AM to 12 noon
# early eveving (3) corresponds to times 12 noon to 6 PM
# late evening (4) corresponds to times 6 PM to 12 midnight

head(ceiling(myDF$DepTime/600))

v1 <- ceiling(myDF$DepTime/600)

dim(myDF)

dim(myDF)[1]
# we assume by default, the part of day is unknown (NA)

# bulid a vector called parts of day
# initially we put 7009728 NA values inside
partofday <- rep(NA, times=dim(myDF)[1])

head(partofday)

length(partofday)

length(v1)

partofday[v1 == 1] <- "early morning"
partofday[v1 == 2] <- "late morning"
partofday[v1 == 3] <- "early evening"
partofday[v1 == 4] <- "late evening"

table(partofday)

head(partofday)
# double-check that the length of partofday vector is the same 
# as the number of rows in the data frame myDF
length(partofday)
dim(myDF)

# and then we can create a new column in the myDF data frame called "timeofday"
# and we can store this information we just found into this column
myDF$timeofday <- partofday

# now our data frame myDF has 30 columns instead of 29 columns
dim(myDF)

# Just check to make sure that the first 6 flights were done properly
head(myDF$timeofday)
head(myDF$DepTime)

tail(myDF$timeofday)
tail(myDF$DepTime)


# Quiz 16

# How many flights departed from IND early in the morning?

# to get the number of all 4 possible result of IND
tapply(myDF$Origin,list(myDF$Origin, myDF$timeofday), length)["IND", ]

# restrict attention to early morning flights in IND
tapply(myDF$Origin,list(myDF$Origin, myDF$timeofday), 
       length)["IND","early morning" ]

# Answer: There were 755 flights departed from IND early in the morning


# 3.18 Analyzing flights by time of day for departure and origin airport

# We can tabulate how many flights occur, by splitting the flights according to 
# both the city of origin and also the time of day when the flight departed

tapply(myDF$DepDelay,list(myDF$Origin, myDF$timeofday), length)

# We get matrix with all of the results

class(tapply(myDF$DepDelay,list(myDF$Origin, myDF$timeofday), length))
dim(tapply(myDF$DepDelay,list(myDF$Origin, myDF$timeofday), length))

# We have a matrix with 303 rows (one row per city)
# and 4 columns (one column per time of day)

# we can restrict attention to the flights that departed from IND, CVG, or JFK

tapply(myDF$DepDelay,list(myDF$Origin, myDF$timeofday), 
       length)[c("IND","CVG","JFK"), ]

# Here we did not specify the columns, 
# so as a result, we get all 4 possible columns








