myDF <- read.csv("E://study/IT/R/2008.csv")

head(myDF)
tail(myDF)

head(myDF$Origin)
tail(myDF$Origin)

head(myDF$Dest)
tail(myDF$Dest)

head(myDF$Origin == "IND")
sum(myDF$Origin == "IND")
   # FALSE values converted to 0's
   # TRUE values converted to 1's
   # sum just adds up the total
   # which yields the number of flights departing from Indy in 2008

sum(myDF$Dest == "IND")


   #Test
   #chicago Midway - MDW
sum(myDF$Origin == "MDW")
sum(myDF$Dest == "MDW")

sum(myDF$Origin == "NYC")
   #New York Kennedy - JFK
sum(myDF$Origin == "JFK")
sum(myDF$Dest == "JFK")


   #Quiz 3
   #In 2008, how many flights departed from O¡¯Hare Airport (ORD)?

sum(myDF$Origin == "ORD")
  
   #In 2008, how many flights arrived at O¡¯Hare Airport (ORD)?

sum(myDF$Dest == "ORD")
  
   #How many flights in the data set departed from the Indianapolis Airport (IND) and arrived at the O¡¯Hare Airport (ORD)?

sum(myDF$Origin == "IND" & myDF$Dest =="ORD")
   # Teacher's answer note
   #sum((myDF$Origin == ¡°IND¡±) & (myDF$Dest == ¡°ORD¡±))
sum((myDF$Origin == "IND") & (myDF$Dest == "ORD"))


IndyOrigin <- subset(myDF, myDF$Origin == "IND")

IndyDest <- subset(myDF, myDF$Dest == "IND")


head(IndyOrigin)
head(IndyDest)

head(IndyOrigin$Month)
tail(IndyOrigin$Month)

table(IndyOrigin$Month)
plot(table(IndyOrigin$Month))

table(IndyDest$Month)
plot(table(IndyDest$Month))


   #Quiz 4
   #Make a data frame that consists of the flights in the year 2008 departing from TUP. 
   #Then, answer the following questions.
TUPOrigin <- subset(myDF, myDF$Origin == "TUP")

   #What is the average departure delay of the flights that depart from TUP in 2008?
mean(TUPOrigin$DepDelay)

  #The flights departed 3.8 minutes early, on average, from TUP in 2008.




head(IndyOrigin)

head(IndyOrigin$DepTime < 600)
head(IndyOrigin$DepTime < 1000)

sum(IndyOrigin$DepTime < 600)
    # again, sum up TRUE's AND FALSE's as 1's and 0's resp.
?sum

sum(IndyOrigin$DepTime < 600, na.rm=TRUE)
sum(IndyOrigin$DepTime < 1200, na.rm=TRUE)
sum(IndyOrigin$DepTime < 1800, na.rm=TRUE)
sum(IndyOrigin$DepTime < 2400, na.rm=TRUE)
sum(IndyOrigin$DepTime <= 2400, na.rm=TRUE)


sum(is.na(IndyOrigin$DepTime))
42011 + 739


    # Quiz 5
    # How many flights arrived at LAX in 2008?


sum(myDF$Dest == "LAX")

LAXDest <- subset(myDF, myDF$Dest == "LAX")

    #How many flights departed from ATL and landed at LAX in 2008?
sum((myDF$Origin == "ATL") & (myDF$Dest =="LAx"))
sum(LAXDest$Origin == "ATL")

ATLtoLAX <- subset(myDF, (myDF$Origin == "ATL") & (myDF$Dest == "LAX"))

    #Among the flights from ATL to LAX in 2008, how many departed before 12 noon?
    #?sum
    #SUM(..., na.rm = FALSE)
    #Arguments
    #... or complex or logical vectors.
    #na.rm	logical. Should missing values (including NaN) be removed?

sum(ATLtoLAX$DepTime <= 1200, na.rm=TRUE)

sum(is.na(ATLtoLAX$DepTime))


#Make a plot of departure times, grouped by hour, for the flights from ATL to LAX. 

plot(table(ATLtoLAX$DepTime))
table(ATLtoLAX$DepTime)
head(ATLtoLAX)
?floor
#takes a single numeric argument x and 
#returns a numeric vector containing the largest integers not greater than the corresponding elements of x.
table(floor(ATLtoLAX$DepTime/100))
plot(table(floor(ATLtoLAX$DepTime/100)))
