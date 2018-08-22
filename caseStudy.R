# R Program By Abhijeet Sengupta For Uber Case Study.

#-----------------------------SETUP-------------------------------------------------------------

#install packages 
#install.packages("tidyr")
#install.packages("ggplot2")

#Load the required libraries
library(stringr)
library(tidyr)
library(ggplot2)
library(dplyr)

#Assuming working directory will be set accordingly.
#setwd("D:\\")

#------------------------------------------------------------------------------------------------


#----------------------------DATA LOADING--------------------------------------------------------

#Load the Data from csv file, considered csv file is present in working directory

UberRideData <- read.csv("taxidata.csv", stringsAsFactors = F,header = TRUE,na.strings = "NA")

#Take a glipse of data
View(UberRideData)
str(UberRideData)

#-------------------------------------------------------------------------------------------------


#------------------------------DATA CLEANING------------------------------------------------------

#Analyzing NA Values do we need to take care of them?
sapply(colnames(UberRideData), function(x) length(which(is.na(UberRideData[,x]))))
#There are 3914 NA in Drop.Timestamp column. So no drop times for these NA. One reason could be no trip. Let's confirm
UberRideData  %>% group_by(Status) %>% filter(is.na(Drop.timestamp)) %>% summarize(cnt=length(Request.id))
#Total cancelled trip 1264+ No Cars Avialable 2650 = 3914.
#So it's confirmed these NA is valid and no direct impact on analysis.
UberRideData %>% group_by(Status) %>% filter(is.na(Driver.id))  %>% summarize(cnt=length(Request.id))
#Similarly no of Driver.id which are NA due to There are no cars avialable. Hence driver id is blank.
#Conclusion: Drop.timestamp and Driver.IDs can remain as NA.


#Request.timestamp & Drop.timestamp values are in 2 formats
#Make the time separator simillar and consistent
UberRideData$Request.timestamp <- str_replace_all(UberRideData$Request.timestamp, "[/]",  "-")
UberRideData$Drop.timestamp <- str_replace_all(UberRideData$Drop.timestamp, "[/]",  "-")


# convert timestamp  to datetime oject, used POSIXct for simplicity in filters

UberRideData$Request.timestamp <- as.POSIXct(UberRideData$Request.timestamp, format = "%d-%m-%Y %H:%M")
UberRideData$Drop.timestamp <- as.POSIXct(UberRideData$Drop.timestamp, format = "%d-%m-%Y %H:%M")



#Take a glipse of data, Take the data for ppt presentation
View(UberRideData)
#write.csv(UberRideData,"uberProcessedData.csv",na="")
#-----------------------------------------------------------------------------------------------------------------

#-------------------------------------Data Analysis--------------------------------------------------------------

#------------Derived Metric:
#By glancing in data it's very much clear that the entire anlaysis can be done by consedering
#Day and hours part from timestamps. Hence derive 4 new metrics Req.Day, Req.Hrs, Drop.Day & Drop.Hrs 

UberRideData$Request.Day <- format(UberRideData$Request.timestamp, "%d")
UberRideData$Request.Hrs <- format(UberRideData$Request.timestamp, "%H")
UberRideData$Drop.Day <- format(UberRideData$Drop.timestamp, "%d")
UberRideData$Drop.Hrs <- format(UberRideData$Drop.timestamp, "%H")


#converting day and hrs column to numeric
UberRideData$Request.Day <- as.numeric(UberRideData$Request.Day);
UberRideData$Request.Hrs <- as.numeric(UberRideData$Request.Hrs);


#--------UNIVARIATE ANALYSIS:

#Analyze all columns and find Area of our interest where we can deep dive to find the RCA

#Request.id, This is trip number and assuming it should be unique, if it's not unique
#Then there is some duplicate rows and need to be attended.
length(UberRideData$Request.id)-length(unique(UberRideData$Request.id))
#Above expression returns 0, which means all id's are unique. 

#Pickup.point, This column is not as expected.

#Status, it's Discrete in nature and type of char. good enough but need to be cosidered as factor for analysis.

UberRideData %>% group_by(Status) %>% summarize(CountOfRequest=length(Request.id)) %>% mutate(perc=(CountOfRequest/sum(CountOfRequest))*100)
####
#Status            CountOfRequest  perc
#<chr>                      <int> <dbl>
#  1 Cancelled                   1264  18.7
#2 No Cars Available           2650  39.3
#3 Trip Completed              2831  42.0
####

###########Looks like "No Cars Available" is a major issue and need to be analyzed. This is almost equal to number of
#trip completed. this issue must be impacting Uber revenue. Similarlly "Cancelled" also need to be analyzed as this is also high.

#-----------------------------------------------------------------------------------------------------------------

#-------------------------------------ANALYSIS FOR THE PROBLEM---------------------------------------------------


#Lets get a plot which shows the distribution of "No Cars Avialable" and "Cancelled"
#Excluding "Trip Completed" for now.
dataWithoutTripC<- subset(UberRideData, UberRideData$Status != "Trip Completed")
#converting day and hrs column to numeric
dataWithoutTripC$Request.Day <- as.numeric(dataWithoutTripC$Request.Day);
dataWithoutTripC$Request.Hrs <- as.numeric(dataWithoutTripC$Request.Hrs);

p1 <- ggplot(dataWithoutTripC, aes(x = as.factor(dataWithoutTripC$Request.Hrs),fill = Status))+geom_bar()
#add labels to bar plot using geom_text()
p1 <- p1 +geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))

#chosing this color pallate to keep identical color scheme in R and tableau plots
p1PlotColor1<-c("#ff0000")
p1PlotColor2<-c("#76b7b2")
p1PlotColor3<-c("#00ff00")
#Add lables to plot & #Fill the color pallete
p1<-p1+scale_fill_manual("legend",values=c("Cancelled"=p1PlotColor1,"No Cars Available"=p1PlotColor2))+ labs(x = "Hours of the day",y="No Of Request Made") 

p1


#With this above plot,  it's clear, Mostly all cancelations are during morning 4-10AM and
#scarcity of cabs during evening hours between 5-10PM.  Is it same for everyday? 

p2<-ggplot(dataWithoutTripC, aes(x = as.factor(Request.Hrs),fill = Status))+geom_bar()
p2 <- p2 + facet_wrap(~Request.Day, ncol=1)+geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
p2<-p2+labs(x = "Hours of the day",y="No Of Request Made") 
p2<-p2+scale_fill_manual("legend",values=c("Cancelled"=p1PlotColor1,"No Cars Available"=p1PlotColor2))
p2

#with above plot, Looks like same for every day.
#Cancellation and No cabs pattern is same. Is it depends on pickup point?

p3<-ggplot(UberRideData, aes(x = as.factor(Request.Hrs),fill = Status))+geom_bar()
p3<-p3+facet_wrap(~Pickup.point,ncol=1)+geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
p3<-p3+scale_fill_manual("legend",values=c("Cancelled"=p1PlotColor1,"No Cars Available"=p1PlotColor2,"Trip Completed"=p1PlotColor3))
p3<-p3+labs(x = "Hours of the day",y="No Of Request Made") 
p3


#with above plot, Cab drivers are cancelling more City to Airport trips during Morning hours.
#Huge shortage of cabs in the evening from Airport to City 


#So we have proper identification of time frame when and where it's hapenning.
#Let's bin the timeframe for better understanding.


#From Above plots it's clear to bucket time into 5 distinct categories for further analysis

TimeBin <-c("Early Morning","Morning Peak Hours","Day Time","Evening Peak Hours","Late Night")

UberRideData$timeSlot = ifelse(UberRideData$Request.Hrs < 5, TimeBin[1],
                               ifelse(UberRideData$Request.Hrs < 10,TimeBin[2],
                                      ifelse(UberRideData$Request.Hrs < 17,TimeBin[3],
                                             ifelse(UberRideData$Request.Hrs < 22,TimeBin[4],TimeBin[5]))))


p4<-ggplot(UberRideData, aes(x = as.factor(timeSlot),fill = Status))+geom_bar()
p4<-p4+geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
p4<-p4+scale_fill_manual("legend",values=c("Cancelled"=p1PlotColor1,"No Cars Available"=p1PlotColor2,"Trip Completed"=p1PlotColor3))
p4<-p4+labs(x = "Time Slot",y="No Of Request Made") 
p4<-p4+scale_x_discrete(limits=TimeBin)
p4

#from above plot, there two major issues clearly,
#1) Cancellation during morning peak hours
#2) Unavailability of cabs during evening peak hours. 


#From the plots it's easily shown there is a gap between supply and demand. 
#Let's do a gap analysis by ploating it.


UberRideData_grpBy <- UberRideData %>%   group_by(timeSlot,Status,Pickup.point) %>% 
  summarize(cnt=length(Request.id))

#ABove data frame is in long format, let's convert it into Wide format.

UberRideData_grpBy_spread <- data.frame(spread(UberRideData_grpBy,key=Status,value=cnt))

#Reshaping wide format to long format

UberRideData_demand_supply <- gather((UberRideData_grpBy_spread %>% 
                                           mutate(demand=Cancelled+No.Cars.Available+Trip.Completed,supply=Trip.Completed+Cancelled,gap=((Cancelled+No.Cars.Available+Trip.Completed)-(Trip.Completed+Cancelled))) %>% 
                                           select(timeSlot,Pickup.point,demand,supply,gap)),
                                        key="demand.supply.gap",values=c(3,4,5))



#Plot the Demand, supply & Gap 

p5 <- ggplot(data = UberRideData_demand_supply) +
  geom_col(aes(x=as.character(timeSlot),y=value,fill=demand.supply.gap),position=position_dodge(width=0.8)) +
  facet_wrap(~Pickup.point,ncol=1) +
  labs(title="Demand and Supply",x="Time Slot",y="No. of Requests",fill="Demand/Supply/Gap")+
  scale_x_discrete(limits=TimeBin)


p5

#This plot says there is always a positive GAP(High demand less supply).
#Gap is the most severe at Airport during Evening Peak hours. Gap is moderate at city during Morning peak hours.

#it's clear root cause is different in city and Airport

########################################Observations for Trip starting @  City

#For morning peak hours, the volume of request is very high.
#This High request is not being converted to a business leading to a loss of potential revenue.
#Morning peak hours is the main segment of time where Uber need to concentrate.
#Supply is adequate in the city but still morning peak hours need to be under scanner.
#The Major issue in city is Cancellation of cab and it's very high during morning peak hours.
#Why cab drivers are cancelling the request is major area of investigation.
#One reason could be low demand in airport during morning hours. 
#This low demand at airport during morning time leads to high waiting time of cab drivers to get next trip. So cab drivers are rejecting requests for city to airport trips.

########################################Recommendations for Trip starting @ City
#Uber can rectify this low demand at airport by reducing price cuts for passengers, easy cab booking methods. 
#Cab driver can be incentivized  during morning hours by providing bonus, by setting a wait time threshold,
 #beyond that time Uber can pay gas cost to drivers to come back to city without a ride.
#One solution could be Uber tie up with Airport services to bring back airport staffs from airport to city. This program can drastically reduce the low demand at airport. 

########################################Observations for Trip starting @  Airport
#Airport has more severe supply and demand gap as compared to city.
#During evening peak hours there is huge request but Uber loosing the business due to non availability of cabs.
#This high positive gap(more demand) is major area of investigation.
#One cause, is very less incoming cabs from city to airport during day and evening time, this leads to low availability of cabs at airport.
#Also plots shows, there is nice demand during day and evening time at city. Which keeps the cab drivers busy at city.  
#Cancellation is not a major issue at airport.


########################################Recommendations for Trip starting @  Airport
#Drivers can be given a bonus to complete a trip from Airport to city at pick hours.
#Offer day time cost benefit to customer for city to airport trips so more cabs can be accumulated in airport before evening peak hours.
#Give cash incentives to riders and cab drivers for share rides from airport to city in evening hours.
#Uber can pay fuel cost to drivers who comes to airport without any passenger at evening hours.
#Uber should look into a long term solutions to increase more cabs into its fleet. This will solve it's supply and demand issues.


#----------------------------------END---------------------------------------------------------------------------















