rm(list=ls())
#Load and view all data (times, focal events, location)
library(readr)
library(dplyr)
#load squirrel data and behaviors from library

squirreldata <- read_csv("ringtail/data/squirreldata.csv", 
                         col_types = cols(time = col_time(format = "%H:%M:%S")))

Behaviors <- read_csv("ringtail/data/Behaviors.csv")

#Add duration
behavior_data<- squirreldata %>% mutate(endtime= lead(time), 
         duration= endtime-time, #removal of unnecessary variables below
         people_pass=NULL,
         human_squirrel_engage=NULL,
         conspecific_interaction=NULL,
         trail=NULL,
         photo=NULL,
         contact=NULL,
         notes=NULL,
         X13=NULL,
         food_item=NULL) 

#Categorize behaviors
categorized_data<-Behaviors%>%left_join(behavior_data) 
#this also removes all of the no squirrel events which have no listed behaviors to categorize
#Remove the description column
categorized_data<-categorized_data %>% mutate(description=NULL)
#Remove oos and end events
oos_end<-as.data.frame(subset(categorized_data, categorized_data$behavior=="oos"| 
                         categorized_data$behavior=="end"|
                           categorized_data$time=="no squirrel"))
categorized_data<-anti_join(categorized_data,oos_end)

summarized_data<- data.frame(m<-matrix(ncol=6,nrow=1))
colnames(summarized_data)<-c('activity','activity_proportion','activityduration','focal_event','focalduration','site')
listFocalIDs <- unique(categorized_data$focal_event)
for (i in 1:length(listFocalIDs)) {
focal<- subset(categorized_data, focal_event==listFocalIDs[i]) #selects individual events
focal <- focal[order(focal$time),]
  focal$duration <-as.numeric(focal$duration) #change to numeric from time/seconds
  focal$focalduration<-sum(focal$duration, na.rm = TRUE) #create column with focal duration
  focal$activity_proportion<- focal$duration/focal$focalduration #temp indicates these numbers will be replaced
  focal$activity_proportion<-as.numeric(focal$activity_proportion)
  focal_event <- focal$focal_event
  activity <- focal$category
  activity_proportion<-focal$activity_proportion
  site <- focal$site
  focalduration <- focal$focalduration
  activityduration <-focal$duration
  summarizedRow<-cbind(activity,activity_proportion, activityduration,focal_event,focalduration,site)
  summarized_data<-rbind(summarized_data, summarizedRow)
}

summarized_data<-summarized_data[-c(1),] #removes extra row created in matrix
summarized_data$activity_proportion<-as.double(summarized_data$activity_proportion)#corrects data type error from previous 
summarized_data$activityduration<-as.double(summarized_data$activityduration)

#check for negative proportions or activity values
subsetNegs1 <- subset(summarized_data, summarized_data$activityduration < 0 )
subsetNegs2 <- subset(summarized_data, summarized_data$activity_proportion < 0 )
subsetNegs3<-  subset(summarized_data, summarized_data$focalduration< 0 )

#correct these errors
summarized_data$activityduration <- ifelse(summarized_data$activityduration < 0, 0, summarized_data$activityduration) #make duration of activity 0 if it's negative
summarized_data$activity_proportion <- ifelse(summarized_data$activity_proportion < 0, 0, summarized_data$activity_proportion)  #make proportion 0 because we already created proportion #s before we made duration 0
summarized_data$focalduration<-ifelse(summarized_data$focalduration < 0, 0, summarized_data$focalduration)

#use this to check if above lines worked:
sum(summarized_data$activity_proportion < 0  ) #should be 0
sum(summarized_data$focalduration < 0  ) #should be 0
sum(summarized_data$activityduration < 0  ) #should be 0

#check data from one event (A_04_31J19)
checkloop <- subset(summarized_data, focal_event=="A_04_31J19") #subset
checkloop2 <-aggregate(x=checkloop$activity_proportion,
                       by=list(checkloop$activity),
                       FUN=sum)
sum(checkloop2$x) #if this is '1' then the proportions are working

#now add all activities together
sumofActivityProps <- aggregate(x=summarized_data$activity_proportion,
                                by=list(summarized_data$activity, summarized_data$focal_event), #sum all all activity proportions by individual follow
                                FUN=sum) 
names(sumofActivityProps)[names(sumofActivityProps) == "Group.1"] <- "Activity" #rename column
names(sumofActivityProps)[names(sumofActivityProps) == "Group.2"] <- "focal_event" #rename column
names(sumofActivityProps)[names(sumofActivityProps) == "x"] <- "activity_proportion" #rename column

#check to see if it works:
check2 <- subset(sumofActivityProps, focal_event=="A_01_07A19") #subset
sum(check2$activity_proportion) #does this equal 1?


#add all activities for one event together
totals <- summarize(group_by(sumofActivityProps, focal_event),proportionsum=sum(activity_proportion))




#import site assignment data for merge
site_assignment <- read_csv("ringtail/data/site_assignment.csv")

activityprops_location<-sumofActivityProps%>%left_join(site_assignment)


#average activity by site
activityprops_bylocation<-aggregate(activityprops_location$activity_proportion, by=list(activityprops_location$Activity,activityprops_location$location), mean)

locationtotals<-summarize(group_by(activityprops_bylocation, location), proportionsum=sum(activity_proportion))

#rename columns
names(activityprops_bylocation)[names(activityprops_bylocation) == "x"] <- "activity_proportion" 
names(activityprops_bylocation)[names(activityprops_bylocation) == "Group.1"] <- "activity"
names(activityprops_bylocation)[names(activityprops_bylocation) == "Group.2"] <- "location"

library(ggplot2)

plotactivity_bylocation<-ggplot() + geom_bar(aes(y = activity_proportion, x = location, fill = activity), data=activityprops_bylocation,
                                          stat="identity") + labs(y="Proportion of Time Spent", x="Site")
ggplot() + geom_bar(aes(y = activity_proportion, x = location, fill = activity), data=activityprops_bylocation,
                    stat="identity") + labs(y="Proportion of Time Spent", x="Site")




