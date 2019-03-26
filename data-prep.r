
#Loading our two data sets which will need to be joined. We can use fread instead of 
#read.csv becasue it is much faster. 
#we will also convert our data to a tibble data frame to make manipulating it easier
library(data.table)

data_contact <- fread("Data/SalesForce_Contact.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))

#install.packages("tidyverse")
library(tidyverse)

tb_contact <-as_tibble(data_contact)
tb_hire <- as_tibble(data_hire)

#We are only concerned with demographic data
#We can pare down the Contact data set to only usable columns
#Refernce: https://www.datanovia.com/en/lessons/select-data-frame-columns-in-r/
colnames(data_contact)

demographics <- c("Id",
                  "Race__c",
                  "Gender__c",
                  "Service_Branch__c",
                  "Service_Rank__c",
                  "Reserves_National_Guard__c",
                  "Date_of_Service_EntryNew__c",
                  "Date_of_SeparationNew__c",
                  "Foreign_Service__c",
                  "Purple_Heart_Recipient__c",
                  "Bilingual__c",
                  "Discharge_Type__c",
                  "Willing_to_Relocate__c",
                  "Enrolled_in_School__c",
                  "priority_veteran__c",
                  "Hire_Heroes_USA_Confirmed_Hire__c")


tb_contact_demo <- tb_contact %>% select(demographics)
head(tb_contact_demo)

#We would like to look at two data points called "service era" and "length of service" that we will have to synthesize
#First we have to deal with date/times
head(tb_contact_demo$Date_of_Service_EntryNew__c)

tb_contact_demo$Date_of_Service_start <- as.Date(as.POSIXct(tb_contact_demo$Date_of_Service_EntryNew__c, format = "%m/%d/%Y %H:%M"))
tb_contact_demo$Date_of_Service_end <- as.Date(as.POSIXct(tb_contact_demo$Date_of_SeparationNew__c, format = "%m/%d/%Y %H:%M"))
summary(tb_contact_demo$Date_of_Service_start)
summary(tb_contact_demo$Date_of_Service_end)


#Wartime eras according to the VA...
#reference: https://www.benefits.va.gov/pension/wartimeperiod.asp
world_war_1_era_start <- as.Date("1917-04-06")
world_war_1_era_end <- as.Date("1918-11-11")
world_war_2_era_start <- as.Date("1941-12-07")
world_war_2_era_end <- as.Date("1946-12-31")
korean_conflict_era_start <- as.Date("1950-06-27")
korean_conflict_era_end <- as.Date("1955-01-31")
vietnam_era_start <- as.Date("1961-02-28")  
vietnam_era_end <- as.Date("1975-05-07") 
gulf_war_era_start <- as.Date("1990-08-02") 
gulf_war_era_end <- Sys.Date()

tb_contact_demo$Date_of_Service_end

#Calculating service era
tb_contact_demo$last_service_era <- ifelse(tb_contact_demo$Date_of_Service_end > world_war_1_era_start & tb_contact_demo$Date_of_Service_end < world_war_1_era_end, "World_War_1_era",
                                        ifelse(tb_contact_demo$Date_of_Service_end > world_war_2_era_start & tb_contact_demo$Date_of_Service_end < world_war_2_era_end, "World_War_2_era",
                                               ifelse(tb_contact_demo$Date_of_Service_end > korean_conflict_era_start & tb_contact_demo$Date_of_Service_end < korean_conflict_era_end, "korean_conflict_era",
                                                      ifelse(tb_contact_demo$Date_of_Service_end > vietnam_era_start & tb_contact_demo$Date_of_Service_end < vietnam_era_end, "vietnam_era",
                                                             ifelse(tb_contact_demo$Date_of_Service_end > gulf_war_era_start & tb_contact_demo$Date_of_Service_end < gulf_war_era_end, "gulf_war_era",
                                        "peace_time")))))


head(tb_contact_demo$last_service_era)

#Now we need to calculate length of service
#These dates are hand punched.  We will ignore entries that are egregiously wrong. 
tb_contact_demo$Length_of_Service <- ifelse(tb_contact_demo$Date_of_Service_end - tb_contact_demo$Date_of_Service_start < 0, NA, #ignore negative terms
                                        ifelse(tb_contact_demo$Date_of_Service_end - tb_contact_demo$Date_of_Service_start > 21915, NA, #ignore terms over 60 years
                                               tb_contact_demo$Date_of_Service_end - tb_contact_demo$Date_of_Service_start))
summary(tb_contact_demo$Length_of_Service)

#Now we can create length of service bins
min_commitment <- 1461 #in days and assuming 365.25 days per year
length_1 <- min_commitment*1
length_2 <- min_commitment*2
length_3 <- min_commitment*3
length_4 <- min_commitment*4
length_5 <- min_commitment*5
length_6 <- min_commitment*6
length_7 <- min_commitment*7
length_8 <- min_commitment*8
length_9 <- min_commitment*9
length_10 <- min_commitment*10
length_11 <- min_commitment*11
length_12 <- min_commitment*12
length_13 <- min_commitment*13
length_14 <- min_commitment*14
length_15 <- min_commitment*15

#binning our service start dates
tb_contact_demo$Length_of_Service_bin <- as.factor(ifelse(tb_contact_demo$Length_of_Service<=length_1,1,
                                                ifelse(tb_contact_demo$Length_of_Service<=length_2,2,
                                                ifelse(tb_contact_demo$Length_of_Service<=length_3,3,
                                                ifelse(tb_contact_demo$Length_of_Service<=length_4,4,
                                                ifelse(tb_contact_demo$Length_of_Service<=length_5,5,
                                                ifelse(tb_contact_demo$Length_of_Service<=length_6,6,
                                                ifelse(tb_contact_demo$Length_of_Service<=length_7,7,
                                                ifelse(tb_contact_demo$Length_of_Service<=length_8,8,
                                                ifelse(tb_contact_demo$Length_of_Service<=length_9,9,
                                                ifelse(tb_contact_demo$Length_of_Service<=length_10,10,
                                                ifelse(tb_contact_demo$Length_of_Service<=length_11,11,
                                                ifelse(tb_contact_demo$Length_of_Service<=length_12,12,
                                                ifelse(tb_contact_demo$Length_of_Service<=length_13,13,
                                                ifelse(tb_contact_demo$Length_of_Service<=length_14,14,
                                                ifelse(tb_contact_demo$Length_of_Service<=length_15,15,NA))))))))))))))))
summary(tb_contact_demo$Length_of_Service_bin)



#We can now create a final cleaned data set for our analysis. 
colnames(tb_contact_demo)
#eliminating unused columns
cleaned_data <- tb_contact_demo
cleaned_data$Id <- NULL
cleaned_data$Date_of_Service_EntryNew__c <- NULL
cleaned_data$Date_of_SeparationNew__c <- NULL
cleaned_data$Date_of_Service_start <- NULL
cleaned_data$Date_of_Service_end <- NULL
cleaned_data$Length_of_Service <- NULL
cleaned_data$Discharge_Type__c <- NULL
cleaned_data$Race__c <- NULL
cleaned_data$Gender__c <- NULL


colnames(cleaned_data)
#making sure all remaining columns are factors
cleaned_data$Race__c <- as.factor(cleaned_data$Race__c)
cleaned_data$Gender__c <- as.factor(cleaned_data$Gender__c)
cleaned_data$Service_Branch__c <- as.factor(cleaned_data$Service_Branch__c)
cleaned_data$Service_Rank__c <- as.factor(cleaned_data$Service_Rank__c)
cleaned_data$Reserves_National_Guard__c <- as.factor(cleaned_data$Reserves_National_Guard__c)
cleaned_data$Foreign_Service__c <- as.factor(cleaned_data$Foreign_Service__c)
cleaned_data$Purple_Heart_Recipient__c <- as.factor(cleaned_data$Purple_Heart_Recipient__c)
cleaned_data$Bilingual__c <- as.factor(cleaned_data$Bilingual__c)
cleaned_data$Discharge_Type__c <- as.factor(cleaned_data$Discharge_Type__c)
cleaned_data$Hire_Heroes_USA_Confirmed_Hire__c <- as.factor(cleaned_data$Hire_Heroes_USA_Confirmed_Hire__c)
cleaned_data$last_service_era <- as.factor(cleaned_data$last_service_era)
cleaned_data$Length_of_Service_bin <- as.factor(cleaned_data$Length_of_Service_bin)

summary(cleaned_data)

#Narrowing down to only complete cases
complete_data <- cleaned_data[complete.cases(cleaned_data),]

#Checking out the distribution of the target variable
summary(complete_data$Hire_Heroes_USA_Confirmed_Hire__c)

fwrite(complete_data, file = "Data/complete_data1.csv")


