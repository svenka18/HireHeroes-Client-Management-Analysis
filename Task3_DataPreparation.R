
library(stringr)
require(stringr)
library(reshape)
require(reshape2)
require(dplyr)
library(data.table)
require(data.table)
question3 <- read.csv("C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Question3/q3 - data (1).csv", header=T, na.strings=c("","NA"))
dim(question3)
View(question3)
extra <- data.frame(contact$Id,contact$Interview_Skills__c,contact$Responsive__c)
extra$contact.Interview_Skills__c<-ifelse(is.na(extra$contact.Interview_Skills__c) | extra$contact.Interview_Skills__c=="No",0,1)
extra$contact.Responsive__c<-ifelse(is.na(extra$contact.Responsive__c) | extra$contact.Responsive__c=="No",0,1)
View(extra)
dim(extra)
extra$Id <- extra$contact.Id
extra$contact.Id <- NULL
question3 <- merge(question3,extra,by=c("Id"))
View(question3)

y <- dcast(as.data.table(question3)[, strsplit(as.character(Desired_Industry_for_Employment__c), ';', fixed = TRUE), by = .(Id, Desired_Industry_for_Employment__c)], 
           Id ~ V1, fun.aggregate = length)
colnames(y) <- paste("Ind", colnames(y), sep = "_")
colnames(y)[1] <- "Id"
View(y)
dim(y)

x <- dcast(as.data.table(question3)[, strsplit(as.character(Desired_Geographic_Region_of_Employment__c), ';', fixed = TRUE), by = .(Id, Desired_Geographic_Region_of_Employment__c)], 
           Id ~ V1, fun.aggregate = length)
colnames(x) <- paste("Geo", colnames(x), sep = "_")
colnames(x)[1] <- "Id"
View(x)
dim(x)
consolidated <- merge(question3,y,by=c("Id"))
consolidated <- merge(consolidated,x,by=c("Id"))
dim(consolidated)
View(consolidated)
consolidated_data <- consolidated[,-c(35:114)]
names(consolidated)
names(consolidated_data)
View(consolidated_data)

write.csv(consolidated,"C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Question3/consolidated.csv")
write.csv(consolidated_data,"C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Question3/consolidated_data.csv")

#####################################
contact1$Bus_Days_between_Assigned_and_Assessed__c <- as.Date(as.character(contact1$Dat_Initial_Assessment_was_Completed__c), format="%m/%d/%Y")-
  as.Date(as.character(contact1$Date_assigned_to_staff__c), format="%m/%d/%Y")

contact1$Bus_Days_between_Assessment_and_Resume__c <- as.Date(as.character(contact1$Date_Resume_Completed__c), format="%m/%d/%Y")-
  as.Date(as.character(contact1$Dat_Initial_Assessment_was_Completed__c), format="%m/%d/%Y")
View(contact1)

nrow(subset(contact1,Bus_Days_between_Assigned_and_Assessed__c < 0))
contact1$x <- ifelse(contact1$Bus_Days_between_Assigned_and_Assessed__c<0,0,1)
x1<-which(contact1$x == 0)
contact1 <- contact1[-c(x1), ] 

nrow(subset(contact1,Bus_Days_between_Assessment_and_Resume__c < 0))
contact1$y <- ifelse(contact1$Bus_Days_between_Assessment_and_Resume__c<0,0,1)
y1<-which(contact1$y == 0)
contact1 <- contact1[-c(y1), ] 
contact1$x <- NULL;contact1$y <- NULL;contact1$z <- NULL;

dim(contact1)
names(contact1)
View(contact1)
contact3 <- contact1[,-c(1,3,4,5,7,8,9)]
names(contact3)

##################################
final <- merge(contact3,consolidated,by=c("Id"))
View(final)
dim(final)
names(final)
final_data <- final[,-c(8:23,41:120)]
names(final_data)

final_data$a__Gender__c <- ifelse(is.na(final_data$a__Gender__c),"Unknown",final_data$a__Gender__c)

contact_log_df <- data.frame(contact_log$contact_log.Id,contact_log$logged_activities)
View(contact_log_df)
contact_log_df$Id <- contact_log_df$contact_log.contact_log.Id
contact_log_df$contact_log.contact_log.Id <- NULL
dim(contact_log_df)

###################################
final_data <- merge(final_data,contact_log_df,by=c("Id"))
dim(final_data)
names(final_data)
View(final_data)

sapply(final_data, function(x) sum(is.na(x)))
final_data <- final_data[complete.cases(final_data$days_to_get_hired,final_data$Bus_Days_between_Assigned_and_Assessed__c,
                                        final_data$Bus_Days_between_Assessment_and_Resume__c,final_data$Volunteer_Services__c), ]

############################
final_data_hired <- subset(final_data,Blue_c==1)
dim(final_data_hired)
names(final_data_hired)
View(final_data_hired)
final_data_hired_volunteer <- subset(question4,volunteer_c==1)
dim(final_data_hired_volunteer)
View(final_data_hired_volunteer)
sapply(final_data_hired, function(x) sum(is.na(x)))
final_data_hired <- final_data_hired[complete.cases(final_data_hired$days_to_get_hired,final_data_hired$Bus_Days_between_Assigned_and_Assessed__c,
                                                    final_data_hired$Bus_Days_between_Assessment_and_Resume__c), ]

names(final_data_hired)
write.csv(final_data,"C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Question3/final_data.csv")
write.csv(final_data_hired,"C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Question3/final_data_hired.csv")


########################################

industry <- final[,c(1,3,40:119)]
industry <- industry[complete.cases(industry), ]
sapply(industry, function(x) sum(is.na(x))) 
dim(industry)

write.csv(industry,"C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Question3/industry.csv")

glm.fit<-glm(Blue_c ~.,
             family=binomial, data = industry,na.action = na.omit)
summary(glm.fit)


trainIndexc <- createDataPartition(final_data_hired, p = 0.60, 
                                   list = FALSE, 
                                   times = 1)
final_data_hired.training = final_data_hired[trainIndexc,]
final_data_hired.testing = final_data_hired[-trainIndexc,]
dim(final_data_hired.training);dim(final_data_hired.training)

names(final_data.training)
final_reg <- final_data_hired.training[,-c(1,3)]
names(final_reg)
lm.combined <- lm(final_reg$days_to_get_hired~.,data=final_reg, na.action = na.omit)
lm.combined$coefficients[,4]
library(caret)
summary(lm.combined)$coefficients[,4] 

lm.combined <- step(lm(hired_contacts_with_volunteers$days_to_get_hired~hired_contacts_with_volunteers$Volunteer_Services__c,na.action = na.omit)
                    ,direction="both")



