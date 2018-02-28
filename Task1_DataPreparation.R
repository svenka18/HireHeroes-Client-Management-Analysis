contact_log <- read.csv("C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Contact.csv", header=T, na.strings=c("","NA"))

dim(contact_log)
contact_log <- data.frame(contact_log$Confirmed_Hired_Date__c,contact_log$Id,contact_log$Dat_Initial_Assessment_was_Completed__c,contact_log$Date_assigned_to_staff__c,
                          contact_log$Date_Resume_Completed__c,contact_log$Date_turned_green__c,
                          contact_log$Date_Turned_Blue__c,contact_log$Attended_Day_1__c,contact_log$Attended_Day_2__c,contact_log$Compass_Participant__c,
                          contact_log$Career_Counseling_Sessions_Number_given__c,contact_log$Date_Attended_HHUSA_Workshop__c,contact_log$Career_Opportunity_Day_Participant__c,contact_log$Created_LinkedIn_account__c)
View(contact_log)
sapply(contact_log, function(x) sum(is.na(x))) 

contact_log$Bus_Days_between_Assigned_and_Assessed__c <- as.Date(as.character(contact_log$contact_log.Dat_Initial_Assessment_was_Completed__c), format="%m/%d/%Y")-
  as.Date(as.character(contact_log$contact_log.Date_assigned_to_staff__c), format="%m/%d/%Y")


contact_log$Bus_Days_between_Assessment_and_Resume__c <- as.Date(as.character(contact_log$contact_log.Date_Resume_Completed__c), format="%m/%d/%Y")-
  as.Date(as.character(contact_log$contact_log.Dat_Initial_Assessment_was_Completed__c), format="%m/%d/%Y")

nrow(subset(contact_log,Bus_Days_between_Assigned_and_Assessed__c>0 & Bus_Days_between_Assessment_and_Resume__c>0 & days_to_get_hired>0))

nrow(subset(contact_log,Bus_Days_between_Assigned_and_Assessed__c < 0))
contact_log$x <- ifelse(contact_log$Bus_Days_between_Assigned_and_Assessed__c< 0,0,1)
x1<-which(contact_log$x == 0)
contact_log <- contact_log[-c(x1), ] 

nrow(subset(contact_log,Bus_Days_between_Assessment_and_Resume__c < 0))
contact_log$y <- ifelse(contact_log$Bus_Days_between_Assessment_and_Resume__c<0,0,1)
y1<-which(contact_log$y == 0)
contact_log <- contact_log[-c(y1), ] 

contact_log$days_to_get_hired <- as.Date(as.character(contact_log$contact_log.Date_turned_green__c), format="%m/%d/%Y")-
  as.Date(as.character(contact_log$contact_log.Date_Turned_Blue__c), format="%m/%d/%Y")
contact_log$days_to_get_hired <- (-1)*contact_log$days_to_get_hired

nrow(subset(contact_log,days_to_get_hired < 0))
contact_log$z <- ifelse(contact_log$days_to_get_hired<0,0,1)
z1<-which(contact_log$z == 0)
contact_log <- contact_log[-c(z1), ] 

dim(contact_log)
contact_log$contact_log.Attended_Day_1__c <- ifelse(is.na(contact_log$contact_log.Attended_Day_1__c) | contact_log$contact_log.Attended_Day_1__c==0,0,1)
contact_log$contact_log.Attended_Day_2__c <- ifelse(is.na(contact_log$contact_log.Attended_Day_2__c) | contact_log$contact_log.Attended_Day_2__c==0,0,1)
contact_log$contact_log.Compass_Participant__c <- ifelse(is.na(contact_log$contact_log.Compass_Participant__c) | contact_log$contact_log.Compass_Participant__c==0,0,1)
contact_log$contact_log.Career_Counseling_Sessions_Number_given__c <- ifelse(is.na(contact_log$contact_log.Career_Counseling_Sessions_Number_given__c),0,1)
contact_log$contact_log.Date_Attended_HHUSA_Workshop__c <- ifelse(is.na(contact_log$contact_log.Date_Attended_HHUSA_Workshop__c),0,1)
contact_log$contact_log.Career_Opportunity_Day_Participant__c <- ifelse(is.na(contact_log$contact_log.Career_Opportunity_Day_Participant__c) | contact_log$contact_log.Career_Opportunity_Day_Participant__c==0,0,1)
contact_log$contact_log.Created_LinkedIn_account__c <- ifelse(is.na(contact_log$contact_log.Created_LinkedIn_account__c) | contact_log$contact_log.Created_LinkedIn_account__c==0,0,1)

contact_log$logged_activities <-     contact_log$contact_log.Attended_Day_1__c +
  contact_log$contact_log.Attended_Day_2__c +
  contact_log$contact_log.Compass_Participant__c +
  contact_log$contact_log.Career_Counseling_Sessions_Number_given__c +
  contact_log$contact_log.Date_Attended_HHUSA_Workshop__c +
  contact_log$contact_log.Career_Opportunity_Day_Participant__c +
  contact_log$contact_log.Created_LinkedIn_account__c


contact_log$x <- NULL;contact_log$y <- NULL;contact_log$z <- NULL;contact_log$contact_log.Career_Counseling__c1 <- NULL
sapply(contact_log, function(x) sum(is.na(x))) 
dim(contact_log)
write.csv(contact_log,"C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Question1/contact_log_final.csv")


################################
contact_log1 <- contact_log[complete.cases(contact_log$Bus_Days_between_Assigned_and_Assessed__c,contact_log$Bus_Days_between_Assessment_and_Resume__c,contact_log$days_to_get_hired,
                                           contact_log$logged_activities,contact_log$contact_log.Confirmed_Hired_Date__c), ]
sapply(contact_log1, function(x) sum(is.na(x))) 
dim(contact_log1)
write.csv(contact_log1,"C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Question1/contact_log_final1.csv")

################################
contact_log$Bus_Days_between_Assigned_and_Assessed__c_na = is.na(contact_log$Bus_Days_between_Assigned_and_Assessed__c)
contact_log$Bus_Days_between_Assigned_and_Assessed__c[contact_log$Bus_Days_between_Assigned_and_Assessed__c_na == TRUE] <-
  mean(na.omit(contact_log$Bus_Days_between_Assigned_and_Assessed__c))

contact_log$Bus_Days_between_Assessment_and_Resume__c_na = is.na(contact_log$Bus_Days_between_Assessment_and_Resume__c)
contact_log$Bus_Days_between_Assessment_and_Resume__c[contact_log$Bus_Days_between_Assessment_and_Resume__c_na == TRUE] <-
  mean(na.omit(contact_log$Bus_Days_between_Assessment_and_Resume__c))

contact_log$days_to_get_hired_na = is.na(contact_log$days_to_get_hired)
contact_log$days_to_get_hired[contact_log$days_to_get_hired_na == TRUE] <-
  mean(na.omit(contact_log$days_to_get_hired))

write.csv(contact_log,"C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Question1/contact_log_final.csv")



###################################
lm.hiredsooner <- lm(contact_log$days_to_get_hired~contact_log$Bus_Days_between_Assigned_and_Assessed__c+contact_log$Bus_Days_between_Assessment_and_Resume__c
                     +contact_log$logged_activities,na.action = na.omit)
summary(lm.hiredsooner)
lm.hiredsooner.step <- step(lm(contact_log$days_to_get_hired~contact_log$Bus_Days_between_Assigned_and_Assessed__c+contact_log$Bus_Days_between_Assessment_and_Resume__c
                               +contact_log$logged_activities,na.action = na.omit),direction="both")
step <- stepAIC(lm.hiredsooner, direction="both")
step$coefficients
