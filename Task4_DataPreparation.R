library(dplyr)
require(dplyr)

case <- read.csv("C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Case.csv", header=T, na.strings=c("","NA"))
contact <- read.csv("C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Contact.csv", header=T, na.strings=c("","NA"))
question4 <- read.csv("C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Question4/final_data_hired.csv", header=T, na.strings=c("","NA"))


c1 <- c("Active_Color__c","Id","Date_Turned_Blue__c","Date_turned_green__c",
        "Salary_Range__c","Volunteer_Services__c","Dat_Initial_Assessment_was_Completed__c","Date_assigned_to_staff__c","Date_Resume_Completed__c")
contact1 <- contact[c1]
colnames(contact1)<-c("Active_Color__c","Id","Date_Turned_Blue__c","Date_turned_green__c",
                      "Salary_Range__c","Volunteer_Services__c","Dat_Initial_Assessment_was_Completed__c","Date_assigned_to_staff__c","Date_Resume_Completed__c")

View(contact1)
dim(contact1)
dim(question3)
case <- subset(case,Status=="Completed")
case1 <- case["ContactId"]
colnames(case1)<-c("ContactId")
View(case)
dim(case1)
#duplicates in case
case1$x<-duplicated(case1)
nrow(subset(case1,x==TRUE))
c<-subset(case,ContactId=="0035000002PSd6AAAT")
View(c)

contact_case <- left_join(contact1,case1,by=c("ContactId"))
View(contact_case)
dim(contact_case)
contact_case$x<-duplicated(contact_case)
nrow(subset(contact_case,x==TRUE))
y<-which(contact_case$x == TRUE)
contact_case <- contact_case[-c(y), ] #removing duplicates
dim(contact_case)
View(contact_case)


question3$volunteer_c<-match(question3$Id,case1$ContactId)
question3$volunteer_c<-ifelse(is.na(question3$volunteer_c),0,1)
nrow(subset(question3,volunteer_c==1))

contact1$volunteer_c<-match(contact1$Id,case1$ContactId)
contact1$volunteer_c<-ifelse(is.na(contact1$volunteer_c),0,1)
nrow(subset(contact1,volunteer_c==1))

contact1$Blue_c<-ifelse(contact1$Active_Color__c=="Blue",c(1),c(0))

contact1$days_to_get_hired <- as.Date(as.character(contact1$Date_turned_green__c), format="%m/%d/%Y")-
  as.Date(as.character(contact1$Date_Turned_Blue__c), format="%m/%d/%Y")
contact1$days_to_get_hired <- (-1)*contact1$days_to_get_hired

nrow(subset(contact1,days_to_get_hired < 0))
contact1$z <- ifelse(contact1$days_to_get_hired<0,0,1)
z1<-which(contact1$z == 0)
contact1 <- contact1[-c(z1), ] 
dim(contact1)
contact1$z <- NULL

names(contact1)
contact_data <- contact1[-c(1,3,4,7:9)]
names(contact_data)
dim(contact_data)

contacts_with_volunteer <- subset(contact_data,volunteer_c==1)
dim(contacts_with_volunteer)
View(contacts_with_volunteer)

hired_contacts <- subset(contact_data,Blue_c=="1")
dim(hired_contacts)
View(hired_contacts)

hired_contacts_with_volunteers <- subset(contact_data,Blue_c=="1" & volunteer_c==1)
dim(hired_contacts_with_volunteers)


contact1;contacts_with_volunteer;hired_contacts;hired_contacts_with_volunteers;

write.csv(contact_data,"C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Question4/contact_data.csv")
write.csv(contacts_with_volunteer,"C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Question4/contacts_with_volunteer.csv")
write.csv(hired_contacts,"C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Question4/hired_contacts.csv")
write.csv(hired_contacts_with_volunteers,"C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Question4/hired_contacts_with_volunteers.csv")


sapply(contact_data, function(x) sum(is.na(x))) 
sapply(contact_data, function(x) sum(is.na(x))/length(x) > 0.7) 

sapply(contacts_with_volunteer1, function(x) sum(is.na(x))) 
sapply(hired_contacts, function(x) sum(is.na(x))) 
sapply(hired_contacts_with_volunteers, function(x) sum(is.na(x))) 

contact_data1 <- contact_data[complete.cases(contact_data$volunteer_c,contact_data$Blue_c), ]
contacts_with_volunteer1 <- contacts_with_volunteer[complete.cases(contacts_with_volunteer$Blue_c,contacts_with_volunteer$Volunteer_Services__c), ]
hired_contacts1 <- hired_contacts[complete.cases(hired_contacts$volunteer_c,hired_contacts$days_to_get_hired,hired_contacts$avg_salary), ]
hired_contacts_with_volunteers1 <- hired_contacts_with_volunteers[complete.cases(hired_contacts_with_volunteers$Volunteer_Services__c,hired_contacts_with_volunteers$days_to_get_hired), ]

write.csv(contact_data1,"C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Question4/contact_data1.csv")
write.csv(contacts_with_volunteer1,"C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Question4/contacts_with_volunteer1.csv")
write.csv(hired_contacts1,"C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Question4/hired_contacts1.csv")
write.csv(hired_contacts_with_volunteers1,"C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Question4/hired_contacts_with_volunteers1.csv")

dim(contact_data1)
dim(contacts_with_volunteer1)
dim(hired_contacts1)
dim(hired_contacts_with_volunteers1)
View(contacts_with_volunteer1)


lm.out <- lm(hired_contacts$days_to_get_hired~hired_contacts$volunteer_c, na.action = na.omit)
