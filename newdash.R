library(tidyverse)
library(readr)

rm(list=ls())

#API to retrive data from the QuickBase database 
#CONFIDENTIAL INFORMATION HAS BEEN ALTERED
data <- read.csv("https://oic.quickbase.com/db/blahblahblah=api_genresultstable&qid=anumber&options=csv&usertoken=somemoreannoyingjunk", header=TRUE)

#select data and separate by grant
data2 <- data %>% select(`Grant.Name`,`Site`,`Measurable.Skills.Gain`,`Trainings.Lead.to.IRC`,
                         `Attained.Industry.Recogonized.Credential`,`Job.Placement`, 
                         `Recidivism..DOL.Definition.`)
SOAR2 <- filter(data2, `Grant.Name`=="SOAR 2 (DOL RP) Young Adult")
SOAR3A <- filter(data2, `Grant.Name`=="SOAR 3A (DOL RP) Adult")
SOAR3Y <- filter(data2, `Grant.Name`=="SOAR 3Y (DOL RP) Young Adult")
SOAR4 <- filter(data2, `Grant.Name`=="SOAR 4 (DOL RP) Adult ")


#get count of relevant data for more convenient visulization creations 
S2Enrollments <- SOAR2 %>% group_by(`Site`) %>% count(`Site`)
S2MSG <- SOAR2 %>% group_by(`Site`) %>% summarise(x = sum(`Measurable.Skills.Gain`))
S2IRC <- SOAR2 %>% group_by(`Site`) %>% summarise(x = sum(`Trainings.Lead.to.IRC`))
S2AIRC <- SOAR2 %>% group_by(`Site`) %>% summarise(x = sum(`Attained.Industry.Recogonized.Credential`))
S2JP <- SOAR2 %>% group_by(`Site`) %>% summarise(x = sum(`Job.Placement`))
S2Recidivism <- SOAR2 %>% group_by(`Site`) %>% summarise(x = sum(`Recidivism..DOL.Definition.`))

S2wide <- data.frame(S2Enrollments$`Site`,S2Enrollments$`n`,S2MSG$`x`,S2IRC$`x`,S2AIRC$`x`,
                     S2JP$`x`,S2Recidivism$`x`)

#reshape and restructure data
S2wide <- S2wide %>% rename(Site=S2Enrollments.Site) %>% rename(count_Enroll=S2Enrollments.n) %>% rename(
  count_MSG=S2MSG.x) %>% rename(count_Train=S2IRC.x) %>% rename(count_AIRC=S2AIRC.x) %>%
  rename(count_JP=S2JP.x) %>% rename(count_Recid=S2Recidivism.x)

S2wide <- S2wide %>% add_column(count_EG=c(110, 240, 120, 130))

S2long <- S2wide %>% reshape(varying = c("count_Enroll", "count_MSG", "count_Train", "count_AIRC", "count_JP", "count_Recid", "count_EG"), 
                             v.names = "count", timevar = "Outcome", times = c("Enrollments", "Measurable Skills Gain",
                                                                               "In Training", "Attained Industry Recogonized Credentials",
                                                                               "Job Placements", "Recidivism", "Enrollment Goal"), 
                             new.row.names = 1:1000, direction="long") 
S2long <- S2long %>% select(`Site`,`Outcome`,`count`)  

rm(S2Enrollments, S2MSG, S2IRC, S2AIRC, S2JP, S2Recidivism)

#filtering to create smaller datasets, allowing optimal visualizations  
S2Enroll <- S2long %>% filter(Outcome=="Enrollments" | Outcome=="Enrollment Goal")

S2Training <- S2long %>% filter(Outcome=="Enrollments" | Outcome=="In Training")
S2Training <- S2Training %>% mutate (count = ifelse(Outcome=="Enrollments", 0.7*count, count))
S2Training <- S2Training %>% mutate (Outcome = ifelse(S2Training$Outcome=="Enrollments", "Training Goal", "In Trainings"))

S2IRC <- S2long %>% filter(Outcome=="In Training" | Outcome=="Attained Industry Recogonized Credentials")
S2IRC <- S2IRC %>% mutate (count = ifelse(Outcome=="In Training", 0.6*count, count))
S2IRC <- S2IRC %>% mutate (Outcome = ifelse(Outcome=="In Training", "Industry Recogonized Credential Goal", "Industry Recogonized Credential Attainments"))

S2JobP <- S2long %>% filter(Outcome=="Enrollments" | Outcome=="Job Placements")
S2JobP <- S2JobP %>% mutate (count = ifelse(Outcome=="Enrollments", 0.7*count, count))
S2JobP <- S2JobP %>% mutate (Outcome = ifelse(Outcome=="Enrollments", "Job Placement Goal", "Job Placements"))


#repeat process for grants SOAR3A, SOAR3Y, and SOAR4
S3AEnrollments <- SOAR3A %>% group_by(`Site`) %>% count(`Site`)
S3AMSG <- SOAR3A %>% group_by(`Site`) %>% summarise(x = sum(`Measurable.Skills.Gain`))
S3AIRC <- SOAR3A %>% group_by(`Site`) %>% summarise(x = sum(`Trainings.Lead.to.IRC`))
S3AAIRC <- SOAR3A %>% group_by(`Site`) %>% summarise(x = sum(`Attained.Industry.Recogonized.Credential`))
S3AJP <- SOAR3A %>% group_by(`Site`) %>% summarise(x = sum(`Job.Placement`))
S3ARecidivism <- SOAR3A %>% group_by(`Site`) %>% summarise(x = sum(`Recidivism..DOL.Definition.`))

S3Awide <- data.frame(S3AEnrollments$`Site`,S3AEnrollments$`n`,S3AMSG$`x`,S3AIRC$`x`,S3AAIRC$`x`,
                      S3AJP$`x`,S3ARecidivism$`x`)

S3Awide <- S3Awide %>% rename(Site=S3AEnrollments.Site) %>% rename(count_Enroll=S3AEnrollments.n) %>% 
  rename(count_MSG=S3AMSG.x) %>% rename(count_Train=S3AIRC.x) %>% rename(count_AIRC=S3AAIRC.x) %>%
  rename(count_JP=S3AJP.x) %>% rename(count_Recid=S3ARecidivism.x)

S3Awide <- S3Awide %>% add_column(count_EG=c(120, 150, 160, 140))

S3Along <- S3Awide %>% reshape(varying = c("count_Enroll", "count_MSG", "count_Train", "count_AIRC", "count_JP", "count_Recid", "count_EG"), 
                               v.names = "count", timevar = "Outcome", times = c("Enrollments", "Measurable Skills Gain",
                                                                                 "In Training", "Attained Industry Recogonized Credentials",
                                                                                 "Job Placements", "Recidivism", "Enrollment Goal"), 
                               new.row.names = 1:1000, direction="long") 
S3Along <- S3Along %>% select(`Site`,`Outcome`,`count`)  

rm(S3AEnrollments, S3AMSG, S3AIRC, S3AAIRC, S3AJP, S3ARecidivism)

S3AEnroll<-S3Along %>% filter(Outcome=="Enrollments" | Outcome=="Enrollment Goal")

S3ATraining<-S3Along %>% filter(Outcome=="Enrollments" | Outcome=="In Training")
S3ATraining <- S3ATraining %>% mutate (count = ifelse(Outcome=="Enrollments", 0.6*count, count))
S3ATraining <- S3ATraining %>% mutate (Outcome = ifelse(S3ATraining$Outcome=="Enrollments", "Training Goal", "In Trainings"))

S3AIRC <- S3Along %>% filter(Outcome=="In Training" | Outcome=="Attained Industry Recogonized Credentials")
S3AIRC <- S3AIRC %>% mutate (count = ifelse(Outcome=="In Training", 0.7*count, count))
S3AIRC <- S3AIRC %>% mutate (Outcome = ifelse(Outcome=="In Training", "Industry Recogonized Credential Goal", "Industry Recogonized Credential Attainments"))

S3AJobP <- (S3Along %>% filter(Outcome=="Enrollments" | Outcome=="Job Placements"))
S3AJobP <- S3AJobP %>% mutate (count = ifelse(Outcome=="Enrollments", 0.8*count, count))
S3AJobP <- S3AJobP %>% mutate (Outcome = ifelse(Outcome=="Enrollments", "Job Placement Goal", "Job Placements"))


S3YEnrollments <- SOAR3Y %>% group_by(`Site`) %>% count(`Site`)
S3YMSG <- SOAR3Y %>% group_by(`Site`) %>% summarise(x = sum(`Measurable.Skills.Gain`))
S3YIRC <- SOAR3Y %>% group_by(`Site`) %>% summarise(x = sum(`Trainings.Lead.to.IRC`))
S3YAIRC <- SOAR3Y %>% group_by(`Site`) %>% summarise(x = sum(`Attained.Industry.Recogonized.Credential`))
S3YJP <- SOAR3Y %>% group_by(`Site`) %>% summarise(x = sum(`Job.Placement`))
S3YRecidivism <- SOAR3Y %>% group_by(`Site`) %>% summarise(x = sum(`Recidivism..DOL.Definition.`))

S3Ywide <- data.frame(S3YEnrollments$`Site`,S3YEnrollments$`n`,S3YMSG$`x`,S3YIRC$`x`,S3YAIRC$`x`,
                      S3YJP$`x`,S3YRecidivism$`x`)

S3Ywide <- S3Ywide %>% rename(Site=S3YEnrollments.Site) %>% rename(count_Enroll=S3YEnrollments.n) %>% 
  rename(count_MSG=S3YMSG.x) %>% rename(count_Train=S3YIRC.x) %>% rename(count_AIRC=S3YAIRC.x) %>%
  rename(count_JP=S3YJP.x) %>% rename(count_Recid=S3YRecidivism.x)

S3Ywide <- S3Ywide %>% add_column(count_EG=c(155, 110, 150, 150))

S3Ylong <- S3Ywide %>% reshape(varying = c("count_Enroll", "count_MSG", "count_Train", "count_AIRC", "count_JP", "count_Recid", "count_EG"), 
                               v.names = "count", timevar = "Outcome", times = c("Enrollments", "Measurable Skills Gain",
                                                                                 "In Training", "Attained Industry Recogonized Credentials",
                                                                                 "Job Placements", "Recidivism", "Enrollment Goal"), 
                               new.row.names = 1:1000, direction="long") 
S3Ylong <- S3Ylong %>% select(`Site`,`Outcome`,`count`)  

rm(S3YEnrollments, S3YMSG, S3YIRC, S3YAIRC, S3YJP, S3YRecidivism)

S3YEnroll<-S3Ylong %>% filter(Outcome=="Enrollments" | Outcome=="Enrollment Goal")

S3YTraining<-S3Ylong %>% filter(Outcome=="Enrollments" | Outcome=="In Training")
S3YTraining <- S3YTraining %>% mutate (count = ifelse(Outcome=="Enrollments", 0.6*count, count))
S3YTraining <- S3YTraining %>% mutate (Outcome = ifelse(Outcome=="Enrollments", "Training Goal", "In Trainings"))

S3YIRC<-S3Ylong %>% filter(Outcome=="In Training" | Outcome=="Attained Industry Recogonized Credentials")
S3YIRC <- S3YIRC %>% mutate (count = ifelse(Outcome=="In Training", 0.7*count, count))
S3YIRC <- S3YIRC %>% mutate (Outcome = ifelse(Outcome=="In Training", "Industry Recogonized Credential Goal", "Industry Recogonized Credential Attainments"))

S3YJobP<-S3Ylong %>% filter(Outcome=="Enrollments" | Outcome=="Job Placements")
S3YJobP <- S3YJobP %>% mutate (count = ifelse(Outcome=="Enrollments", 0.7*count, count))
S3YJobP <- S3YJobP %>% mutate (Outcome = ifelse(Outcome=="Enrollments", "Job Placement Goal", "Job Placements"))

S4Enrollments <- SOAR4 %>% group_by(`Site`) %>% count(`Site`)
S4MSG <- SOAR4 %>% group_by(`Site`) %>% summarise(x = sum(`Measurable.Skills.Gain`))
S4IRC <- SOAR4 %>% group_by(`Site`) %>% summarise(x = sum(`Trainings.Lead.to.IRC`))
S4AIRC <- SOAR4 %>% group_by(`Site`) %>% summarise(x = sum(`Attained.Industry.Recogonized.Credential`))
S4JP <- SOAR4 %>% group_by(`Site`) %>% summarise(x = sum(`Job.Placement`))
S4Recidivism <- SOAR4 %>% group_by(`Site`) %>% summarise(x = sum(`Recidivism..DOL.Definition.`))

S4wide <- data.frame(S4Enrollments$`Site`,S4Enrollments$`n`,S4MSG$`x`,S4IRC$`x`,S4AIRC$`x`,
                     S4JP$`x`,S4Recidivism$`x`)

S4wide <- S4wide %>% rename(Site=S4Enrollments.Site) %>% rename(count_Enroll=S4Enrollments.n) %>% 
  rename(count_MSG=S4MSG.x) %>% rename(count_Train=S4IRC.x) %>% rename(count_AIRC=S4AIRC.x) %>%
  rename(count_JP=S4JP.x) %>% rename(count_Recid=S4Recidivism.x)

S4wide <- S4wide %>% add_column(count_EG=c(110, 135, 200, 125))

S4long <- S4wide %>% reshape(varying = c("count_Enroll", "count_MSG", "count_Train", "count_AIRC", "count_JP", "count_Recid", "count_EG"), 
                             v.names = "count", timevar = "Outcome", times = c("Enrollments", "Measurable Skills Gain",
                                                                               "In Training", "Attained Industry Recogonized Credentials",
                                                                               "Job Placements", "Recidivism", "Enrollment Goal"), 
                             new.row.names = 1:1000, direction="long") 
S4long <- S4long %>% select(`Site`,`Outcome`,`count`)  

rm(S4Enrollments, S4MSG, S4IRC, S4AIRC, S4JP, S4Recidivism)

S4Enroll<-S4long %>% filter(Outcome=="Enrollments" | Outcome=="Enrollment Goal")

S4Training<-S4long %>% filter(Outcome=="Enrollments" | Outcome=="In Training")
S4Training <- S4Training %>% mutate (count = ifelse(Outcome=="Enrollments", 0.7*count, count))
S4Training <- S4Training %>% mutate (Outcome = ifelse(Outcome=="Enrollments", "Training Goal", "In Trainings"))

S4IRC<-S4long %>% filter(Outcome=="In Training" | Outcome=="Attained Industry Recogonized Credentials")
S4IRC <- S4IRC %>% mutate (count = ifelse(Outcome=="In Training", 0.7*count, count))
S4IRC <- S4IRC %>% mutate (Outcome = ifelse(Outcome=="In Training", "Industry Recogonized Credential Goal", "Industry Recogonized Credential Attainments"))

S4JobP<-S4long %>% filter(Outcome=="Enrollments" | Outcome=="Job Placements")
S4JobP <- S4JobP %>% mutate (count = ifelse(Outcome=="Enrollments", 0.8*count, count))
S4JobP <- S4JobP %>% mutate (Outcome = ifelse(Outcome=="Enrollments", "Job Placement Goal", "Job Placements"))

