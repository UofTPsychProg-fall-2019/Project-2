setwd("~/Desktop/PSY1210 Y/Project2/")
setwd("~/Desktop/Density-Project/")

#packages
library(tidyverse)

# DATA WRANGLING in Base R ---------------------------------------------------
#[see Script1]


# DATA WRANGLING in RStudio -------------------------------------------------------------

#read in csv created in Base R
data <- read_csv("Orig_long1.csv", col_names = TRUE, na = "NA")

#check
str(data)
colnames(data) #should have 26 subs
data <- select(data, -X1) #remove random extra column created by excel

#reformat data so that all 3 traffic density are in the same row, for each turn trial
datawider <- data %>% pivot_wider(names_from = traffic_type, values_from = density)
#(for some reason this command did not work in Base R, so I had to save the csv file and do it here!)

#save data to csv 
write.csv(datawider, file = "Orig_long2.csv")

#In Excel, repeated-measures variables were added to the dataframe (i.e. turn specific variables)
#These variables are: turn direction, intersection control type, and failure codings (DV)
data <- read_csv("Orig_long3.csv", col_names = TRUE, na = "NA")

#check
str(data) #should have 18 turns x 26 participants = 468 observations
data <- select(data, -X1, -ID) #remove random extra columns created by excel as well as old ID variable
colnames(data) 

# DATA PREP ----------------------------------------------------------

#density values are an average out of 3 raters, let's change these to "low". "medium" or "high" instead of demicals
#1 = low
#1.33 = low
#1.66 = med
#2 = med
#2.33 = med
#2.66 = high
#3 = high

FactorVars <- c('MV', 'PED','CYC')
data <- mutate_at(data, FactorVars, ~factor(.))
levels(data$MV)

data$MV <- ifelse(data$MV == "1", "low", ifelse(data$MV == "1.333333333", "low", ifelse(data$MV == "1.666666667", "med", 
                                                 ifelse(data$MV == "2", "med", ifelse(data$MV == "2.333333333", "med", 
                                                 ifelse(data$MV == "2.666666667", "high", ifelse(data$MV == "3", "high", 99)))))))
data$PED <- ifelse(data$PED == "1", "low", ifelse(data$PED == "1.333333333", "low", ifelse(data$PED == "1.666666667","med", 
                                                 ifelse(data$PED == "2", "med", ifelse(data$PED == "2.333333333", "med",
                                                 ifelse(data$PED == "2.666666667", "high", ifelse(data$PED == "3", "high", 99)))))))
data$CYC <- ifelse(data$CYC == "1", "low", ifelse(data$CYC == "1.333333333", "low", ifelse(data$CYC == "1.666666667", "med", 
                                                 ifelse(data$CYC == "2", "med", ifelse(data$CYC == "2.333333333", "med", 
                                                 ifelse(data$CYC == "2.666666667", "high", ifelse(data$CYC == "3", "high", 99)))))))

#look at totals for each group
table(data$MV); table(data$PED); table(data$CYC)
#because there is a high number of "low" densities, and almost no "high" densities, merge "med" and "high" into just "high"

#merging medium and high codings for PED and MV traffic types
data$MV <- recode(data$MV, "med" = "high", .missing = NULL)
data$PED <- recode(data$PED, "med" = "high", .missing = NULL)
data$CYC <- recode(data$CYC, "med" = "high", .missing = NULL)
#make sure it worked (should be no mediums)
table(data$MV); table(data$PED); table(data$CYC) 
#note the very unequal numbers for cyclist density -- will likely remove this variable from any statistical analysis


## Group drivers into "cyclist" or "noncylclist" groups based on their answer to this question: “Over the year (excluding winter), 
#how often do you ride a bicycle as a transportation tool?”
data$cycexp <- factor(data$cycexp, levels = c("Never", "A few times or less", "A few days a month", "A few days a week", 
                                              "Every day or almost every day"))
levels(data$cycexp)
#Group respondents who self-reported that they cycled at least “a few days a month” under the cyclist-driver category, while those 
#who selected that they cycled less than “a few days a year” will be grouped under the non-cyclist driver group
#use dplyr recode function (old = new):
data$cycexp <- recode(data$cycexp, "Never" = "noncyclist", "A few times or less" = "noncyclist", 
                      "A few days a month" = "cyclist", "A few days a week" = "cyclist", "Every day or almost every day" = "cyclist", 
                      .default = "other", .missing = NULL)


#refactor DV (failure) and order if it makes sense
data$failure = factor(data$failure, levels = c("No","Yes"), labels = c("No","Yes"))

#factor & order reorder levels
data$cycling_test <- factor(data$cycling_test, levels = c('Never', 'A few times a year or less', 'A few days a month',
                                                          'A few days a week', 'Every day or almost every day'),
                            labels = c("Never", "FTAY", "FDAM", "FDAW", "EDOAED"))
data$walk_test <- factor(data$walk_test, levels = c('Never', 'A few times a year or less', 'A few days a month',
                                                    'A few days a week', 'Every day or almost every day'),
                         labels = c("Never", "FTAY", "FDAM", "FDAW", "EDOAED"))
data$downtown_test <- factor(data$downtown_test, levels = c('Never', 'A few times a year or less', 'A few days a month',
                                                            'A few days a week', 'Every day or almost every day'),
                             labels = c("Never", "FTAY", "FDAM", "FDAW", "EDOAED"))
data$downtown_tor <- factor(data$downtown_tor, levels = c('Never', 'A few times a year or less', 'A few days a month',
                                                          'A few days a week', 'Every day or almost every day'),
                            labels = c("Never", "FTAY", "FDAM", "FDAW", "EDOAED"))
data$turn_num <- factor(data$turn_num, levels = c('T1', 'T2', 'T3', 'T4', 'T5', 'T6', 'T7', 'T8', 'T9',
                                                  'T10','T11', 'T12', 'T13', 'T14', 'T15', 'T16', 'T17', 'T18'))


# DATA SUMMARIES ----------------------------------------------------------
theme_set(theme_minimal())
library(viridis)

###########effects of traffic density on rate of visual checking failures#################
#build summary tables for graphs
MVfail <- table(data$failure, data$MV, useNA = "no")
PEDfail <- table(data$failure, data$PED, useNA = "no")

#Failure counts by Motor Vehicle Traffic conditions
MVfail <- as.data.frame(MVfail)
colnames(MVfail) <- c("Fail","Density","Count")
MVfail$Type <- "Motor vehicles"

#Failures counts by Pedestrian Traffic conditions
PEDfail <- as.data.frame(PEDfail)
colnames(PEDfail) <- c("Fail","Density","Count")
PEDfail$Type <- "Pedestrians"

#combined table
allFail <- rbind(MVfail, PEDfail)
allFail$Type <- as.factor(allFail$Type)
allFail <- subset(allFail, Fail=="Yes")

#failure counts for all traffic types/densities
d1 <- ggplot(allFail, aes(fill = Density , y = Count, x = Type)) + geom_bar(position = "stack", stat = "identity")
d1 + labs(x = "Traffic type", y = "Failure count", fill = "Density of traffic")

#failure counts x MV traffic densities
d2 <- ggplot(MVfail, aes(fill = Fail , y = Count, x = Density)) + geom_bar(position = "stack", stat = "identity")
d2 + labs(x = "Motor Vehicle Density", y = "Frequency", fill = "Visual Checking Failure")

#failure counts x pedestrian traffic densities
d3 <- ggplot(PEDfail, aes(fill = Fail , y = Count, x = Density)) + geom_bar(position = "stack", stat = "identity")
d3 + labs(x = "Pedestrian Density", y = "Frequency", fill = "Visual Checking Failure")


###########turn/intersection differences re: rate of failures#################
#failure rates per turn
tfails <- subset(data, failure=="Yes")
tfails <- table(tfails$turn_num)
tfails <- as.data.frame(tfails) 
colnames(tfails) <- c("Intersection","Failures")
tfails$Obstot <- c(26, 26, 26, 26, 26, 26, 26, 25, 25, 24, 24, 24, 24, 22, 23, 23, 23, 23)
tfails <- tfails[order(tfails$Failures, decreasing = TRUE),]

#divide Failures by total number of observations per turn
tfails <- tfails %>% mutate(Percent = Failures/Obstot*100)
tfails <- tfails %>% mutate_if(is.numeric, round, 2) #round to 2 decimal pts

t1 <- ggplot(tfails, aes(x = reorder(Intersection,-Percent), y = Percent)) + 
  geom_bar(stat = "identity", fill = "#84F2D1") + theme(axis.text.x = element_text(angle = 60, hjust = 1))
t1 + labs(title = "% Failures by intersection", x = "Turn Number", y = "Rate (%) of Failures")

#failure rates by turn direction and control type
turncontin <- subset(data, failure=="Yes")
turncontin <- table(turncontin$direction, turncontin$control_type, useNA = "no")
turncontin <- as.data.frame(turncontin)
turncontin$Grouptot <- c(104, 78, 104, 182)
colnames(turncontin) <- c("Turn_Direction","Control_Type", "Failure_Count", "Grouptot")
#divide Failures by total number of observations for each turn type
turncontin <- turncontin %>% mutate(Percent = Failure_Count/Grouptot*100)
turncontin <- turncontin %>% mutate_if(is.numeric, round, 2) #round to 2 decimal pts

#check that there are really only 3/104 failures for left, signalized turns
check <- data %>%
  filter(direction == "Left", control_type == "Signalized")

#plot turn types x failures 
t2 <- ggplot(turncontin, aes(fill = Control_Type, y = Percent, x = Turn_Direction)) + 
  geom_bar(position = "stack", stat = "identity")
t2 + labs(x = "Turn Direction", y = "Rate (%) of Failures", fill = "Control Type")
#it appears that right turns are more prone to failure (irrespective of control type)
#for left turns, it appears that uncontrolled lefts may be more dangerous!


###########individual differences re: rate of failures#################
#Rate of failures by participant id 
pfails <- subset(data, failure=="Yes")
pfails <- table(pfails$id)
pfails <- as.data.frame(pfails)
colnames(pfails) <- c("id","Failures") 
pfails$id <- factor(pfails$id, levels = c(1:26))
pfails <- rbind(pfails, c(6, 0)) #one participant did not exhibit any failures, make sure they are kept in the table
pfails <- pfails[order(pfails$id),]
pfails$Turntot <- c(18, 18, 9, 18, 18, 18, 18, 16, 18, 18, 18, 18, 18, 18, 18, 18, 17, 18, 18, 18, 14, 18, 18, 18, 18, 9)
pfails <- pfails %>% mutate(Percent = Failures/Turntot*100)
pfails <- pfails %>% mutate_if(is.numeric, round, 2) #round to 2 decimal pts

#failure rate by participant
p1 <- ggplot(pfails, aes(x = reorder(id,-Percent), y = Percent)) + geom_bar(stat = "identity", fill = "#FF9333") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
p1 + labs(x = "Participant", y = "Rate (%) of Failures")
#there appear to be large individual differences!

###########questionnaire data#################
#questionnaire response frequencies (out of total 26 participants)
#select questionnaire items
Qfreqs <- data %>% select(id, YOB, cycexp, walk_test, cycling_test, downtown_test, downtown_tor, YOB, G2_year) 
Qfreqs <- distinct(Qfreqs) #remove any repeated rows, which gives us one row for each participant 

table(Qfreqs$walk_test); table(Qfreqs$downtown_test); table(Qfreqs$downtown_tor); table(Qfreqs$cycling_test)
table(Qfreqs$G2_year) ; table(Qfreqs$YOB)

#set up dataframe for graphs
total <- merge(Qfreqs, pfails, by = "id")

#roughly plot response frequencies and relations (out of 26 participants)
q1 <- plot(total$downtown_test, main = "Distribution of responses: how often do you drive in the test area?")

q2 <- plot(total$downtown_tor, main = "Distribution of responses: how often do you drive in downtown Toronto?")

q3 <- ggplot(total, aes(x = G2_year, y = YOB)) + geom_point() + geom_smooth(method = lm)
q3 + labs(title = "Correlation of G2 attainment and year of birth", x = "G2 attaiment", y = "Year of birth")

q4 <- ggplot(total, aes(x = downtown_tor, y = downtown_test)) + geom_point()
q4 + labs(title = "Correlation of driving downtown and driving in test area", x = "Driving downtown", 
          y = "Driving in test area")

#visualize relationships between demographic variables and rate of visual checking failures
p5 <- ggplot(total, aes(x = cycexp, y = Percent)) + geom_bar(position = "dodge", stat = "identity", fill = "#76D7C4")
p5 + labs(title = "Failures by cycling experience", x = "Experience", y = "Rate (%) of Failures")

q6 <- ggplot(total, aes(x = YOB, y = Percent)) + geom_point() + geom_smooth(method = lm)
q6 + labs(title = "Failures by year of birth", x = "Year of Birth", y = "Rate (%) of Failures")

q7 <- ggplot(total, aes(x = G2_year, y = Percent)) + geom_point() + geom_smooth(method = lm)
q7 + labs(title = "Failures by year G2 attainment", x = "Year of G2 attainment", y = "Rate (%) of Failures")

q8 <- ggplot(total, aes(x = downtown_test, y = Percent)) + geom_bar(position = "dodge", stat = "identity", fill = "#F4D03F")
q8 + labs(title = "Test area driving", x = "How often they drive in test area", y = "Rate (%) of Failures")

q9 <- ggplot(total, aes(x = downtown_tor, y = Percent)) + geom_bar(position = "dodge", stat = "identity", fill = "#C39BD3")
q9 + labs(title = "Downtown driving", x = "How often they drive downtown", y = "Rate (%) of Failures")

q10 <- ggplot(total, aes(x = cycling_test, y = Percent)) + geom_bar(position = "dodge", stat = "identity", fill = "#EC7063") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
q10 + labs(x = "How often participant cycles in area", y = "Rate (%) of Failures")

q11 <- ggplot(total, aes(x = walk_test, y = Percent)) + geom_bar(position = "dodge", stat = "identity", fill = "#76D7C4") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
q11 + labs(x = "How often participant walks in area", y = "Rate (%) of Failures")
