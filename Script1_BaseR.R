setwd("~/Desktop/PSY1210 Y/Project2/")

#packages
library(tidyverse)


# Only worked in Base R ---------------------------------------------------
#put into separate script for Charlotte

#read in as tibble & view
datawide <- read_csv("Orig_wide1.csv", col_names = TRUE, na = "NA")
str(datawide)

#Since each participant has only 1 row of data, let's take the chance to clean up ID numbers
#create new id variable, and code 1:26
datawide$ID #make sure variable is in sequential order
datawide$id <- 1:26
datawide$id
datawide <- datawide %>% select(id, everything()) #make it the fisrt column

#Currently, each traffic density/turn trial has it's own column... Let's put them all under one variable
#Convert to long format with a gather command on the traffic/turn combinations (MV_T1...CYC_T18)
datalong <- datawide %>% gather(MV_T1:CYC_T18, key= traffic_int, value= density)
datalong
str(datalong)

#Convert variables of interest into factors (this is required for the "separate" function below)
datalong$traffic_int <- as.factor(datalong$traffic_int)

#Separate the column which includes both traffic and turn number (e.g., MV_T1, but we want MV in a separate column from turn 1)
datalong <- datalong %>% separate(traffic_int, into=c('traffic_type','turn_num'), sep="_")

datalong <- datalong %>% arrange(ID)

#check
datalong$traffic_type
datalong$turn_num
datalong$density

#Save data to csv
write.csv(datalong, file = "Orig_long1.csv")