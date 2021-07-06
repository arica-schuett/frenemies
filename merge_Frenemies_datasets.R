##Merging Frenemies source data 


##load needed packages
library(foreign)
library(Hmisc)
library(irr)
library(tidyverse)
library(stringr)
library(tidytext)
library(dplyr)
library("readxl")
library(devtools)
library(xlsx)
library(lubridate)
library(writexl)

##load datasets
frenemies_nocheck <- read_excel("Frenemies_NOTdouble_checked.xlsx")
frenemies_check <-read_excel("frenemiesData_Mar30_CODED.xlsx")


# keep only the columns needed for frenemies 
fC<-frenemies_check %>% select(year, governorate_1, governorate_2, planned, joint_statement, 
                              unspecified_JO, gov_collaboration, affiliates, parent_group, operations_room, 
                              big_bomb, target_class, target_class2, target_class3, target_class4, claim_source, notes,
                              ose_claim_id, claim_full_text)

# separate governorate into two columns. 
frenemies_nocheck <- separate(frenemies_nocheck, governorate, into = c("governorate_1", "governorate_2"), sep = ";", 
                               remove = FALSE)

# keep only the columns needed for frenemies 
fNC <- frenemies_nocheck %>% select(year, governorate_1, governorate_2, planned, joint_statement, 
                                 unspecified_JO, gov_collaboration, affiliates, parent_group, operations_room, 
                                 big_bomb, target_class, target_class2, target_class3, target_class4, claim_source, notes,
                                 ose_claim_id, claim_full_text)


# bind the checked and unchecked frenemies claims:
Frenemies <- rbind(fC, fNC)

# separate OSE letters from number code
Frenemies <- separate(Frenemies, 
                      ose_claim_id, 
                      into = c("ose_letters", "ose_date"), 
                      sep = 3,
                      remove = FALSE)
# create OSE year
Frenemies <- separate(Frenemies, 
                      ose_date, 
                      into = c("ose_year", "ose_month_day"), 
                      sep = 4, 
                      remove = FALSE)
# create OSE month
Frenemies <- separate(Frenemies, 
                      ose_month_day, 
                      into = c("ose_month", "ose_day"), 
                      sep = 2, 
                      remove = FALSE)

# create OSE day
Frenemies <- separate(Frenemies, 
                      ose_day, 
                      into = c("ose_day", "ose_code"), 
                      sep = 2, 
                      remove = FALSE)


# Create a standardized date
Frenemies <- unite(Frenemies, 
                   date, c("ose_day", "ose_month", "ose_year"), 
                   sep = "/", 
                   remove = FALSE)


Frenemies <- Frenemies %>% select(year, governorate_1, governorate_2, planned, joint_statement, 
                                    unspecified_JO, gov_collaboration, affiliates, parent_group, operations_room, 
                                    big_bomb, target_class, target_class2, target_class3, target_class4, claim_source, notes,
                                    ose_claim_id, claim_full_text, date)

# Sort data by date
Frenemies$date <- lubridate::dmy(Frenemies$date)
dplyr::arrange(Frenemies, date)

# write csv
write_xlsx(Frenemies, 
           "frenemiesData.xlsx" )

