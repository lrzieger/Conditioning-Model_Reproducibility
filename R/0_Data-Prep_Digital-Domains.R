
# Generate a student response data including the additional domains -------
## Generate a data set like PISA responses scored, but this time including the 
## domains problem solving, digital reading and mathematics
## Proceedings equal to the other DP response script


# Load necessary packages
library(foreign)
library(dplyr)


# Read in data from SPSS --------------------------------------------------

## Read in scored cognitive data including the digital domains
## "CBA_COG12_S_MAR31.txt" (+ sps-file) can be downloaded/computed from 
## http://www.oecd.org/pisa/pisaproducts/database-cbapisa2012.htm
pa12_digi <- read.spss(file = "0_Raw-Data/CBA_COG12_S_MAR31.sav", 
                       use.value.labels = F, to.data.frame = T, 
                       use.missings = F)


# Check response variable and manipulate form and missing -----------------

## Counter check sample size with technical report (n=485490)
dim(pa12_digi)

## Show sample sizes per country
pa12_digi %>% group_by(CNT) %>% summarise(n = n())

## Check if the deleted item is already removed from the data
"PM603Q02" %in% names(pa12_digi)

## Check on a sample base if the dodgy items in the respective countries are 
## set to NA
table(subset(pa12_digi, CNT == "ISL")$PR437Q06)
table(subset(pa12_digi, CNT == "FRA")$PS131Q04)
table(subset(pa12_digi, CNT == "FRA" & BOOKID == 1)$PS131Q04)
table(subset(pa12_digi, CNT == "FIN")$PR406Q01)
table(subset(pa12_digi, CNT == "FIN" & TESTLANG == 420)$PR406Q01)

## Check the values of the variables 
for(i in 9:317){
  print(i)
  print(table(pa12_digi[, i]))
}

## Recode not reached (8) and not administered (7) to NAs
for(i in 9:317){
  pa12_digi[, i] <- car::recode(pa12_digi[, i], "c(7, 8) = NA")
}

## Change the structure of the test responses from factor to numeric 
for(i in 9:317){
  pa12_digi[, i] <- as.numeric(as.character(pa12_digi[, i]))
}



# Save data  --------------------------------------------------------------

save(pa12_digi, file = "1_Data/PA12_responses_digital.RData")
