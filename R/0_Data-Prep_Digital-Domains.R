
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

## Check the values of the variables 
for(i in 9:317){
  print(i)
  print(table(pa12_digi[, i]))
}

## Recode not reached (8) and not administered (7) to NAs
pa12_digi[, 9:317] <- lapply(pa12_digi[, 9:317],
                             function(x) car::recode(x, "c(7, 8) = NA"))

## Change the structure of the test responses from factor to numeric 
pa12_digi[, 9:317] <- lapply(pa12_digi[, 9:317],
                             function(x) as.numeric(as.character(x)))



# Save data  --------------------------------------------------------------

save(pa12_digi, file = "1_Data/PA12_responses_digital.RData")
