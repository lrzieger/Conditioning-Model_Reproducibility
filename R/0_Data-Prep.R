# Generate a student response data set and a plausible value data set -------

# Load necessary packages
library(foreign)
library(dplyr)



# Read in data sets from SPSS ---------------------------------------------



## Read in scored cognitive data
## "INT_COG12_S_DEC03.txt" (+ sps-file) can be downloaded/computed from 
## http://www.oecd.org/pisa/data/pisa2012database-downloadabledata.htm
pa12_resp_s <- read.spss(file = "0_Raw-Data/INT_COG12_S_DEC03.sav", 
                         use.value.labels = F, to.data.frame = T, 
                         use.missings = F)

## Read in student data to final student weight (W_FSTUWT) to test responses
## "INT_STU12_DEC03.txt" (+ sps-file) can be downloaded/computed from 
## http://www.oecd.org/pisa/data/pisa2012database-downloadabledata.htm
pa12_stud <- read.spss(file = "0_Raw-Data/INT_STU12_DEC03.sav", to.data.frame = T,
                       use.value.labels = F)

## Extract PV data into a separate dataset
pa12_pv <- pa12_stud[c("StIDStd", "CNT", "SCHOOLID", "OECD", 
                       paste0("PV", 1:5, rep(c("MATH", "READ", "SCIE"), 
                                             each = 5)))]

## Extract weights
pa12_wgt <- pa12_stud[c("CNT", "SUBNATIO", "STRATUM", "OECD", "NC", "SCHOOLID",
                        "StIDStd", "W_FSTUWT", paste0("W_FSTR", 1:80), "WVARSTRR", 
                        "VAR_UNIT", "senwgt_STU", "VER_STU")]

## Extract country, gender, bookid and the indicator if it is the easy version 
## of PISA. Add a student ID for each country by just counting along the 
## observations
book_dat <- data.frame(CNT = pa12_stud$CNT, 
                       pid = with(pa12_stud, ave(rep(1, nrow(pa12_stud)), CNT,
                                                 FUN = seq_along)),
                       female = ifelse(pa12_stud$ST04Q01 == 2, 0, 
                                       pa12_stud$ST04Q01),
                       BOOKID = pa12_stud$Bookid,
                       EASY = pa12_stud$EASY)

## Add final student weight (and remove unnecessary data afterwards)
pa12_resp_s <- merge(pa12_resp_s, pa12_stud[, c(1, 6, 7, 550)], all.x = T)
# Differences in the sample sizes are due to the fact that the USA regions are
# not included in the student data set

rm(pa12_stud)




# Check response variable and manipulate form and missing -----------------

## Counter check sample size with technical report (n=485490)
dim(pa12_resp_s)

## Show sample sizes per country
pa12_resp_s %>% group_by(CNT) %>% summarise(n = n())

## Check if the deleted item is already removed from the data
"PM603Q02" %in% names(pa12_resp_s)

## Check on a sample base if the dodgy items in the respective countries are 
## set to NA
table(subset(pa12_resp_s, CNT == "ISL")$PR437Q06)
table(subset(pa12_resp_s, CNT == "FRA")$PS131Q04)
table(subset(pa12_resp_s, CNT == "FRA" & BOOKID == 1)$PS131Q04)
table(subset(pa12_resp_s, CNT == "FIN")$PR406Q01)
table(subset(pa12_resp_s, CNT == "FIN" & TESTLANG == 420)$PR406Q01)

## Check the values of the variables 
for(i in 9:214){
  print(i)
  print(table(pa12_resp_s[, i]))
}

## Recode not reached (8) and not administered (7) to NAs
pa12_resp_s[, 9:214] <- lapply(pa12_resp_s[, 9:214], 
                               function(x) car::recode(x, "c(7, 8) = NA"))

## Change the structure of the test responses from factor to numeric 
pa12_resp_s[, 9:214] <- lapply(pa12_resp_s[, 9:214], 
                               function(x) as.numeric(as.character(x)))

## Removing the unnecessary data from regions (not used for scaling):
## Perm (QRS; Russia), Florida (QUA; USA), Connecticut (QUB; USA),
## Massachusetts (QUC; USA)
pa12_resp_s <- subset(pa12_resp_s, !(CNT %in% c("QRS", "QUA", "QUB", "QUC")))



# Save data  --------------------------------------------------------------

save(pa12_resp_s, file = "1_Data/PA12_responses_scored_prepared.RData")
save(pa12_pv, file = "1_Data/PA12_plausible_values.RData")
save(book_dat, file = "1_Data/PA12_bookid_dat.RData")
save(pa12_wgt, file = "1_Data/PA12_weights.RData")
