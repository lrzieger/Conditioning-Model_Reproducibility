
# Generation of the conditioning variables --------------------------------
## Student background variables are prepared according to the TR to be later 
## used as conditioning variables in the population model


# Load necessary packages
library(dplyr)
library(foreign)

# Helper syntax includes self-written functions to help with the computation
source("2_R-Syntax/2H_Recode_helper.R")




# Read in data and roughly prepare it -------------------------------------

## Read in student questionnaire. 
## "INT_STU12_DEC03.txt" (+ sps-file) can be downloaded/computed from 
## http://www.oecd.org/pisa/data/pisa2012database-downloadabledata.htm
pa12_stud <- read.spss(file = "0_Raw-Data/INT_STU12_DEC03.sav", to.data.frame = T,
                       use.value.labels = F)

## Read in parent questionnaire
## "INT_PAQ12_DEC03.txt" (+ sps-file) can be downloaded/computed from 
## http://www.oecd.org/pisa/data/pisa2012database-downloadabledata.htm
pa12_pa <- read.spss(file = "0_Raw-Data/INT_PAQ12_DEC03.sav", to.data.frame = T,
                     use.value.labels = F)


## Merge student and parent data
pa12_stud <- left_join(pa12_stud, pa12_pa)
rm(pa12_pa)

## Read in variable information
var_inf <- readxl::read_excel("1_Data/Conditioning_var_information.xlsx")
var_inf <- as.data.frame(var_inf)

## All variables in the student questionnaire? 
cbind(var_inf[, 1], var_inf[, 1] %in% names(pa12_stud))
# Look at Annex B and codebooks
# (ST25int corresponds to ST25Q01)
# (ST20int corresponds to IMMIG)

pa12_stud <- split(pa12_stud, f = pa12_stud$CNT)
nam <- names(pa12_stud)




# Variables which need recoding: Check ------------------------------------

## Which variables are prepared how according to TR and information sheet 
## Technical report PISA 2012, page 421-431.

table(var_inf$Coding_type)

### Manual recoding required first
subset(var_inf, Coding_type == "manual")

### Variables with coding type manual_two
subset(var_inf, Coding_type == "manual_two")

subset(var_inf, Coding_type == "missing.dummy")

subset(var_inf, Coding_type == "national.mode")

subset(var_inf, Coding_type == "compute.score")

subset(var_inf, Coding_type == "national.max")




# Recode variables (PCA not yet) ------------------------------------------

## Initialise new data/list
con_dat <- list()

## Recode the data country-by-country
for(i in 1:length(nam)){
  print(i)
  dat <- pa12_stud[[nam[i]]]
  
  ################################################################################
  ## Recoding see pages 421 to 429 TR2012
  ## Use helper_functions.R
  ## Recoding is constructed in a way that it can only be used country-wise !!!!!
  ## The variable ST02Q01 needs to be modified country-wise at the end 
  ## --> Remove variables completly for HKG, ISL, JOR, NZL, SGP, USA
  ## --> Remove for all other countries belonging variables which only have the 
  ##     categories -1 and 0 (not 1)
  ## Also remove constant features country-wise !! Applies when a country has 
  ## only NAs or one category for the whole variable
  
  con_dat[[i]] <- data.frame(cnt = dat$CNT, stidstd = dat$StIDStd)
  
  ### Manual recoding required first
  
  #Grade
  con_dat[[i]]$grade.1 <- ifelse(dat$ST01Q01 %in% 7:14, 
                                 dat$ST01Q01 - getmode(dat$ST01Q01), 0)
  con_dat[[i]]$grade.2 <- ifelse(is.na(dat$ST01Q01), 1, 0)
  con_dat[[i]]$grade.3 <- ifelse(is.na(dat$ST01Q01), 1, 0)
  
  #Country arrival age: Set N/A (9997) is SPSS as not missing to recode it here
  con_dat[[i]]$countryArrivalAge.1 <- ifelse(!(is.na(dat$ST21Q01) | dat$ST21Q01 == 9997),
                                             dat$ST21Q01, 0)
  con_dat[[i]]$countryArrivalAge.2 <- ifelse(is.na(dat$ST21Q01), -1, 0)
  
  #Language at home
  con_dat[[i]]$langAtHome.1 <- ifelse((dat$ST25Q01 == 2 & !(is.na(dat$ST25Q01))), 1, 0)
  con_dat[[i]]$langAtHome.2 <- ifelse(is.na(dat$ST25Q01), 1, 0)
  con_dat[[i]]$langAtHome.1[dat$ST25Q01 == 1] <- -1
  con_dat[[i]]$langAtHome.2[dat$ST25Q01 == 1] <- -1
  
  ### Variables with coding type manual_two
  con_dat[[i]] <- cbind(con_dat[[i]], 
                        missing.dummy(dat$ST69Q01*dat$ST70Q01, "lmins", mode = "mean"))
  con_dat[[i]] <- cbind(con_dat[[i]], 
                        missing.dummy(dat$ST69Q02*dat$ST70Q02, "mmins", mode = "mean"))
  con_dat[[i]] <- cbind(con_dat[[i]], 
                        missing.dummy(dat$ST69Q03*dat$ST70Q03, "smins", mode = "mean"))
  
  
  ### Variables with coding type missing.dummy
  x <- subset(var_inf, Coding_type == "missing.dummy")
  
  # Manually change variable type of PA22Q01 and PA23Q01 to numeric
  dat$PA22Q01 <- as.numeric(as.character(dat$PA22Q01))
  dat$PA23Q01 <- as.numeric(as.character(dat$PA23Q01))
  
  for(j in 1:dim(x)[1]){
    w <- which(names(dat) == x[j, 1])
    print(x[j, 3])
    con_dat[[i]] <- cbind(con_dat[[i]], missing.dummy(vars = dat[, w], varname = x[j, 3],
                                                      mode = x[j, 4], recod = x[j, 5]))
  }
  
  
  ### Variables with coding type national.mode
  y <- subset(var_inf, Coding_type == "national.mode")
  for(j in 1:dim(y)[1]){
    w <- which(names(dat) == y[j, 1])
    print(y[j, 3])
    con_dat[[i]] <- cbind(con_dat[[i]], national.mode(vars = dat[, w], varname = y[j, 3]))
  }
  
  
  ### Variables with coding type compute.score
  z <- subset(var_inf, Coding_type == "compute.score")
  z_sub <- unique(z$Varname)
  
  ## Special case recoding of three possession variables:
  ## Different items are asked for each country, but the item itself is not of 
  ## interest - only if they possess it or not
  dat$ST26Q15 <- as.numeric(as.character(dat$ST26Q15)) %% 10
  dat$ST26Q16 <- as.numeric(as.character(dat$ST26Q16)) %% 10
  dat$ST26Q17 <- as.numeric(as.character(dat$ST26Q17)) %% 10
  
  for(j in z_sub){
    print(j)
    con_dat[[i]] <- cbind(con_dat[[i]], compute.score(j, z, dat = dat))
  }
  
  ### Variables with coding type national.max
  w <- subset(var_inf, Coding_type == "national.max")
  w_sub <- unique(w$Varname)
  
  for(j in w_sub){
    print(j)
    con_dat[[i]] <- cbind(con_dat[[i]], national.max(j, w, dat = dat))
  }
  
  con_dat[[i]] <- con_dat[[i]][, !(unlist(lapply(con_dat[[i]], function(x) all(is.na(x))))
  )]
  # Add contrast coding for schools and booklets
  con_dat[[i]] <- cbind(con_dat[[i]], contrast.school.book(dat$SCHOOLID, dat$Bookid))
}


lapply(con_dat, dim)

names(con_dat) <- nam

## Save complete prepared dataframe 
save(con_dat, file = "1_Data/Conditioning_data_beforePCA.RData")




# Conditioning variables: Pre-process through PCA -------------------------


## Initialise variables which are direct regressors and therefore should not be
## used in the principal component analysis
notPCA <- c("stidstd", "cnt", "schoolid", "bookid", "grade", "gender", "isei", "pIsei")

con_data <- list()

## Iterate over different countries and conduct principal component analysis 
for(i in nam){
  print(i)
  
  # Select country data
  d <- con_dat[[i]]
  
  # Remove constant and replicate data
  d <- d[, -which(duplicated(lapply(d, c)))]
  d <- d[, !unlist(lapply(d, function(y) all(duplicated(y)[-1L])))]
  
  # Get indices of direct regressors (+ student ID)
  whi <- unlist(lapply(notPCA, function(x) which(startsWith(names(d), x))))
  
  # Start new data set with direct regressors (+ student ID)
  d_neu <- d[, whi]
  
  # Principal component analysis with correlation matrix
  pca <- prcomp(d[, -whi], scale. = T)
  
  # Calculate the number of required components for 95% of the variance
  n_comp <- which(cumsum(summary(pca)$importance[2, ]) > 0.95)[1]
  
  # Add this number of principal components to the conditioning variables
  d_neu <- cbind(d_neu, pca$x[, 1:n_comp])
  
  # Save in a new list
  con_data[[i]] <- d_neu
}

lapply(con_data, dim)

## Remove constant variables from the data sets
con_data <- lapply(con_data, function(x) x[, !(unlist(lapply(x, function(y) all(duplicated(y)[-1L]))))])

## Save data which is prepared for PV computation
save(con_data, file = "1_Data/Conditioning_variables.RData")

