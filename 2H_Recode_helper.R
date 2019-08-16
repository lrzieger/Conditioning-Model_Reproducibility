library(fastDummies)
library(dplyr)



# getmode() ---------------------------------------------------------------


## Get mode of a variable
# Input: numeric vector
# Output: single number
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}




# missing.dummy() ---------------------------------------------------------


## Generate a dummy for missing values and subtract either nothing, mean or median
# Input: vars -- variable (numeric for median and mean)
#        varname -- the variable name which the resulting variables should have
#        mode -- should the not missing variable be transformed (string "mean" or
#                "median" to subtract either)
#        recod -- if variable should be recoded, enter recoding text according to 
#                 car::recode here ("x=y;a=b")
# Output: two variables named varname.1 and varname.2 -- one missing dummy and other
#         with the value of (transformed) variable

missing.dummy <- function(vars, varname, mode = "median", recod = NA){
  ## Recode if necessary
  if(!(is.na(recod))){
    vars <- car::recode(vars, recod, as.factor = F)
  }
  
  ## Difference depending on mode
  m <- 0
  if(mode == "median") m <- median(vars, na.rm = T)
  if(mode == "mean")  m <- mean(vars, na.rm = T)
  
  ## Make dummies
  temp <- ifelse(!is.na(vars), vars - m, 0)
  temp <- cbind(temp, ifelse(is.na(vars), 1, 0))
  
  ## Rename dummies
  colnames(temp) <- paste(varname, 1:2, sep = ".")
  return(temp)
}




# national.mode() ---------------------------------------------------------


## Compute the dummy coding with -1 for observation with national mode in all dummies
## (n_cat - 1 dummies, n_cat incl. missing category)
# Input: vars -- variable (type irrelevant) which will be recoded
#        varname -- the variable name which the resulting variables should have
# Output: (n_cat - 1) variables with the values -1, 0 and 1. Variable names are 
#         varname.i (i in 1:(n_cat-1))
national.mode <- function(vars, varname){
  # If the variable is not present in the country, return NA
  if(all(is.na(vars))){
    return(NA)
  }
  
  ## Get all present values of variable in the country
  lev <- unique(vars)
  
  ## For studyProgramme remove countries with only one programme
  if(varname == "studyProgram" & length(lev) == 1){
    return(NA)
  }
  
  ## Get national modes
  m <- getmode(vars)
  
  ## Convert to general dummy data set with n dummies
  dat <- dummy_cols(vars)
  mat <- NULL
  
  ## Remove mode dummy and set mode as -1 for all columns
  ## Row-bind dummies independent of their value (OK, because only necessary
  ## for national use)
  # Test if NA is the most frequent value: Set NA or value as reference (-1)
  if(is.na(m)){
    dat[is.na(dat[, 1]), ] <- -1
  }else{
    dat[(!is.na(dat[, 1]) & dat[, 1] == m), ] <- -1
  }
  # Test again if NA is the most frequent value: Remove column of the most 
  # frequent value (as it is set as reference/-1 in all other columns)
  if(is.na(m)){
    dat[, which(is.na(lev))] <- NULL
  }else{
    dat[, which(lev == m)] <- NULL
  }
  dat[, 1] <- NULL
  
  ## Rename and return dataset
  colnames(dat) <- paste(varname, 1:(length(lev)-1), sep = ".")
  return(dat)
}




# compute.score() ---------------------------------------------------------


## Compute score of multiple questions according to contrast coding on pages 421-431.
## Ratio score of the achieved score to possible score (not incl. missing questions)
# Input: varname -- name of the new ratio score (corresponding to var_dat)
#        var_dat -- table with information for the computation of the new score 
#                   (variables in the dataset, new variable name, possible recoding 
#                   per variable, mode; see "conditioning_var_information.xlsx" for example)
#        dat -- data which contains the relevant variables for the score computation
# Output: two variables named varname.1 and varname.2 -- one missing dummy and other with the
#         value of the computed score

compute.score <- function(varname, var_dat = z, dat = pa12_stud){
  ## Find relevant variables for construct
  sub <- subset(var_dat, Varname == varname) 
  whi <- which(names(dat) %in% sub[, 1])
  
  ## Subset data to only include those variables
  d <- dat[, whi]
  
  ## Check if the variables are present in the country, else return NA
  if(all(is.na(d))){
    return(NA)
  }
  
  ## Remove big data set
  rm(dat)
  
  ## Recode variables according to variable information
  for(i in 1:dim(sub)[1]){
    d[, i] <- car::recode(d[, i], sub[i, 5])
  }
  
  ## Compute ratio score of the variables (NAs are not taken into account)
  new <- rowSums(d, na.rm = T) / (rowSums(!is.na(d)) * max(unique(d[, 1]), na.rm = T))
  new[is.nan(new)] <- NA
  
  ## Scale ratio score by mean and add missing dummy
  ret <- missing.dummy(new, varname, mode = sub[1, 4])
  
  ## Return data
  return(ret)
}




# national.max() ----------------------------------------------------------


## Compute maximal score of multiple variables according to p.429-431 TR2012. The
## maximum of each row for multiple variables is taken and national.mode() is applied.
# Input: varname -- name of the new maximum score (corresponding to var_dat)
#        var_dat -- table with information for the computation of the new score 
#                   (variables in the dataset, new variable name, possible recoding 
#                   per variable, mode; see "conditioning_var_information.xlsx" for example)
#        dat -- data which contains the relevant variables for the score computation
# Output: (n_cat - 1) variables with the values -1, 0 and 1. Variable names are 
#         varname.i (i in 1:(n_cat-1); n_cat incl. missing)

national.max <- function(varname, var_dat = w, dat = pa12_stud){
  ## Find relevant variable and subset data
  sub <- subset(var_dat, Varname == varname) 
  whi <- which(names(dat) %in% sub[, 1])
  d <- dat[, whi]
  rm(dat)
  
  ## Recode variables according to the variable information table
  for(i in 1:dim(sub)[1]){
    d[, i] <- car::recode(d[, i], sub[i, 5], as.factor = F)
  }
  
  ## Select the maximum value of the relevant variables in each row
  new <- matrixStats::rowMaxs(as.matrix(d), na.rm = T)
  new[is.infinite(new)] <- NA
  
  ## Apply national.mode() to variable with the maximums
  ret <- national.mode(new, sub[1, 3])
  
  ## Return
  return(ret)
}





# contrast.school.book() --------------------------------------------------


## Contrast coding for school and booklet ID as described on page 431 in TR2012
# Input: schoolvar -- school ID variable of one country
#        bookvar -- booklet identifier of one country
# Output: Variables containing the contrast codes for BOOKID and SCHOOLID for one
#         country
contrast.school.book <- function(schoolvar, bookvar, n_crit = 8){
  
  dats <- data.frame(schoolvar, bookvar)
  
  ## Recode SCHOOLID
  #Table of students, uh-booklets and other booklets per school
  tabs <- as.data.frame(dats %>% group_by(schoolvar) %>% 
                          summarise(anz = n(), uh = sum(bookvar %in% c(20, 70)),
                                    uhElse = sum(bookvar %in% c(1:13, 21:27, 71:74))))
  tabs$schoolvar <- as.numeric(tabs$schoolvar)
  
  # Identify small schools with certain features
  s1 <- tabs$schoolvar[((tabs$anz < n_crit) & (tabs$anz == tabs$uh))]
  s2 <- tabs$schoolvar[((tabs$anz < n_crit) & (tabs$uh == 0))]
  s3 <- tabs$schoolvar[((tabs$anz < n_crit) & (tabs$uh == tabs$uhElse))]
  neu <- data.frame(schoolvar = as.numeric(dats$schoolvar))
  
  # Recode these schools
  neu$schoolvar[neu$schoolvar %in% s2] <- 999999
  neu$schoolvar[neu$schoolvar %in% s1] <- 999998
  neu$schoolvar[neu$schoolvar %in% s3] <- 888888
  
  # Do the equivalent to national.mode() 
  neu <- dummy_cols(neu$schoolvar)
  m <- getmode(neu[, 1])
  w <- which(unique(neu[, 1]) == m) + 1
  neu[neu[, 1] == m, ] <- -1
  neu <- neu[, -c(1, w)]
  # Rename columns
  colnames(neu) <- paste("schoolid", 1:dim(neu)[2], sep = ".")
  
  ## Recode BOOKID
  #Similar to national.mode(), but 13 is always the "mode" and 20 is always set as 0
  lev <- unique(dats$bookvar)
  neu_b <- dummy_cols(dats$bookvar)
  neu_b[neu_b[, 1] == 13, ] <- -1
  neu_b[neu_b[, 1] == 20, ] <- 0
  neu_b <- neu_b[, -c(1, (which(lev %in% c(13, 20)) + 1))]
  colnames(neu_b) <- paste("bookid", 1:(dim(neu_b)[2]), sep = ".")
  
  ##Return both
  return(cbind(neu, neu_b))
}
