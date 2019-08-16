library(TAM)




# run.irt() ---------------------------------------------------------------

## run.irt - Run the IRT model for one country in mathematics, science and reading
# Input: x_country - country data set (scored cognitive item responses)
#        items     - fix item difficulties to that values (must have the form 
#                    of 1_Get_Item-Params_TR.R) (also set to that by default)
#        seed.1    - seed.1 for the computation (with default value)
#        iter.1    - maximal number of computations (default = 10000)
# Output: IRT model from the function tam.mml (tam.mml obj)

run.irt <- function(x_country, items = item.dif, seed.1 = 32425474, iter.1 = 10000){
  # Save country name
  cnt <- x_country$CNT[1]
  
  # Remove columns/variables with constant value
  x_country <- Filter(function(x) (length(unique(x)) > 1), x_country)
  
  # Remove items from the item difficulties that do not occur in the country
  items <- items[items$item %in% colnames(x_country), ]
  
  ## Transform item difficulties to fit the parameterization option of TAM
  # Select item name domain and difficulties from table 
  tam.pars <- items[c("item", "domain", "Delta", "Tau1")]
  # We believe that Tau1 of PM155Q03D is a typo in the TR
  # We set it to the average value of all the cycles before
  tam.pars[which(tam.pars$item == "PM155Q03D"), ]$Tau1 <- 0.184
  # Append the item difficulties 
  xsi.set <- c(tam.pars$Delta, na.omit(tam.pars$Tau1))
  xsi.set <- cbind(1:length(xsi.set), xsi.set)
  
  # Set dimensions (Math, reading, science separately)
  Q <- matrix(0, dim(tam.pars)[1], 3)
  colnames(Q) <- c("MATH", "READ", "SCIE")
  Q[tam.pars$domain == "MATH", 1] <- 1    
  Q[tam.pars$domain == "READ", 2] <- 1  
  Q[tam.pars$domain == "SCIE", 3] <- 1  
  
  # Which variables/columns are used in the country (for subsetting the data 
  # before the algorithm)
  w.var <- which(names(x_country) %in% items$item)
  
  # Remove data with empty response pattern (some students did not answer a 
  # single question, only background questionnaire, etc.)
  dat.irt <- subset(x_country, rowSums(is.na(x_country[w.var])) != ncol(x_country[w.var]))
  
  # Set seed
  set.seed(seed.1)
  
  # Compute IRT model with parameterization "PCM2", fixed item difficulties, 
  # Q the matrix for the dimensions, StIDStd the personal identifier and 
  # optimization option Quasi-Monte-Carlo TRUE and 2000 stochastical nodes
  mod1 <- tam.mml(resp = dat.irt[w.var], 
                  irtmodel = "PCM2",
                  xsi.fixed = xsi.set,
                  Q = Q,
                  pid = dat.irt$StIDStd,
                  control = list(maxiter = iter.1,
                                 QMC = TRUE,
                                 snodes = 2000))
  
  # Return the model
  return(mod1)
}





# run.irt.md --------------------------------------------------------------

## run.irt.md - Run the IRT model for one country in four or six domains now:
##            mathematics, science, reading and problem solving and optional 
##            digital reading and mathematics
# Input: x_country - country data set (scored cognitive item responses)
#        items     - fix item difficulties to that values (must have the form 
#                    of 1_Get_Item-Params_TR.R) (also set to that by default)
#        seed.1    - seed.1 for the computation (with default value)
#        iter.1    - maximal number of computations (default = 10000)
#        drm       - problem solving and digital reading and maths (T) or 
#                    problem solving only 
# Output: IRT model from the function tam.mml (tam.mml obj)

run.irt.md <- function(x_country, items = item.dif2, seed.1 = 32425474,
                     iter.1 = 10000, drm = F){
  
  # Save country name
  cnt <- x_country$CNT[1]
  
  # Remove columns/variables with constant value
  x_country <- Filter(function(x) (length(unique(x)) > 1), x_country)
  
  # Remove items that do not occur in the country
  items <- items[items$item %in% colnames(x_country), ]
  
  ## Transform item difficulties to fit the parameterization option of TAM
  # Select item name domain and difficulties from table 
  tam.pars <- items[c("item", "domain", "Delta", "Tau1")]
  # We believe that Tau1 of PM155Q03D is a typo in the TR
  # We set it to the average value of all the cycles before
  tam.pars[which(tam.pars$item == "PM155Q03D"), ]$Tau1 <- 0.184
  # In digital reading, there is one parameter where a further step parameter is
  # required, additional steps for checking, extracting and buliding the item 
  # difficulties are needed. Problem solving only works the "regular" way.
  if(drm){
    nam_check <- tam.pars$item[!is.na(tam.pars$Tau1)]
    len_check <- lapply(x_country[nam_check], function(x) length(table(x)))
    if(any(len_check == 2)){
      nums <- which(len_check == 2)
      for(i in nums){
        tam.pars[which(tam.pars$item == nam_check[i]), ]$Tau1 <- NA
      }
    }
    xsi.set <- c(tam.pars$Delta, na.omit(tam.pars$Tau1))
    if(any(len_check == 4)){
      xsi.set <- c(xsi.set, -1.13453)
    }
  } else {
    xsi.set <- c(tam.pars$Delta, na.omit(tam.pars$Tau1))
  }
  xsi.set <- cbind(1:length(xsi.set), xsi.set)
  
  # Set dimensions (Math, reading, science and the digital domains)
  if(drm) {
    Q <- matrix(0, dim(tam.pars)[1], 6)
    colnames(Q) <- c("MATH", "READ", "SCIE", "CP", "CM", "CR")
    Q[tam.pars$domain == "MATH", 1] <- 1    
    Q[tam.pars$domain == "READ", 2] <- 1  
    Q[tam.pars$domain == "SCIE", 3] <- 1 
    Q[tam.pars$domain == "CP", 4] <- 1 
    Q[tam.pars$domain == "CM", 5] <- 1 
    Q[tam.pars$domain == "CR", 6] <- 1 
  } else {
    Q <- matrix(0, dim(tam.pars)[1], 4)
    colnames(Q) <- c("MATH", "READ", "SCIE", "CP")
    Q[tam.pars$domain == "MATH", 1] <- 1    
    Q[tam.pars$domain == "READ", 2] <- 1  
    Q[tam.pars$domain == "SCIE", 3] <- 1 
    Q[tam.pars$domain == "CP", 4] <- 1 
  }
  
  # Which variables/columns are used in the country (for subsetting the data 
  # before the algorithm)
  w.var <- which(names(x_country) %in% items$item)
  
  # Remove data with empty response pattern (some students did not answer a 
  # single question, only background questionnaire, etc.)
  dat.irt <- subset(x_country, rowSums(is.na(x_country[w.var])) != ncol(x_country[w.var]))
  
  # Set seed
  set.seed(seed.1)
  
  # Compute IRT model with parameterization "PCM2", fixed item difficulties, 
  # Q the matrix for the dimensions, StIDStd the personal identifier and 
  # optimization option Quasi-Monte-Carlo TRUE and 2000 stochastical nodes
  # and Ramsay acceleration
  mod1 <- tam.mml(resp = dat.irt[w.var], 
                  irtmodel = "PCM2",
                  xsi.fixed = xsi.set,
                  Q = Q,
                  pid = dat.irt$StIDStd,
                  control = list(maxiter = iter.1,
                                 QMC = TRUE,
                                 snodes = 2000, 
                                 acceleration = "Ramsay"))
  return(mod1)
}


