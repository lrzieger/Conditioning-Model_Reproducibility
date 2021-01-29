library(TAM)


# run.pv() ----------------------------------------------------------------

## run.pv - Function to be compute the plausible values for countries which only
##          administered mathematics, reading and science
## Input: mod1    - IRT model with mathematics, reading and science (tam.mml())
#         con_dat - conditioning data (necessary if conditioning is used)
#         seed.2  - seed for the computations
#         iter.2  - maximal number of iterations (default 10000)
#         n.pv    - number of plausible values that are drawn (default 5)
#         conditioning  - logical: Should conditioning be used (default TRUE)
#         samp.regr.opt - logical: Should regression coefficients be sampled in 
#                         the plausible value imputation or fixed (default F)
#         books.zero    - logical: Should the latent regression coefficients for
#                         the booklet IDs which only cover two domains be set to 
#                         0 for the third
## Output: Object of tam.pv - Values from the computation incl. the PVs 

run.pv <- function(mod1, con_dat = NULL, seed.2 = 4385697, iter.2 = 10000, n.pv = 5, 
                   conditioning = T,  samp.regr.opt = F, books.zero = F){
  
  ## Check that conditioning data is present when conditioning is done and 
  ## remove constant columns
  if(conditioning){
    stopifnot(!is.null(con_dat))
    con_dat <- con_dat[mod1$pid, ]
    con_dat <- con_dat[, unlist(lapply(con_dat, function(x) dplyr::n_distinct(x) > 1))]
  }
  
  ## Set seed for the computations
  set.seed(seed.2)
  
  ## Select whether conditioning should be used for the computation of plausible 
  ## values or not
  if(conditioning){
    
    # Extract IRT likelihood
    likeli <- IRT.likelihood(mod1)
    
    # Extract personal IDs
    pid.sele <- mod1$pid
    
    # Check whether the regression coefficient for the booklets which do not 
    # contribute to a domain should be set a zero
    if(books.zero){
      
      # Set the latent regression coefficients for booklet 1, 5, 7 and 10 for reading and
      # booklet 4, 6, 9 and 11 for science to 0
      betas <- data.frame(var = c(which(names(con_dat) %in% paste0("bookid.", c(1, 5, 7, 10))),
                                  which(names(con_dat) %in% paste0("bookid.", c(4, 6, 9, 11)))),
                          dim = rep(2:3, each = 4),
                          value = 0)
      
      # Compute latent regression of the latent ability on the conditioning 
      # variables (excl. the first column which is the student ID)
      latreg <- tam.latreg(likeli, Y = con_dat[, -1], pid = pid.sele,
                           control = list(maxiter = iter.2, acceleration = "Ramsay"),
                           beta.fixed = as.matrix(betas))
    } else {
      # Compute latent regression of the latent ability on the conditioning 
      # variables (excl. the first column which is the student ID)
      latreg <- tam.latreg(likeli, Y = con_dat[, -1], pid = pid.sele,
                           control = list(maxiter = iter.2, acceleration = "Ramsay"))
    }
    
    # Draw 5 plausible values for each student out of the resulting distribution
    # which is assumed to be normal distributed
    pvs    <- tam.pv(latreg, nplausible = 5, normal.approx = T, samp.regr = samp.regr.opt)
  } else {
    # Draw 5 plausible values for each student out of the personal distributed, 
    # which is assumed to be normal distributed, but not influenced by background
    # variables
    pvs    <- tam.pv(mod1, nplausible = 5, normal.approx = T, samp.regr = samp.regr.opt)
  }
  
  
  ## In the end, return plausible values
  return(pvs)
}



# run.pv.md() -------------------------------------------------------------

## run.pv.md - Function to be compute the plausible values for countries which 
##          administered mathematics, reading, science and at least Problem
##          Solving
## Input: mod1    - IRT model with mathematics, reading and science (tam.mml())
#         mod2    - IRT model with mathematics, reading, science and digital 
#                   domain (tam.mml())
#         con_dat - conditioning data (necessary if conditioning is used)
#         seed.2  - seed for the computations
#         iter.2  - maximal number of iterations (default 10000)
#         n.pv    - number of plausible values that are drawn (default 5)
#         conditioning  - logical: Should conditioning be used (default TRUE)
#         samp.regr.opt - logical: Should regression coefficients be sampled in 
#                         the plausible value imputation or fixed (default F)
## Output: Object of tam.pv - Values from the computation incl. the PVs 

run.pv.md <- function(mod1, con_dat = NULL,  seed.2 = 4385697, iter.2 = 5000, 
                      mod2 = mod_dig, n.pv = 5, conditioning = T, 
                      samp.regr.opt = F){
  
  con_dat1 <- con_dat
  
  ## Check that conditioning data is present when conditioning is done and 
  ## remove constant columns
  if(conditioning){
    stopifnot(!is.null(con_dat))
    con_dat <- con_dat[mod2$pid, ]
    con_dat <- con_dat[, unlist(lapply(con_dat, function(x) dplyr::n_distinct(x) > 1))]
    con_dat1 <- con_dat1[mod1$pid, ]
    con_dat1 <- con_dat1[, unlist(lapply(con_dat1, function(x) dplyr::n_distinct(x) > 1))]
  }

  ## Set seed for the computations
  set.seed(seed.2)
  
  ## Select whether conditioning should be used for the computation of plausible 
  ## values or not
  if(conditioning){
    
    # Extract IRT likelihood of the first model (only math, read and scie)
    likeli <- IRT.likelihood(mod1)
    
    # Compute latent regression of the latent ability on the conditioning 
    # variables (excl. the first column which is the student ID)
    latreg <- tam.latreg(likeli, Y = con_dat1[, -1], 
                         control = list(maxiter = iter.2, acceleration = "Ramsay"))
    
    # Extract the regression coefficients of the conditioning variables, because
    # they are fixed for the core domains in the next step at that value
    reg.coefs <- cbind(rep(1:dim(latreg$beta)[1], 3), 
                       rep(1:3, each = dim(latreg$beta)[1]),
                       c(latreg$beta[, 1], latreg$beta[, 2], latreg$beta[, 3]))
    
    # Extract IRT likelihood of the second model (math, read and scie plus digital
    # domains)
    likeli_md <- IRT.likelihood(mod2)
    
    # Extract personal IDs
    pid.sele <- mod2$pid
    
    # Compute latent regression of the latent ability on the conditioning 
    # variables (excl. the first column which is the student ID). But this time
    # only the regression coefficients of the digital domains are computed freely.
    # The rest is fixed at the values of the first model
    latreg_md <- tam.latreg(likeli_md, Y = con_dat[, -1],  pid = pid.sele, 
                            control = list(maxiter = iter.2, acceleration = "Ramsay"),
                            beta.fixed = reg.coefs)
    
    # Draw 5 plausible values for each student out of the resulting distribution
    # which is assumed to be normal distributed
    pvs    <- tam.pv(latreg_md, nplausible = 5, normal.approx = T, samp.regr = samp.regr.opt)
  } else {
    # Draw 5 plausible values for each student out of the personal distributed, 
    # which is assumed to be normal distributed, but not influenced by background
    # variables.
    # Only the second model is used because there is no need to fix regression
    # coefficients with the first model (only math, read and scie)
    pvs    <- tam.pv(mod2, nplausible = 5, normal.approx = T, samp.regr = samp.regr.opt)
  }
  
  
  ## In the end, return plausible values
  return(pvs)
}
