
# Part 1 of PV computation: IRT models ------------------------------------

## Compute IRT model for each country consisting of maths, reading, and science
## Set item difficulties at published values

# Load prepared student response data, published item difficulties and 
# PISA weight data (for splitting countries with more than 10000 students)
input <- list(
  responses = "1_Data/PA12_responses_scored_prepared.RData",
  tech.rep = "1_Data/2012_Reported_Item_Params.RData",
  wgt.dat = "1_Data/PA12_weights.RData"
)

# Source IRT helper function
source("2_R-Syntax/3H_IRT_helper.R")



# Preparation: Load and prepare data --------------------------------------

## Select a country
a <- "DEU"

## Load and reduce data (select country)
load(input$responses)
pa12_resp_s <- subset(pa12_resp_s, CNT == a)

## Split the data if necessary (more than 10000 students)
## Need to specify the subset of the country (e.g country with 15000 --> 2 data 
## sets, country with 28000 -> 3 data sets)

if(dim(pa12_resp_s)[1] > 10000){
  ## Weight data for a clean split (by strata)
  load(input$wgt.dat)
  
  ## Set number manually for the data set (b in 1, 2, 3, 4)
  b <- "XXX"
  
  ## Subset weight for the country
  pa12_wgt <- subset(pa12_wgt, CNT == a)
  
  ## Determine number of data sets (but only one is used in this script)
  anz <- ceiling(dim(pa12_resp_s)[1] / 10000)
  
  ## Order strata by number
  zw <- levels(droplevels(pa12_wgt$STRATUM))[order(table(droplevels(pa12_wgt$STRATUM)), 
                                                  decreasing = T)]
  
  ## Select observations of the alternating strata
  whi <- which(pa12_wgt$STRATUM %in% zw[seq(as.numeric(b), length(zw), by = anz)])
  
  ## Subset the data set
  pa12_resp_s <- pa12_resp_s[whi, ]
} else {
  b <- ""
}



# Run IRT model: Multi-dim model for math, read and scie ------------------

## Load item difficulties
load(input$tech.rep)

## Run IRT model: 3-dim model for math, read  and scie, no weights 
## Possibility to set seeds and iteration number in the function parameter
mod <- run.irt(pa12_resp_s, items = item.dif)



# Save model --------------------------------------------------------------

assign(paste0("mod_", b, a), mod)

save(list = paste0("mod_", b, a), 
     file = paste0("3_Models/PA12_PV-mod_", b, a, ".RData"))
