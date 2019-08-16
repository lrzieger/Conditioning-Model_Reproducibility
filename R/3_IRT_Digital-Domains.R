
# Optional part 2 of PV computation: IRT model with digital domains -------

## This syntax is run for countries if they participate in either problem 
## solving and/or digital reading and maths. Not run for countries who only
## participate in maths, reading and science. 
## IRT model is computed per country for all available domains. 

# Load student responses including the digital domains, item difficulties 
# including those for digital domains, data set which country participates in 
# which domains and weight data
input <- list(
  responses = "1_Data/PA12_responses_digital.RData",
  tech.rep = "1_Data/2012_Reported_Item_Params_digital.RData",
  part.dig = "1_Data/PA12_Digital_Participation.RData",
  wgt.dat = "1_Data/PA12_weights.RData"
)

# Source IRT helper functions
source("2_R-Syntax/4_helper_PV_V5.R")


# Load and prepare data  --------------------------------------------------

### ONLY NECESSARY FOR COUNTRIES THAT PARTICIPATED IN AT LEAST 1 DIGITAL DOMAIN!!
## Select a country
a <- "DEU"

## Load and reduce data
load(input$responses)
load(input$part.dig)
pa12_digi <- subset(pa12_digi, CNT == a)

if(dim(pa12_digi)[1] > 10000){
  ## Weight data for a clean split (by strata)
  load(input$wgt.dat)
  
  ## Set number manually for the data set (b in 1, 2, 3, 4)
  b <- "XXX"
  
  ## Subset weight for the country
  pa12_wgt <- subset(pa12_wgt, CNT == a)
  
  ## Determine number of data sets (but only one is used in this script)
  anz <- ceiling(dim(pa12_digi)[1] / 10000)
  
  ## Order strata by number
  zw <- levels(droplevels(pa12_wgt$STRATUM))[order(table(droplevels(pa12_wgt$STRATUM)), 
                                                   decreasing = T)]
  
  ## Select observations of the alternating strata
  whi <- which(pa12_wgt$STRATUM %in% zw[seq(as.numeric(b), length(zw), by = anz)])
  
  ## Subset the data set
  pa12_digi <- pa12_digi[whi, ]
} else {
  b <- ""
}



# Compute IRT model -------------------------------------------------------

## Load item difficulties
load(input$tech.rep)

## Countries which only participate with a region in the digital domains are not
## considered here 

## Check if the country was participating only in Problem Solving. 
## If not set drm_opt to TRUE, so the participation can be considered in the 
## IRT model later
if(a %in% subset(dig_part, DRM & !Subset)$CNT){
  drm_opt <- T
}

## Run IRT model: Either 4- (math, scie, readig and problem solving) or 
## 6-dimensional (4 + digital reading and mathematics), no weights 
## Possibility to set seeds and iteration number in the function parameter
mod <- run.irt.md(pa12_digi, items = item.dif2, drm = drm_opt)



# Save the more dimensional models ----------------------------------------

assign(paste0("mod_md_", b, a), mod)

save(list = paste0("mod_md_", b, a), 
     file = paste0("3_Models/PA12_PV-mod-md_", b, a, ".RData"))
