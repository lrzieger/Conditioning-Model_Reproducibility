

# Part 3 of PV computation: PVs with different conditioning variab --------

## In this part of the analysis, PVs are drawn.
## Additional models with variation of conditioning variables are computed here
## They are later compared to no and full conditioning

## Model 
# 0: No conditioning
# 1: School direct regressors 
# 2: Individual direct regressors
# 3: Indirect regressors
# 4: School and individual direct regressors
# 5: School direct and indirect regressors
# 6: Individual direct and indirect regressors
# 7: Full conditioning

## Read in PV helper functions
source("2_R-Syntax/4H_PV_helper.R")


# Load and prepare data and arguments -------------------------------------


## Select a country 
a <- "DEU"

## Choose a seed
seed_a <- 12345

## And the respective subset if necessary (country has more than 10000 
## participants) --> either "" or 1, 2, 3 or 4
b <- ""

## Read in the respective path for model from the step before
path <- list.files("3_Models/", recursive = T, full.names = T, 
                   pattern = paste0("PA12_PV-mod_", b, a, ".RData"))

## Load model and conditioning variables
input <- list(
  model = path,
  con.dat = "1_Data/Conditioning_variables.RData",
  part.dig = "1_Data/PA12_Digital_Participation.RData"
)

load(input$model)
load(input$con.dat)
load(input$part.dig)

## Reassign model to easier handle it in the computation
mod <- get(paste0("mod_", b, a))

## Subset the data set of the conditioning variables
con_dat <- con_data[[a]]

## Subset the digital participation file as well
dig_part <- subset(dig_part, CNT == a)

pvs <- list()


## Different versions of the conditioning variables
con_dat_opt <- list(
  school = c(1, which(startsWith(names(con_dat), "schoolid"))),
  back = which(!startsWith(names(con_dat), "schoolid") & !startsWith(names(con_dat), "PC")),
  pc = c(1, which(startsWith(names(con_dat), "PC"))),
  sb = which(!startsWith(names(con_dat), "PC")),
  sp = c(1, which(startsWith(names(con_dat), "schoolid") | startsWith(names(con_dat), "PC"))),
  bp = which(!startsWith(names(con_dat), "schoolid"))
)


## Decide for each country if a digital domain was administered.
## Depending on that, the plausible values are computed differently.
## Therefore jump to the appropriate section
# Plausible value computation: No digital domain --------------------------

if(!(dig_part$PS) | dig_part$Subset){
  
  pvs[[1]] <- try(run.pv(mod, conditioning = F, iter.2 = 10000,
                         seed.2 = seed_a, samp.regr.opt = F)$pv)
  
  pvs[[2]] <- try(run.pv(mod, con_dat[, con_dat_opt$school],
                         conditioning = T, iter.2 = 10000,
                         seed.2 = seed_a, samp.regr.opt = F)$pv)

  pvs[[3]] <- try(run.pv(mod, con_dat[, con_dat_opt$back],
                         conditioning = T, iter.2 = 10000,
                         seed.2 = seed_a, samp.regr.opt = F)$pv)

  pvs[[4]] <- try(run.pv(mod, con_dat[, con_dat_opt$pc],
                         conditioning = T, iter.2 = 10000,
                         seed.2 = seed_a, samp.regr.opt = F)$pv)

  pvs[[5]] <- try(run.pv(mod, con_dat[, con_dat_opt$sb],
                         conditioning = T, iter.2 = 10000,
                         seed.2 = seed_a, samp.regr.opt = F)$pv)

  pvs[[6]] <- try(run.pv(mod, con_dat[, con_dat_opt$sp], 
                         conditioning = T, iter.2 = 10000,
                         seed.2 = seed_a, samp.regr.opt = F)$pv)

  pvs[[7]] <- try(run.pv(mod, con_dat[, con_dat_opt$bp], 
                         conditioning = T, iter.2 = 10000,
                         seed.2 = seed_a, samp.regr.opt = F))
  
  pvs[[8]] <- try(run.pv(mod, con_dat, conditioning = T, iter.2 = 10000,
                         seed.2 = seed_a, samp.regr.opt = F)$pv)

}

# Plausible value computation: at least one digital domain ----------------

# Load 4-/6-dimensional model and then compute plausible values with different options, 
# but in two steps in compare to the no digital countries
if(dig_part$PS & !(dig_part$Subset)){

  
  # Find 4-/6-dim model 
  path2 <- list.files("3_Models/", recursive = T, full.names = T, 
                      pattern = paste0("PA12_PV-mod-md_", b, a, ".RData"))
  
  load(path2)
  mod_dig <- get(paste0(paste0("mod_md_", b, a)))
  
  pvs[[1]] <- try(run.pv.md(mod, conditioning = F, iter.2 = 10000,
                            seed.2 = seed_a, samp.regr.opt = F, mod2 = mod_dig)$pv)

  pvs[[2]] <- try(run.pv.md(mod, con_dat[, con_dat_opt$school], 
                            conditioning = T, iter.2 = 10000,
                            seed.2 = seed_a, samp.regr.opt = F, mod2 = mod_dig)$pv)

  pvs[[3]] <- try(run.pv.md(mod, con_dat[, con_dat_opt$back], 
                            conditioning = T, iter.2 = 10000,
                            seed.2 = seed_a, samp.regr.opt = F, mod2 = mod_dig)$pv)

  pvs[[4]] <- try(run.pv.md(mod, con_dat[, con_dat_opt$pc], 
                            conditioning = T, iter.2 = 10000,
                            seed.2 = seed_a, samp.regr.opt = F, mod2 = mod_dig)$pv)

  pvs[[5]] <- try(run.pv.md(mod, con_dat[, con_dat_opt$sb], 
                            conditioning = T, iter.2 = 10000,
                            seed.2 = seed_a, samp.regr.opt = F, mod2 = mod_dig)$pv)

  pvs[[6]] <- try(run.pv.md(mod, con_dat[, con_dat_opt$sp], 
                            conditioning = T, iter.2 = 10000,
                            seed.2 = seed_a, samp.regr.opt = F, mod2 = mod_dig)$pv)

  pvs[[7]] <- try(run.pv.md(mod, con_dat[, con_dat_opt$bp], 
                            conditioning = T, iter.2 = 10000,
                            seed.2 = seed_a, samp.regr.opt = F, mod2 = mod_dig)$pv)
  
  pvs[[8]] <- try(run.pv.md(mod, con_dat, conditioning = T, iter.2 = 10000,
                            seed.2 = seed_a, samp.regr.opt = F, mod2 = mod_dig)$pv)
  
}


# Save PV models and running time  ----------------------------------------


## Reassign a unique name to the plausible values for later
assign(paste0("pvs_", b, a), pvs)

## Save
save(list = paste0("pvs_", b, a), 
     file = paste0("3_Models/PA12_PV-pv-", b, a, ".RData"))
