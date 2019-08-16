
# Combine the PVs and transform them  -------------------------------------

##

## Set working direction and load the data
# pisa.pv: reported plausible values
# own.pv: self-computed plausible values
# booklets: reported booklet effects from the TR 2012
# book.dat: information about the booklet each student got
# wgt.dat: weights for each student

## Select countries 
a <- "DEU"

## Select subset if more than 10000 participating students (b is then 1, 2, ...)
b <- ""

input <- list(pisa.pv = paste0("1_Data/PA12_PV-pv", b, a, ".RData"),
              own.pv = "3_Models/PA12_Combined_PVs_final.RData",
              booklets = "1_Data/Booklet_Effects.xlsx",
              book.dat = "1_Data/PA12_bookid_dat.RData",
              wgt.dat = "1_Data/PA12_weights.RData")

load(input$pisa.pv)
load(input$own.pv)

## Model 
# 0: No conditioning
# 1: School direct regressors 
# 2: Individual direct regressors
# 3: Indirect regressors
# 4: School and individual direct regressors
# 5: School direct and indirect regressors
# 6: Individual direct and indirect regressors
# 7: Full conditioning

load(input$wgt.dat)

## Load required packages
library(dplyr)


#-----------------------------------------------------------------------------------------

## Make equivalent to pid in pvs
pa12_pv$pid <- with(pa12_pv, ave(rep(1, nrow(pa12_pv)), CNT, FUN = seq_along))
pa12_wgt$pid <- with(pa12_wgt, ave(rep(1, nrow(pa12_wgt)), CNT, FUN = seq_along))

## Rearrange
pa12_pv <- pa12_pv[, c(2, 20, 5:19)]
sfx <- paste0(".v", 0:7)
# 0: Nothing
# 1: SchoolID
# 2: Background; direct regressors
# 3: PCA
# 4: SchoolID and Background
# 5: SchoolID and PCA
# 6: Background and PCA
# 7: Full mod

## Combine the different plausible values of the list to one data.frame 
pvs_comb <- pvs[[1]]
for(i in c(1:7)) {
  pvs_comb <- merge(pvs_comb, pvs[[i+1]], all = TRUE, 
                    suffixes = sfx[i:(i+1)], by = c("cnt", "pid"))
}


## Extract country only (larger samples were split into Xcnt)
pvs_comb$cnt <- stringr::str_sub(pvs_comb$cnt, -3, -1)
# Rearrange plausible values, so domains are displayed together
pvs_comb <- pvs_comb[, c(1, 2, seq(from = 3, to = 242, by = 6), 
                         seq(from = 4, to = 242, by = 6), 
                         seq(from = 5, to = 242, by = 6), 
                         seq(from = 6, to = 242, by = 6), 
                         seq(from = 7, to = 242, by = 6), 
                         seq(from = 8, to = 242, by = 6))]

## Merge reported plausible values to the data set
pvs_comb <- merge(pa12_pv, pvs_comb, by.x = c("CNT", "pid"), by.y = c("cnt", "pid"),
                  all = T)

## Merge weights to data set 
pvs_comb <- merge(pvs_comb, pa12_wgt, all = T)

## Remove all (now) unnecessary data
rm(pa12_pv, pvs, pa12_wgt)


#-----------------------------------------------------------------------------------------------#
# Bring PVs on common scale 
# 1. Add booklet parameters from the technical report
# 2. Transform plausible values onto the common scale. Equations from TR

# Load booklet parameter and data
book_eff <- xlsx::read.xlsx(input$booklets, 1)
load(input$book.dat)

# Add indicator if book belongs to the easier booklets
book_eff$EASY <- ifelse(book_eff$Set. == "Easy", 1, 0)


# Combine booklet data and plausible values
pvs_comb <- merge(pvs_comb, book_dat)


############################
### Add booklet parameters

## Add mathematics booklet parameter
for(i in 1:26){
  whi_dom <- which(stringr::str_detect(names(pvs_comb), "Dim1"))
  whi_stud <- which(pvs_comb$BOOKID == book_eff$Number[i] & pvs_comb$EASY == book_eff$EASY[i])
  for(j in whi_dom){
    pvs_comb[whi_stud , j] <- pvs_comb[whi_stud, j] + book_eff$Mathematics[i]
  }
}

## Add reading booklet parameters
for(i in which(!(is.na(book_eff$Reading.)))){
  whi_dom <- which(stringr::str_detect(names(pvs_comb), "Dim2"))
  whi_stud <- which(pvs_comb$BOOKID == book_eff$Number[i] & pvs_comb$EASY == book_eff$EASY[i])
  for(j in whi_dom){
    pvs_comb[whi_stud , j] <- pvs_comb[whi_stud, j] + book_eff$Reading.[i]
  }
}

## Add science booklet parameters
for(i in which(!(is.na(book_eff$Science)))){
  whi_dom <- which(stringr::str_detect(names(pvs_comb), "Dim3"))
  whi_stud <- which(pvs_comb$BOOKID == book_eff$Number[i] & pvs_comb$EASY == book_eff$EASY[i])
  for(j in whi_dom){
    pvs_comb[whi_stud , j] <- pvs_comb[whi_stud, j] + book_eff$Science[i]
  }
}


###############################
### Do the transformations

## Mathematic transformation (see TR 2012)
whi_math <- which(stringr::str_detect(names(pvs_comb), "Dim1"))
pvs_comb[, whi_math] <- ((pvs_comb[, whi_math] + 0.0981 - 0.07) / 1.2838) * 100 + 500

## Reading transformation -- split by gender (see TR 2012)
whi_read <- which(stringr::str_detect(names(pvs_comb), "Dim2"))
pvs_comb[, whi_read] <- (pvs_comb[, whi_read] - 0.0274) 
whi_fem <- which(pvs_comb$female == 1)
whi_male <- which(pvs_comb$female == 0)
pvs_comb[whi_fem, whi_read] <- ((0.8739 * pvs_comb[whi_fem, whi_read] - 0.4655) / 1.1002) * 100 + 500
pvs_comb[whi_male, whi_read] <- ((0.88823 * pvs_comb[whi_male, whi_read] - 0.5427) / 1.1002) * 100 + 500

## Science transformation (see TR 2012)
whi_scie <- which(stringr::str_detect(names(pvs_comb), "Dim3"))
pvs_comb[, whi_scie] <- ((pvs_comb[, whi_scie] - 0.1646) / 1.0724) * 100 + 500


##############################

## Remove domains which we are not interested in (PS, DR, DM)
whi_not <- which(stringr::str_detect(names(pvs_comb), "Dim4") |
                 stringr::str_detect(names(pvs_comb), "Dim5") |
                 stringr::str_detect(names(pvs_comb), "Dim6"))

pvs_comb <- pvs_comb[, -whi_not]


####################################################################################

## Save data 
save(pvs_comb,  file = "3_Results/Data/PVs_transformed_final.RData")
