
# Extract item parameter from TR ------------------------------------------

## The published item parameter from the technical report are needed for this 
## project. They are extracted directly from the PDF rather than copypasting.

library(tabulizer)
library(dplyr)
library(stringr)

# Item paramter: Main domains ---------------------------------------------

## Read in PDF-pages of the technical report that contain the item parameters.
## The technical report can download from
## https://www.oecd.org/pisa/pisaproducts/PISA-2012-technical-report-final.pdf
tabs <- extract_tables("0_Raw-Data/PISA_2012_TR.pdf",
                       pages = seq(409, 415, 2), method = "stream")

## Rearrange the data and add the pages into one big data set 
item.dif <- data.frame(item = c(tabs[[1]][-(1:4), 1], 
                                tabs[[2]][-(1:4), 1], 
                                tabs[[3]][-(1:4), 1], 
                                tabs[[4]][-(1:4), 1]),
                       domain = c(rep("MATH", length(tabs[[1]][-(1:4), 1])),
                                  rep("MATH", length(tabs[[2]][-(1:4), 1])),
                                  rep("READ", length(tabs[[3]][-(1:4), 1])),
                                  rep("SCIE", length(tabs[[4]][-(1:4), 1]))),
                       cluster = c(tabs[[1]][-(1:4), 4], 
                                   tabs[[2]][-(1:4), 4],
                                   tabs[[3]][-(1:4), 5], 
                                   tabs[[4]][-(1:4), 3]),
                       perc.correct = c(tabs[[1]][-(1:4), 5], 
                                        tabs[[2]][-(1:4), 5],
                                        tabs[[3]][-(1:4), 6], 
                                        tabs[[4]][-(1:4), 4]),
                       se.correct = c(tabs[[1]][-(1:4), 6], 
                                      tabs[[2]][-(1:4), 6],
                                      tabs[[3]][-(1:4), 7], 
                                      tabs[[4]][-(1:4), 5]),
                       orig.parameter = c(tabs[[1]][-(1:4), 7], 
                                          tabs[[2]][-(1:4), 7],
                                          tabs[[3]][-(1:4), 8], 
                                          tabs[[4]][-(1:4), 6]),
                       orig.threshold = c(tabs[[1]][-(1:4), 8], 
                                          tabs[[2]][-(1:4), 8],
                                          tabs[[3]][-(1:4), 9], 
                                          tabs[[4]][-(1:4), 7]))  



## Remove the brackets from the percentage correct
item.dif$se.correct <- substr(item.dif$se.correct, 2, 5)


## Multiple item parameters and thresholds were in one cell. Split them into separate
## columns 
item.dif <- cbind(item.dif, str_split_fixed(item.dif$orig.parameter, " ", 3))
item.dif <- cbind(item.dif, str_split_fixed(item.dif$orig.threshold, " ", 2))

## Rename the columns fittingly 
colnames(item.dif) <- c(colnames(item.dif)[1:7], "Delta", "Tau1", "Tau2", 
                        "Threshold1", "Threshold2")



## Change the type of vector. Nearly all factor --> Change into character or number 
item.dif <- as.data.frame(lapply(item.dif, as.character), stringsAsFactors = F)
item.dif[, c(4, 5, 8:12)] <- as.data.frame(lapply(item.dif[, c(4, 5, 8:12)], as.numeric))

## Save item data set
save(item.dif, 
     file = "1_Data/2012_Reported_Item_Params.RData")




# Item parameter: Digital domains -----------------------------------------


## Read in PDF-pages of the technical report that contain the item parameters
## for the digital domains. The technical report can download from
## https://www.oecd.org/pisa/pisaproducts/PISA-2012-technical-report-final.pdf
tabs <- extract_tables("0_Raw-Data/PISA_2012_TR.pdf",
                       pages = c(416, 418, 420), method = "stream")

item.dif2 <- data.frame(item = c(tabs[[2]][-(1:4), 1], tabs[[3]][-(1:4), 1], 
                                 tabs[[4]][-(1:4), 1]),
                        domain = c(rep("CR", length(tabs[[2]][-(1:4), 1])),
                                   rep("CM", length(tabs[[3]][-(1:4), 1])),
                                   rep("CP", length(tabs[[4]][-(1:4), 1]))),
                        cluster = c(tabs[[2]][-(1:4), 4], 
                                    tabs[[3]][-(1:4), 3], 
                                    tabs[[4]][-(1:4), 3]),
                        perc.correct = c(tabs[[2]][-(1:4), 5], 
                                         tabs[[3]][-(1:4), 4], 
                                         tabs[[4]][-(1:4), 4]),
                        se.correct = c(tabs[[2]][-(1:4), 6],
                                       tabs[[3]][-(1:4), 5], 
                                       tabs[[4]][-(1:4), 5]),
                        orig.parameter = c(paste(tabs[[2]][-(1:4), 7], 
                                                 tabs[[2]][-(1:4), 8]),
                                           tabs[[3]][-(1:4), 6],  
                                           tabs[[4]][-(1:4), 6]),
                        orig.threshold = c(tabs[[2]][-(1:4), 10], 
                                           tabs[[3]][-(1:4), 7], 
                                           tabs[[4]][-(1:4), 7]))  



## Remove the brackets from the percentage correct
item.dif2$se.correct <- substr(item.dif2$se.correct, 2, 5)

## Multiple item parameters and thresholds were in one cell. Split them into separate
## columns 
item.dif2 <- cbind(item.dif2, str_split_fixed(item.dif2$orig.parameter, " ", 3))
item.dif2$Tau3 <- ifelse(item.dif2$item == "CR021Q08", 0.74200, NA)
item.dif2 <- cbind(item.dif2, str_split_fixed(item.dif2$orig.threshold, " ", 3))

## Rename the columns fittingly 
colnames(item.dif2) <- c(colnames(item.dif2)[1:7], "Delta", "Tau1", "Tau2", "Tau3", 
                         "Threshold1", "Threshold2", "Threshold3")

## Change the type of vector. Nearly all factor --> Change into character or number 
item.dif2 <- as.data.frame(lapply(item.dif2, as.character), stringsAsFactors = F)
item.dif2[, c(4, 5, 8:14)] <- as.data.frame(lapply(item.dif2[, c(4, 5, 8:14)], as.numeric))

## Add item.dif2 to item.dif
item.dif2 <- plyr::rbind.fill(item.dif, item.dif2)

save(item.dif2, file = "1_Data/2012_Reported_Item_Params_digital.RData")
