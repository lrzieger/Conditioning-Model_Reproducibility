# Code for "Conditioning: How background variables can influence PISA scores"

## General 

In this github repository, all information for reproducing the analysis in the paper can be found. 

This code is made to share the exact steps and properties of the computation in the paper. Due to high computational effort, parts of the computations were run on a high performance computing cluster. The published code does not include these parts, but is simplified in the way that it only covers one country as an example (the code and computations are the same).

The code is written with the following folder structure in mind:

- 0_Raw-Data
- 1_Data
- 2_R-Syntax
- 3_Models
- 4_Results

All R code of this repository is supposed to be put in the folder "2_R-Syntax".

## 0_Raw-Data

All data that is used in this paper is publicly available. 
As a result, no data sets are hosted here. Please download and prepare the following files.
All files are downloaded from the [PISA homepage](http://www.oecd.org/pisa/data/pisa2012database-downloadabledata.htm).

- _PISA cognitive scored data_: Download the file "Scored cognitive item response data file " and the belonging sps-file. Read in the data in SPSS and save as .sav-file with the same name ("INT_COG12_S_DEC03.sav").
- _PISA cogintive scored data digital domains_: Download the file "Scored cognitive item response data file" and the belonging sps-file from the subsection [PISA CBA 2012 dataset download page](http://www.oecd.org/pisa/pisaproducts/database-cbapisa2012.htm). Read in the data with SPSS and save as .sav-file with the same name ("CBA_COG12_S_MAR31.sav").
- _PISA student questionnaire_: Download the file "Student questionnaire data file" and the belongign sps-file. Read in the data in SPSS and save as .sav-file with the same name ("INT_STU12_DEC03.sav").
- _PISA parent questionnaire_: Download the file "Parent questionnaire data file" and the belonging sps-file. Read in the data in SPSS and save as .sav-file with the same name ("INT_PAQ12_DEC03.sav").
- _Technical report_: Also please download the [technical report](https://www.oecd.org/pisa/pisaproducts/PISA-2012-technical-report-final.pdf) and save it under the name "PISA_2012_TR.pdf" in "0_Raw-Data".

## 1_Data 

Some information is already prepared/gathered from the technical report or as general information.
Please download the files in "Additional-data" and put it in the folder "1_Data" on your device.

The data which prepared throughout the computations will be saved in this folder.

The following things are provided:

- _Conditioning_var_information.xlsx_: The information from the pages 421-431 of the technical report, which describe the preparation of the conditioning variables, are prepared here. The columns are preapred in style for functions used in the code.
- _PA12_Digital_Participation.RData_: Table describing which countries participate in which domain (PS = Problem Solving and DRM = Digital Reading and Mathematics) and if they participate with whole sample or just a subset (Subset).
- _OECD_membership_date.RData_: Table containing the ISO 3166 numeric and alpha numeric codes as well as the entry data for all OECD countries. 
- _Booklet_Effects.xlsx_: The booklet effects which are applied later on to the plausible. They are not self-computed but taken from the technical report page 242.

## 2_R-Syntax

All code in the folder "R" should be downloaded and put in "2_R-Syntax".

The R code is numbered for a reason. The R code should be computed in that order. 
Files beginning with same number belong to the same step can be computed in any order. 
Files with an "H" after the number are helper-functions, which do not need run by the user, but other R code will call/source it. 

Generally, the R code numbers belong to the following steps:

- 0: Data preparation and checks on the cognitive items (and weights)
- 1: Read-in item parameter from the technical report
- 2: Prepare conditioning data
- 3: Run IRT models
- 4: Compute latent regression and plausible values 
- 5: Transform plausible values onto PISA scale
- Afterwards the plausible can be analysed, we used the plausible values in combination with "PA12_weights.RData" and the package [intsvy](https://cran.r-project.org/web/packages/intsvy/intsvy.pdf) to account for the complex sampling. 

## 3_Models

The models which are used and reused during the computations are stored here (automatically by the code - no need to do anything).

## 4_Results

Final data and results are stored here (automatically by the code - no need to do anything).


