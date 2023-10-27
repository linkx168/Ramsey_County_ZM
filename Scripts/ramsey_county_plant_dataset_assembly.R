#'---
#' title: "Plant Data Assembly"
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' This script will filter the full MN plants survey db for ramsey county lakes, then add in a bunch of other surveys that have been added since that project ended. 
#' 


#' # Document Preamble
#+ warning = FALSE

# load libraries ------------------------------------------------------------------
# # Load Libraries
library(data.table)
# update_dev_pkg()# remotes::install_github("Rdatatable/data.table")

# load in data -------------------------------------------------
# # 
# plants <- fread(file = "data&scripts/data/output/plants_env_data.csv", drop = 1)
# plants_occurrence_wide <- fread(file = "data&scripts/data/output/plants_env_data_wide.csv", drop = 1)
# plants_rakeabund_wide <- fread(file = "data&scripts/data/output/plants_abund_env_data_wide.csv")
surveys <- fread(file = "E:\\My Drive\\Documents\\UMN\\Grad School\\Larkin Lab\\R_projects\\MN_aquatic_plants_synthesis\\data&scripts\\data\\output\\surveys_aqplants.csv")
plants_db_new <- readRDS("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/MN_aquatic_plants_synthesis/data&scripts/data/input/plants_db_new.rds")



surveys[DOW %in% c(62000000:63000000)|
          LAKE_NAME == "white bear"|
          LAKE_NAME == "otter", .N , .(LAKE_NAME,year, DOW) ]

setDT(plants_db_new)
unique(plants_db_new$)

plants_db_new[ DOW %in% c(62000000:63000000, 82016700, 02000300 ), .N , .(DOW, SURVEY_START) ]

ramsey_co_plant_surveys <- surveys[DOW %in% c(62000000:63000000)|
                                     LAKE_NAME == "white bear",   ]



names(ramsey_co_plant_surveys)

ramsey_co_plant_surveys[ ,.N ,]
ramsey_co_plant_surveys[ , .N , SURVEY_ID] 
ramsey_co_plant_surveys[SUBBASIN != "" ,  , ]

ramsey_co_plant_surveys[ , .(SURVEY_ID, DOW, SUBBASIN, fw_id, DATASOURCE, LAKE_NAME, DATESURVEYSTART, MULTIPARTSURVEY, nobs, tot_n_samp, ), ]


#merge to invasion status
ramsey_co_plant_surveys[ , .N , SURVEY_ID] 


fwrite(surveys[DOW %in% c(62000000:63000000)|
                 LAKE_NAME == "white bear", .N , .(LAKE_NAME,year) ])
