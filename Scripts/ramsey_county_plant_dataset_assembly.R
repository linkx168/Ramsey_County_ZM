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
library(janitor)



# load in data -------------------------------------------------
# # 
# plants <- fread(file = "data&scripts/data/output/plants_env_data.csv", drop = 1)
# plants_occurrence_wide <- fread(file = "data&scripts/data/output/plants_env_data_wide.csv", drop = 1)
# plants_rakeabund_wide <- fread(file = "data&scripts/data/output/plants_abund_env_data_wide.csv")
surveys <- fread(file = "E:\\My Drive\\Documents\\UMN\\Grad School\\Larkin Lab\\R_projects\\MN_aquatic_plants_synthesis\\data&scripts\\data\\output\\surveys_aqplants.csv")

#unprocessed plant data
plants_db_new <- readRDS("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/MN_aquatic_plants_synthesis/data&scripts/data/input/plants_db_new.rds")

plants_db_new <- clean_names(plants_db_new)

cols <- c("submitter_name", "submitter_email", "submit_time",
          "dow", "survey_start", "surveyors", 
          "rake_max", 
          "sta_nbr", "x", "sta_nbr_1", "sample_type", "guid", "latitude", "longitude", 
          "depth", "depth_units", "substrate",  "depth_1", 
          "whole_rake_density", "no_veg_found"
          
          )

setcolorder(plants_db_new, cols)

plants_db_new[ , .N , .(is.na(depth), is.na(depth_1), no_veg_found) ]


# collapse new surveys to match the surveys format: -----------------------

#drop points without depth data
plants_db_new <- plants_db_new[ !is.na(depth)|!is.na(depth_1) , , ]

# depths?
    plot(depth~depth_1, data = plants_db_new[ !is.na(depth) & !is.na(depth_1), .(depth, depth_1) ,])
    
    plants_db_new[ (!is.na(depth) & !is.na(depth_1)) & depth_1>depth, .(depth, depth_1) ,]
    
    #meters in depth
    plot(depth~depth_1, data = plants_db_new[ (!is.na(depth) & !is.na(depth_1)) & (as.numeric(depth_1)/as.numeric(depth))< 1, .(depth, depth_1) ,])
    #feet in depth
    plot(depth~depth_1, data = plants_db_new[ (!is.na(depth) & !is.na(depth_1)) & (as.numeric(depth_1)/as.numeric(depth))> 1, .(depth, depth_1) ,])
    
    #make explicit column
    plants_db_new[ (!is.na(depth) & !is.na(depth_1)) & (as.numeric(depth_1)/as.numeric(depth))> 1, depth_feet := depth_1 ,]
    plants_db_new[ (!is.na(depth) & !is.na(depth_1)) & (as.numeric(depth_1)/as.numeric(depth))< 1, depth_feet := depth_1*3.28084 ,]
    plants_db_new[  , .N , depth_units      ]
    plants_db_new[ depth_units == "Meters" , depth_feet := as.numeric(depth) * 3.28084 , ]
    plants_db_new[ depth_units == "Feet" , depth_feet := as.numeric(depth) , ]
    plants_db_new[ , summary(depth_feet) ,   ]
    
    plants_db_new[ is.na(depth_feet), .N , .(depth, depth_1)] #revisit these to fix them up [someday! not now]
    
# survey ident
    plants_db_new[ , .N ,  .(dow, survey_start)]
    plants_db_new[ , survey_ident := .GRP ,  .(dow, survey_start)]    
    
#point idents
    #given valus:
    plants_db_new[ , .N , .(is.na(sta_nbr), is.na(sta_nbr_1), is.na(x), is.na(latitude), is.na(longitude)) ]
    #lets build our own
    plants_db_new[ , point_ident := seq_len(.N)  , .(survey_ident) ]
    
    
#organize:
    cols <- c("submitter_name", "submitter_email", "submit_time",
              "dow", "survey_start", "survey_ident",
              "rake_max", "surveyors", "sta_nbr", "x", "sta_nbr_1", "sample_type", "guid", "latitude", "longitude", "point_ident", "x_2", "y",
              "depth", "depth_units", "substrate",  "depth_1", "depth_feet", 
              "whole_rake_density", "no_veg_found", "plant_height", "plant_height_1", "percent_biovolume",
              "please_provide_comment"
              
    )
              
    
    setcolorder(plants_db_new, cols)
    taxalist <- names(plants_db_new)[!names(plants_db_new)%in%cols]
    
#melt long:
    plants_new_long <- melt(plants_db_new, id.vars = cols , variable.name = c( "taxon"))
    plants_new_long[ , .N , value]
    plants_new_long <- plants_new_long[!is.na(value)]
    
    #looks like some depths or other zany things made it in here:
    plants_new_long[ , sort(unique(value)) , ]
    
    #what column were containing shitty stuff
    plants_new_long[!value%in%c("X", "2", "4", "3", "1", "x", "0", "5"), .N , taxon]
    plants_new_long[  , .N  , rake_max ] #hows that for a  revelation!!!
    
    #zeros are non-occurrences, drop those
    plants_new_long[ value == "0" , .N , submitter_name ]
    plants_new_long <- plants_new_long[value != "0"]
    plants_new_long[ , taxa_present := is.na(value) , ]
    plants_new_long[rake_max%in%c("3","4","5") , taxa_rakeabund := value , ]
    
    plants_new_long[ , .N , taxa_rakeabund]
    
    plants_new_long[value%in%c("x","X"), .N , survey_ident ]
    
    
# survey metrics
    
    plants_db_new[ , .N ,  .(dow, survey_start, survey_ident)]
    plants_db_new[ , survey_ident := .GRP ,  .(dow, survey_start)]
    
    surveys_new <- plants_db_new[ , .(nobs = .N, 
                                      tot_n_samp) ,  .(dow, survey_start)]







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

ramsey_co_plant_surveys[ , .N, .(SURVEY_ID, DOW, SUBBASIN, fw_id, SURVEY_DATASOURCE, LAKE_NAME, DATESURVEYSTART, MULTIPARTSURVEY, nobs, tot_n_samp ) ]


#merge to invasion status
ramsey_co_plant_surveys[ , .N , SURVEY_ID] 


fwrite(surveys[DOW %in% c(62000000:63000000)|
                 LAKE_NAME == "white bear", .N , .(LAKE_NAME,year) ])
