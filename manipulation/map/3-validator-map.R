#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.

# ---- load-packages -----------------------------------------------------------
library(magrittr)                         # Pipes
library(msm)                              # multi-state modeling
requireNamespace("ggplot2", quietly=TRUE) # graphing
requireNamespace("dplyr", quietly=TRUE)   # data manipulation
requireNamespace("testit", quietly=TRUE)  # condition testing

# ---- load-sources ------------------------------------------------------------
# base::source("http://www.ucl.ac.uk/~ucakadl/ELECT/ELECT.r") # load  ELECT functions
base::source("./scripts/ELECT.r") # load  ELECT functions
base::source("./scripts/ELECT-utility-functions.R") # ELECT utility functions
base::source("./scripts/functions-msm.R")

# ---- declare-globals ---------------------------------------------------------
path_input <- "./data/unshared/derived/dto-2-encoded.rds"
path_output <- "./data/unshared/derived/dto-3-valid.rds"

options(
  origin="1970-01-01"
)


path_folder <- "./data/unshared/derived/models/" # store estimated models here
digits = 2
cat("\n Objected with fitted models will be saved in the folder : \n")
cat("`",path_folder,"`")

# ---- load-data ---------------------------------------------------------------
dto <- readRDS(path_input)
names(dto)
# 1st element - dto$raw     - raw data, as obtained from ialsa-study-curator for RUSH-MAP study
# 2nd element - dto$meta    - meta data, info about variables
# 3rd element - dto$greeted - data produced by the greeter script
# 4th element - dto$tuned   - data produced by the tuner script
# 5th element - dto$encoded - data produced by the encoder script
names(dto$encoded) # is a list consisting of three data frames representing encoding steps
# dto$encoded$missing    # data after encoding missing states (-1, -2)
# dto$encoded$multistate # data after encoding multistates (1,2,3,4)
# dto$encoded$corrected  # data after correcting for longitudinal values

# dto$encoded$corrected contains the data most ready for modeling
# we will refer to is as `ds` in this script, as it is the in its primary focus
ds <- dto$encoded$corrected
ds <- dto$encoded$corrected
ds <- dto$encoded$multistate
ds %>% glimpse()


# ---- inspect-data -------------------------------------------------------------
names(dto)

# if you would like a reminder how the states were encoded, study a few cases:
view_id <- function(ds1,ds2,id){
  cat("Data set A:","\n")
  print(ds1[ds1$id==id,] %>% as.data.frame())
  cat("\nData set B","\n")
  print(ds2[ds2$id==id,] %>% as.data.frame())
}

view_id(dto$tuned, dto$encoded$missing, 33027) # NA -> -1 OR -2  
view_id(dto$encoded$missing, dto$encoded$multistate, 33027)   # age_at_visit -> age; mmse -> state; + mmse
view_id(dto$encoded$multistate, dto$encoded$corrected, 33027)   # all together

view_id(dto$tuned, dto$encoded$missing, 402800) # NA -> -1 OR -2  
view_id(dto$encoded$missing, dto$encoded$multistate, 402800)   # age_at_visit -> age; mmse -> state; + mmse
view_id(dto$encoded$multistate, dto$encoded$corrected, 402800)   # all together


###############################################
##  Part I: data preparation                ##
###############################################

# ---- inspect-created-multistates ----------------------------------
ids <- sample(unique(dto$encoded$corrected$id),1) # turn ## ON ## if using random generator
# useful cased found so far: 
# 50402431 , 37125649, 50101073, 6804844, 83001827 , 56751351, 13485298, 56751351, 75507759)
# ids <- c(50402431) #96351191

# limit the variables to the handful relevant for inspection
var_miss <- c("id", "fu_year","age_at_visit","age_at_death", "mmse" )
var_ms   <- c("id", "fu_year","age", "mmse","state" )
cat("\n Demonstrate the mechanics of encoding states: \n")
ids <- 33027 # turn ## OFF ## if using random generator
view_id(  
  dto$encoded$missing %>% dplyr::select_(.dots=var_miss), 
  dto$encoded$corrected %>% dplyr::select_(.dots=var_ms),  
  ids
) 

ids <- 402800 # turn ## OFF ## if using random generator
view_id(  
  dto$encoded$missing %>% dplyr::select_(.dots=var_miss), 
  dto$encoded$corrected %>% dplyr::select_(.dots=var_ms),  
  ids
) 

# simple frequencies of states
table(dto$encoded$corrected$state)
# examine transition matrix
# msm::statetable.msm(state,id,ds_ms)
knitr::kable(msm::statetable.msm(state,id, dto$encoded$corrected))

# ---- remove-invalid-cases --------------------------------------------------------------
####### 1) Remove observations with missing age
# Initial number of observations with missing age : 
sum(is.na(ds$age))
# ids_with_missing_age <- ds %>% filter(is.na(age)) %>% distinct(id) %>% as.list() %>% unlist()
# ds_temp <- ds %>% 
#   filter(id %in% ids_with_missing_age)

# initiate the existance of `ds_valid`,  which removes cases with invalid entries (as deemed by us)
ds_valid <- ds %>% 
  dplyr::filter(!is.na(age)) # remove rows that do not have associated age (age_at_visit )
sum(is.na(ds_valid$age))# Resultant number of observations with missing age


####### 2) Remove subjects with only ONE observed data point
# Initial number of subjects who have *n* observed data points
ds_valid %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarize(n_data_points = n()) %>% 
  dplyr::group_by(n_data_points) %>% 
  dplyr::summarize(n_people=n()) %>% 
  print()
# Determine which ids have only a single observation
remove_ids <- ds_valid %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarize(n_data_points = n()) %>% 
  dplyr::arrange(n_data_points) %>% 
  dplyr::filter(n_data_points==1) %>% 
  dplyr::select(id)
remove_ids <- remove_ids$id
# How many subjects to be removed from the data set: 
length(remove_ids)
ds_valid <- ds_valid %>% 
  dplyr::filter(!(id %in% remove_ids))
# Resultant number of subjects who have *n* observed data points
ds_valid %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarize(n_data_points = n()) %>% 
  dplyr::group_by(n_data_points) %>% 
  dplyr::summarize(n_people=n()) %>% 
  print()


####### 3) Remove subjects with IMS at the first observation
# Initial view of subjects with intermediate missing state at first observation:
ids_firstobs_ims <- ds_valid %>% 
  dplyr::filter(firstobs == TRUE & state == -1) %>% 
  dplyr::select(id) %>% print()
ids_firstobs_ims <- ids_firstobs_ims[,"id"]
# Establish the focal dataset
ds_valid <- ds_valid %>% 
  dplyr::filter(!id %in% ids_firstobs_ims)
# Resultant view of subjects with intermediate missing state at first observation:
ds_valid %>% 
  dplyr::filter(firstobs == TRUE & state == -1) %>% 
  dplyr::select(id) %>% print()
# should be empty


(a <- unique(dto$encoded$corrected$id) %>%  length())
(b <- unique(ds_valid$id) %>%  length())
(a - b ) # number of individuals removed due to invalid cases
# Here are these individuals in case we want to examine them in the context of full data
removed_ids <- setdiff(unique(dto$encoded$corrected$id), unique(ds_valid$id))
# alternative from tidyverse: 
# removed_ids <- dplyr::anti_join( # rows that do now have a match in A
#   dto$encoded$corrected %>% distinct(id),
#   dto$valid %>% distinct(id),
#   by = "id"
# ) %>% as.list() %>% unlist()

knitr::kable(msm::statetable.msm(state,id, dto$encoded$corrected))
knitr::kable(msm::statetable.msm(state,id, ds_valid))

# ---- count-case-types --------------------------------
# list ids with intermidiate missing (im) or right censored (rc) states or a combination of both/either
ids_with_intermediate_missing <- unique(ds_valid[ds_valid$state == -1, "id"]) 
cat("\n Number of subjects with intermediate missing state (-1) : ",length(ids_with_intermediate_missing) )
ids_with_right_censor <- unique(ds_valid[ds_valid$state == -2, "id"])
cat("\n Number of subjects with right censored state (-2) : ",length(ids_with_right_censor) )
ids_with_either <- unique(c(ids_with_intermediate_missing, ids_with_right_censor))
cat("\n Number of subjects with either IMS or RC state(s) : ",length(ids_with_either) )
ids_with_both   <- dplyr::intersect(ids_with_intermediate_missing, ids_with_right_censor)
cat("\n Number of subjects with both IMS and RC state(s) : ",length(ids_with_both) )

# save this version of the data
# match selected ids to the raw versions of the data for descriptives

# ---- save-to-disk ------------------------------------------------------------
# Save as a compress, binary R dataset.  It's no longer readable with a text editor, but it saves metadata (eg, factor information).
names(dto)
dto[["valid"]] <- ds_valid # the dataset after removing invalid cases
saveRDS(dto, file=path_output, compress="xz")
names(dto)




# # ---- describe-age-composition -----------
# # Time intervals in data:
# # the age difference between timepoint for each individual
# intervals <- matrix(NA,nrow(ds_valid),2)
# for(i in 2:nrow(ds_valid)){
#   if(ds_valid$id[i]==ds_valid$id[i-1]){
#     intervals[i,1] <- ds_valid$id[i]
#     intervals[i,2] <- ds_valid$age[i]-ds_valid$age[i-1]
#   }
#   intervals <- as.data.frame(intervals)
#   colnames(intervals) <- c("id", "interval")
# }
# cat("\n Minimum interval length : ",min(intervals[,2], na.rm=T))
# cat("\n Maximum interval length : ", max(intervals[,2], na.rm=T))
# # the age difference between timepoint for each individual
# intervals <- intervals[!is.na(intervals[,2]),] # Remove NAs:
# cat("\nTime intervals between observations within individuals:\n")
# print(round(quantile(intervals[,2]),digits))
# 
# # Info on age and time between observations:
# cat("\n Graphs of age distribution :\n")
# opar<-par(mfrow=c(1,3), mex=0.8,mar=c(5,5,3,1))
# hist(ds_valid$age[ds_valid$firstobs==1],col="red",xlab="Age at baseline in years",main="")
# hist(ds_valid$age,col="blue",xlab="Age in data in years",main="")
# hist(intervals[,2],col="green",xlab="Time intervals in data in years",main="")
# opar<-par(mfrow=c(1,1), mex=0.8,mar=c(5,5,2,1))
# 




# ---- publish --------------
# inherited : placeholder


# path_report_1 <- "./reports/msm-model-review/msm-model-review-A.Rmd"
path_report_2 <- "./reports/manipulation/msm-estimation-model-A.Rmd"


# allReports <- c(path_report_1)
allReports <- c(path_report_2)
# allReports <- c(path_report_3)
# allReports <- c(path_report_1, path_report_2, path_report_3)



pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      # "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}




