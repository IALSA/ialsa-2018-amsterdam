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
# list ids with intermidiate missing (im) or right censored (rc) states
ids_with_im    <- unique(ds_valid[ds_valid$state == -1, "id"]) 
cat("\n Number of subjects with intermediate missing state (-1) : ",length(ids_with_im) )
ids_with_rc     <- unique(ds_valid[ds_valid$state == -2, "id"])
cat("\n Number of subjects with right censored state (-2) : ",length(ids_with_rc) )
ids_with_either <- unique(c(ids_with_im, ids_with_rc))
cat("\n Number of subjects with either IMS or RC state(s) : ",length(ids_with_either) )
ids_with_both   <- dplyr::intersect(ids_with_im, ids_with_rc)
cat("\n Number of subjects with both IMS and RC state(s) : ",length(ids_with_both) )

# save this version of the data
# match selected ids to the raw versions of the data for descriptives

# ---- save-to-disk-1 ------------------------------------------------------------
# Save as a compress, binary R dataset.  It's no longer readable with a text editor, but it saves metadata (eg, factor information).
names(dto)
dto[["valid"]] <- ds_valid # the dataset after removing invalid cases
saveRDS(dto, file=path_output, compress="xz")
names(dto)




# ---- describe-age-composition -----------
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


# ---- centering-decisions -----------------------
# centering decisions
age_center  = 75
year_center = 1900

# ---- prepare-for-estimation --------------------
ds_valid %>% glimpse()

# define the data object to be passed to the estimation call
# subset a random sample of individuals if needed to test out the modeling scripts
set.seed(42)
sample_ids <- sample(unique(ds_valid$id), 100)

# these selections are kept here for testing duirng estimation calls
ds_estimation <- ds_valid %>% 
  # dplyr::filter(id %in% sample_ids) %>% # make sample smaller if needed 
  # exclude individuals with missing states
  # dplyr::filter(!id %in% ids_with_im) %>%
  # dplyr::filter(!id %in% ids_with_rc) %>%
  dplyr::mutate(
    male = as.numeric(male), 
    age    = (age - age_center), # centering
    # age_bl = (age_bl - age_bl_center), # centering
    birth_year  = as.integer(birth_year - year_center) # centering (for numerical reasons)
) %>% 
  dplyr::select(
     id            # person identifier
    # ,age_bl        # age at baseline     
    ,birth_year    # year of birth         
    ,male          # sex
    ,edu           # years of education
    ,htm_med         # height in meters, median across observed across lifespan
    ,bmi_med         # Body Mass Index, median across observed across lifespan
    ,gait          # gait speed
    # ,cogact_old    # cognitive activity
    # ,soc_net       # size of social network
    ,fu_year       # follow-up year       
    ,firstobs      # baseline indicator        
    ,age           # age at visit
    ,state         # outcome state encoded from mmse
    ,mmse          # the basis for the state variable
  )
# save the object to be used during estimation
path_save_estimation <- paste0(path_folder,"ds_estimation.rds")
saveRDS(ds, path_save_estimation)
# the is is the data as it was passed to the estimation routine


view_id(ds_valid, ds_estimation, 33027)
view_id(ds_valid, ds_estimation, 402800)

# may have different number of id due to selections above
unique(dto$encoded$corrected$id) %>%  length()
unique(ds_valid$id)              %>%  length()
unique(ds_estimation$id)         %>%  length()

# ---- inspect-before-estimation --------------------
# preserve the EXCACT state of the data immediately prior to estimation
ds <- readRDS("./data/unshared/derived/models/ds_estimation.rds")
# view data object to be passed to the estimation call
cat("\n\n The following dataset will be passed to msm call (view for one person): \n")
set.seed(44)
ids <- sample(unique(ds$id), 1)
ds %>% dplyr::filter(id %in% ids) %>% knitr::kable()
cat("\n Subject count : ",length(unique(ds$id)),"\n")
cat("\n Frequency of states at baseline\n")
sf <- ds %>% 
  dplyr::filter(firstobs==TRUE) %>% 
  dplyr::group_by(state) %>% 
  dplyr::summarize(count = n()) %>%  # basic frequiencies
  dplyr::mutate(pct = round(count/sum(count),2)) %>%  # percentages, use for starter values
  print()
cat("\n State table: \n") 
print(msm::statetable.msm(state,id,data=ds)) # transition frequencies
# these will be passed as starting values
initial_probabilities <- as.numeric(as.data.frame(sf[!sf$state %in% c(-1,-2),"pct"])$pct) 
initial_probabilities <- c(initial_probabilities,0) # no death state at first observation
cat('\n The inital values for estimation : ', paste0(initial_probabilities, collapse = ", "))













###############################################
## Part II : msm computations                ##
###############################################

# ----- define-estimation-function --------------------------
estimate_multistate <- function(
  model_name      # code name for the model to be estimated (e.g. mB_v1)
  ,ds             # data object, clean and ready for estimation
  ,Q              # Q-matrix of transitions
  ,E              # misspecification matrix
  ,qnames         # names of the rows in the Q matrix
  ,cf             # string with covariate names for forward transitions
  ,cb             # string with covariate names for backward transitions
  ,cd             # string with covariate names for death transitions
  ,path_folder    # location to store model results
){
  cov_forward  <- as.formula(paste0("~",cf))
  cov_backward <- as.formula(paste0("~",cb))
  cov_death    <- as.formula(paste0("~",cd))
  covariates_ = list(
    "1-2"         = cov_forward,
    "2-3"         = cov_forward,
    "2-1"         = cov_backward,
    "1-4"         = cov_death,
    "2-4"         = cov_death,
    "3-4"         = cov_death
  )  
  model <- msm::msm(
    formula       = state ~ age,
    subject       = id,
    data          = ds,
    center        = FALSE,
    qmatrix       = Q, # transitions
    ematrix       = E, # misclassifications
    death         = TRUE,
    covariates    = covariates_,
    censor        = c(-1,-2),
    censor.states = list(c(1,2,3), c(1,2,3)),
    method        = method_,
    constraint    = constraint_,
    fixedpars     = fixedpars_,
    initprobs     = initprobs_,
    est.initprobs = TRUE,
    control       = list(trace=0, REPORT=1, maxit=1000, fnscale=10000)
  )
  # model <- paste0("test", covariates_)
  saveRDS(model, paste0(path_folder,model_name,".rds"))
  return(model)
} 

# ---- specify-model --------------------------
# initial value
q <- .01
# transition matrix
Q <- rbind( c(0, q, 0, q), 
            c(q, 0, q, q),
            c(0, 0, 0, q), 
            c(0, 0, 0, 0)) 
# misclassification matrix
E <- rbind( c( 0,  0,  0, 0),  
            c( 0,  0, .1, 0), 
            c( 0,  0,  0, 0),
            c( 0,  0,  0, 0) )
# transition names
qnames = c(
  "Healthy - Mild",   # q12
  # "Healthy - Severe", # q13 # disable, but keep to indicate intent to omit
  "Healthy - Dead",  # q14
  "Mild - Healthy",  # q21  
  "Mild - Severe",   # q23
  "Mild - Dead",     # q24
  # "Severe - Healthy",# q31  # disable, but keep to indicate intent to omit
  # "Severe - Mild",   # q32  # disable, but keep to indicate intent to omit
  "Severe - Dead"    # q34
)

# ---- specify-msm-options --------------------------
# set the estimation option
digits      = 2
method_     = "BFGS"     # alternatively, if does not converge "Nedler-Mead" 
constraint_ = NULL    # additional model constraints
fixedpars_  = NULL     # fixed parameters
initprobs_  = initial_probabilities 
# print the declared starting values for estimation
(Q_crude <- get_crude_Q(ds, Q, "age")) 

# ---- estimate-msm-models ------------------------
# turn this chunk OFF when printing the report
# compile model objects with msm() call
# each model will be saved in the specified folder, namely path_folder

# model A_v1
estimate_multistate("A_v1", ds, Q_crude, E, qnames,
                    cf = "age + male  + edu + htm_med + bmi_med",
                    cb = "age",
                    cd = "age + male  + edu + htm_med + bmi_med")

# model A_v2
estimate_multistate("A_v2", ds, Q_crude, E, qnames,
                    cf = "age + male  + edu + htm_med + bmi_med",
                    cb = "age + male  + edu + htm_med + bmi_med",
                    cd = "age + male  + edu + htm_med + bmi_med")

# ---- inspect-estimated-model -----------------------------
# call in the model object for inspection
# msm_model <- readRDS(paste0(path_folder, "mB_v1.rds")) # educat
msm_model <- readRDS(paste0(path_folder, "A_v1.rds"))# edu_low_med, edu_low_high
print_hazards(msm_model)
msm_summary(msm_model)
msm_details(msm_model)

# ---- utility-functions -------------------------------------------------------


# ---- msm1-1 -----------------------------------
model <- readRDS("./data/shared/derived/models/A_v1.rds")
pryr::object_size(model)
model$call
msm_summary(model) 
# ---- msm1-2 -----------------------------------
msm_details(model)
# ---- msm1-3 -----------------------------------
print_hazards(model) %>% neat()


# ---- msm2-1 -----------------------------------
model <- readRDS("./data/shared/derived/models/A_v2.rds")
model$call
msm_summary(model) 
# ---- msm2-2 -----------------------------------
msm_details(model)
# ---- msm2-3 -----------------------------------
print_hazards(model) %>% neat()






# ---- publish --------------
# path_report_1 <- "./reports/msm-model-review/msm-model-review-A.Rmd"
path_report_2 <- "./reports/msm-estimation/msm-estimation-model-A.Rmd"


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




