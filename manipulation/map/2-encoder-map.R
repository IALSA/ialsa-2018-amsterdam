# The purpose of this script is to create multistate measure to be used in modelling
# knitr::stitch_rmd(script="./manipulation/rename-classify.R", output="./manipulation/rename-classify.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
 
# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/functions-common.R") # used in multiple reports
source("./scripts/graph-presets.R") # fonts, colors, themes
source("./scripts/graph-general.R")
source("./scripts/graph-specific.R")
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE) #Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit", quietly=TRUE)
requireNamespace("msm", quietly=TRUE)
# requireNamespace("plyr", quietly=TRUE)

# ---- declare-globals ---------------------------------------------------------
path_input <- "./data/unshared/derived/dto-1-tuned.rds"
path_output <- "./data/unshared/derived/dto-2-encoded.rds"

options(
  origin="1970-01-01"
)

# ---- load-data ---------------------------------------------------------------
# load the product of 0-ellis-island.R,  a list object containing data and metad
dto <- readRDS(path_input)

# ---- inspect-data -------------------------------------------------------------
names(dto)
# 1st element - raw data, as obtained from ialsa-study-curator for RUSH-MAP study
# dplyr::tbl_df(dto[["raw"]])
# 2nd element - meta data, info about variables
# dto[["meta"]]
# 3rd element - data produced by the greeter script
# dto[["greeted"]]
# 4th element - data produced by the tuner script
# dto[["tuned"]]

# ---- tweak-data --------------------------------------------------------------
ds <- dto[["tuned"]]
ds_long <- ds

# ---- describe-before-encoding --------------------------------------------------------------
# if died==1, all subsequent focal_outcome==DEAD.
# during debuggin/testing use only a few ids, for manipulation use all
set.seed(43)
ids <- sample(unique(ds$id),3) # randomly select a few ids
# custom select a few ids that give different pattern of data. To be used for testing
ids <- c(33027) #,33027, 50101073, 6804844, 83001827 , 56751351, 13485298, 30597867)


# ---- encode-missing-states ---------------------------
## ## ## - STEP 1 - Missing values on criterion 
# create an object ds_miss from ds_long 

# x <- c(NA, 5, NA, 7)
determine_censor <- function(x, is_right_censored){
  ifelse(is_right_censored, -2,
         ifelse(is.na(x), -1, x)
  )
}

# new way
# # create file with missing information
# make_ds_miss <- function(
#   d,
#   variable
# ){
#   d_long <- d %>% 
#     dplyr::mutate_(
#       "target" = variable
#     ) %>% 
#     as.data.frame()
# 
#   (N <- length(unique(d_long$id))) # sample size
#   subjects <- as.numeric(unique(d_long$id)) # list the ids
#   
#   for(i in 1:N){
#     # for(i in unique(ds$id)){  # use this line for testing
#     # Get the individual data:
#     (dta.i                  <- d_long[d_long$id==subjects[i],]) # select a single individual
#     # (dta.i                <- d_long[d_long$id==6804844,]) # select a single individual # use this line for testing
#     (dta.i                  <- as.data.frame(dta.i %>% dplyr::arrange(-age_at_visit))) # enforce sorting
#     (dta.i$missed_last_wave <- (cumsum(!is.na(dta.i$target))==0L)) # is the last obs missing?
#     (dta.i$presumed_alive   <-  is.na(any(dta.i$age_at_death))) # can we presume subject alive?
#     (dta.i$right_censored   <- dta.i$missed_last_wave & dta.i$presumed_alive) # right-censored?
#     # dta.i$target_recoded    <- determine_censor(dta.i$target, dta.i$right_censored) # use when tracing
#     (dta.i$target             <- determine_censor(dta.i$target, dta.i$right_censored)) # replace in reality
#     (dta.i                  <- as.data.frame(dta.i %>% dplyr::arrange(age_at_visit)))
#     (dta.i                  <- dta.i %>% dplyr::select(-missed_last_wave, -right_censored ))
#     # Rebuild the data:
#     if(i==1){d_miss <- dta.i}else{d_miss <- rbind(d_miss,dta.i)}
#   } 
#   # this part is not finished yet, need to make replacing the old variable 
#   d_miss <- d_miss %>% 
#     # drop original variable
#     dplyr::mutate(
#       mmse = target
#     ) %>% 
#     dplyr::select(-target)
#   
#   return(d_miss)
# }
# # usage 
# ds_miss <- ds_long %>% make_ds_miss(variable = "mmse")


# old way
(N <- length(unique(ds_long$id))) # sample size
subjects <- as.numeric(unique(ds_long$id)) # list the ids
# ds_long_temp <- ds_long
# i <- 5; 
for(i in 1:N){
  # for(i in unique(ds$id)){  # use this line for testing
  # Get the individual data:
  # ds_long <- ds_long_temp %>% 
  #   dplyr::select(id, fu_year, age_at_visit,died, age_death, mmse) %>% 
  #   as.data.frame()
  (dta.i                  <- ds_long[ds_long$id==subjects[i],]) # select a single individual
  # (dta.i                <- ds_long[ds_long$id==6804844,]) # select a single individual # use this line for testing
  (dta.i                  <- as.data.frame(dta.i %>% dplyr::arrange(-age_at_visit))) # enforce sorting
  (dta.i$missed_last_wave <- (cumsum(!is.na(dta.i$mmse))==0L)) # is the last obs missing?
  (dta.i$presumed_alive   <-  is.na(any(dta.i$age_at_death))) # can we presume subject alive?
  # (dta.i$presumed_alive <-  is.na(any(dta.i$age_death))) # can we presume subject alive?
  (dta.i$right_censored   <- dta.i$missed_last_wave & dta.i$presumed_alive) # right-censored?
  # dta.i$mmse_recoded    <- determine_censor(dta.i$mmse, dta.i$right_censored) # use when tracing
  (dta.i$mmse             <- determine_censor(dta.i$mmse, dta.i$right_censored)) # replace in reality
  (dta.i                  <- as.data.frame(dta.i %>% dplyr::arrange(age_at_visit)))
  (dta.i                  <- dta.i %>% dplyr::select(-missed_last_wave, -right_censored ))
  # Rebuild the data:
  if(i==1){ds_miss <- dta.i}else{ds_miss <- rbind(ds_miss,dta.i)}
} 

# the created data set (ds_miss) replaces NA in mmse with cencored values (-1, -2) where appropriate
ds_long %>% 
  dplyr::filter(id %in% ids) %>% 
  as.data.frame() %>% 
  print()
# note the updated value in mmse and the new variabe presumed_alive
ds_miss %>% 
  dplyr::filter(id %in% ids) %>% 
  print()

# ds_miss contains the mmse values corrected for missing states (-1,-2) 
# (e.g. if last mmse observation was NA, but we new respondent was alive)
# and presumed_alive indicator

# ---- encode-multi-states ------------------------------
## ## ## - STEP 2 - Criterion into a state variable

# in this step we focus on a variable that will identify the states to be modeled
# a few things will happen:
# 1) age_at_visit will become age (needed for passing to msm estimation routines)
# 2) variable state will replace mmse (the basis for it)
# 3) mmse will reappear at the end of the set
# 4) a NEW ROW will be created for each subject containing the measure at death

encode_multistates <- function(
  d,               # data frame in long format 
  outcome_name,    # measure to compute live states
  age_name,        # age at each wave
  age_death_name,  # age of death
  dead_state_value # value to represent dead state
){
  # declare arguments for debugging
  # d = ds_miss
  # outcome_name = "mmse";age_name = "age_at_visit";age_death_name = "age_death";dead_state_value = 4
  (subjects <- sort(unique(d$id))) # list subject ids
  (N <- length(subjects)) # count subject ids
  d[,"raw_outcome"] <- d[,outcome_name] # create a copy
  # standardize names
  colnames(d)[colnames(d)==outcome_name] <- "state" # ELECT requires this name
  colnames(d)[colnames(d)==age_name] <- "age" # ELECT requires this name
  # for(i in unique(ds$id)){  # use this line for testing
  for(i in 1:N){
    # Get the individual data: i = 1
    (dta.i <- d[d$id==subjects[i],])
    # (dta.i <- ds_long[ds_long$id==6804844,]) # select a single individual # use this line for testing
    # Encode live states
    dta.i$state <- ifelse( 
      dta.i$state > 26, 1, ifelse( # healthy
        dta.i$state <= 26 &  dta.i$state >= 23, 2, ifelse( # mild CI
          dta.i$state < 23 & dta.i$state >= 0, 3, dta.i$state))) # mod-sever CI
    # Is there a death? If so, add a record:
    (death <- !is.na(dta.i[,age_death_name][1]))
    if(death){
      (record <- dta.i[1,])
      (record$state <- dead_state_value)
      (record$age   <- dta.i[,age_death_name][1])
      (ddta.i <- rbind(dta.i,record))
    }else{ddta.i <- dta.i}
    # Rebuild the data:
    if(i==1){dta1 <- ddta.i}else{dta1 <- rbind(dta1,ddta.i)}
  }
  dta1[,age_death_name] <- NULL
  colnames(dta1)[colnames(dta1)=="raw_outcome"] <- outcome_name
  dta1[dta1$state == dead_state_value,outcome_name] <- NA_real_
  dta1[dta1$state == dead_state_value,"fu_year"] <- NA_real_
  return(dta1)
}

ds_ms <- 
  encode_multistates(
    d                = ds_miss,
    outcome_name     = "mmse",         # currently supports mmse only, todo: rework functins in tidyverse
    age_name         = "age_at_visit",
    age_death_name   = "age_at_death",
    dead_state_value = 4
  )

# review
ds_miss %>% 
  dplyr::filter(id %in% 50107169) %>% 
  print()
# notice how 
ds_ms %>% 
  dplyr::filter(id %in% 50107169) %>% 
  print()
# 1) age_at_visit becomes age (needed for modeling)
# 2) variable state replaces mmse (which was the basis for it)
# 3) mmse reappears at the end of the set
# 4) most importantly, ds_ms creates an additional row for each subject
# this row identifies measurement at death of which we know for sure only age 
# all other which refer to the time of death should be set to NA as unknown

# ---- correct-values-at-death -----------------------
## ## ## - STEP 3 - Longitudinal corrections

# previous step created an additional row for each subject, measurement at death
# the values in other variables carried over into the new row
# this is ok with time-invariant measures (to the left of  wave/fu_year)
# but NOT ok with time-variant   measures (to the right of wave/fu_year)
# time-invariant measures must be replaced with NA in measures at the moment of death
correct_values_at_death <- function(
  ds, # data frame in long format with multistates encoded
  outcome_name, # measure to correct value in
  dead_state_value # value that represents dead state
){
  ds[ds$state == dead_state_value, outcome_name] <- NA_real_
  return(ds)
}
  ds_corrected <- ds_ms # inherits the form

  # manually correct values for data_at_visit
  ds_corrected[ds_corrected$state == 4, "date_at_visit"] <- NA # because of date format
  # automatically correct values for time-variant measures
  ds_corrected <- ds_corrected %>% correct_values_at_death("wave",4)
  # ds_corrected <- ds_corrected %>% correct_values_at_death("date_at_visit",4)
  ds_corrected <- ds_corrected %>% correct_values_at_death("dementia",4)
  ds_corrected <- ds_corrected %>% correct_values_at_death("cogn_global",4)
  ds_corrected <- ds_corrected %>% correct_values_at_death("gait",4)
  ds_corrected <- ds_corrected %>% correct_values_at_death("grip",4)
  ds_corrected <- ds_corrected %>% correct_values_at_death("htm",4)
  ds_corrected <- ds_corrected %>% correct_values_at_death("bmi",4)
  ds_corrected <- ds_corrected %>% correct_values_at_death("physact",4)
  ds_corrected <- ds_corrected %>% correct_values_at_death("firstobs",4)
  # ds_corrected <- ds_corrected %>% correct_values_at_death("income_40",4)
  # ds_corrected <- ds_corrected %>% correct_values_at_death("cogact_old",4)
  # ds_corrected <- ds_corrected %>% correct_values_at_death("socact_old",4)
  # ds_corrected <- ds_corrected %>% correct_values_at_death("soc_net",4)
  # ds_corrected <- ds_corrected %>% correct_values_at_death("social_isolation",4)
  # TODO: automate this step
  

# presumed_alive is logical for the state of data at the time of access
# TODO: place the presumed_alive variabe together with time invariant variables

# inspect the resultant data set  
ds_corrected %>% 
  dplyr::filter(id %in% ids) %>% 
  print()

# ds_corrected contains data set of long structure with respect to time
# It has been augmented with the measurement at the time of death
# and corrected for time-invariant and time-variant values in other variabels

# ---- inspect-created-multistates ----------------------------------
# compare before and after ms encoding
view_id <- function(ds1,ds2,id){
  cat("Data set A:","\n")
  print(ds1[ds1$id==id,] %>% as.data.frame())
  cat("\nData set B","\n")
  print(ds2[ds2$id==id,])
}
# view a random person for sporadic inspections
set.seed(39)
ids <- sample(unique(ds_miss$id),1)
# ids <- 68914513
ids <- c(33027) # trailing NA; presumed alive
# ids <- c(1243685) # dead; only 4 obs; developed dementia on the last one

view_id(ds_long, ds_miss, 33027) # NA -> -1 OR -2  
view_id(ds_miss, ds_ms, 33027)   # age_at_visit -> age; mmse -> state; + mmse
view_id(ds_long, ds_ms, 33027)   # all together

view_id(ds_long, ds_miss, 402800) # NA -> -1 OR -2   # nothing happens for this person
view_id(ds_miss, ds_ms, 402800) # age_at_visit -> age; mmse -> state; + mmse
view_id(ds_ms,ds_corrected, 402800) # longitudinal corrections



# to find people you might use for examples
target_cohort <- 
  ds_long %>% 
  dplyr::filter(died==1) %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarize(n=n()) %>% 
  dplyr::filter(n==2) %>% 
  dplyr::arrange(id)

  


# ----- transitions-matrix -----------------------------
# simple frequencies of states
table(ds_corrected$state)
# examine transition matrix
# msm::statetable.msm(state,id,ds_ms)
knitr::kable(msm::statetable.msm(state,id,ds_ms))
# TODO:  examine transition cases for missing states (-2 and -1)

# ---- save-to-disk ------------------------------------------------------------
# Save as a compress, binary R dataset.  It's no longer readable with a text editor, but it saves metadata (eg, factor information).

# at this point there exist two relevant data sets:
# ds_long - subset of variables focal to the project
# ds_miss - missing states are encoded
# ds_ms   - multi   states are encoded
# it is useful to have access to all three while understanding/verifying encodings

names(dto)
dto[["encoded"]] <- list()
dto[["encoded"]][["missing"]]    <- ds_miss       # we preserve these forms to compare later
dto[["encoded"]][["multistate"]] <- ds_ms         # we preserve these forms to compare later
dto[["encoded"]][["corrected"]]  <- ds_corrected  # we preserve these forms to compare later 

names(dto)
names(dto$encoded)
saveRDS(dto, file=path_output, compress="xz")

# ---- object-verification ------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
dto <- readRDS(path_output)
pryr::object_size(dto)
names(dto)
names(dto$ms_mmse)
# 1st element - unit(person) level data, all variables available from the source
dplyr::tbl_df(dto[["unitData"]])
# 2nd element - meta data, info about variables
dto[["metaData"]] %>% tibble::as_tibble()
# 3rd element - data for MMSE outcome
names(dto[["ms_mmse"]])
# dto$ms_mmse[["ds_long"]] # subset of variables focal to the project
# dto$ms_mmse[["ds_miss"]] # missing states are encoded
# dto$ms_mmse[["ds_ms"]]   # multi   states are encoded


























