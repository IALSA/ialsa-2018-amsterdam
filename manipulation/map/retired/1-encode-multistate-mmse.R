# knitr::stitch_rmd(script="./manipulation/rename-classify.R", output="./manipulation/rename-classify.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
 
# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/functions-common.R") # used in multiple reports
# source("./scripts/graph-presets.R") # fonts, colors, themes 
# source("./scripts/general-graphs.R")
# source("./scripts/specific-graphs.R")
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE) #Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit", quietly=TRUE)
# requireNamespace("plyr", quietly=TRUE)

# ---- declare-globals ---------------------------------------------------------
path_input <- "./data/unshared/derived/0-dto.rds"
path_output <- "./data/unshared/derived/1-dto.rds"

options(
  origin="1970-01-01"
)

# ---- load-data ---------------------------------------------------------------
# load the product of 0-ellis-island.R,  a list object containing data and metad
dto <- readRDS(path_input)

# ---- inspect-data -------------------------------------------------------------
names(dto)
# 1st element - unit(person) level data
# dplyr::tbl_df(dto[["unitData"]])
# 2nd element - meta data, info about variables
# dto[["metaData"]]


# ---- tweak-data --------------------------------------------------------------
ds <- dto[["unitData"]]

# table(ds$fu_year, ds$dementia)
ds <- ds %>% 
  dplyr::mutate(
    wave = fu_year # the function to examine temporal structure depend on dataset to have a variable "wave"
  ) %>% 
  dplyr::rename(
    physact = phys5itemsum
  )


# some predictors needs to be transformed into time-invariate
# we will follow the convention of computing the median value across lifespan
# instead of assigned the value at baseline (but this is somewhat arbitrary)

# ---- force-to-static-height ---------------------------
ds %>% view_temporal_pattern("htm", 2) # with seed
ds %>% temporal_pattern("htm") # random every time
ds %>% over_waves("htm"); # 2, 4, 6
# check that values are the same across waves
ds %>%
  dplyr::group_by(id) %>%
  dplyr::summarize(unique = length(unique(htm))) %>%
  dplyr::arrange(desc(unique)) # unique > 1 indicates change over wave
# grab the value for the first wave and forces it to all waves
ds <- ds %>%
  dplyr::group_by(id) %>%
  # compute median height across lifespan
  dplyr::mutate(
    htm_bl    = dplyr::first(htm),    # 
    htm_med   = median(htm, na.rm =T) # computes the median height across lifespan
  ) %>%
  dplyr::ungroup()
# examine the difference
ds %>% view_temporal_pattern("htm_med", 2)
ds %>% view_temporal_pattern("htm", 2)

# ---- force-to-static-bmi ---------------------------
ds %>% view_temporal_pattern("bmi", 2) # with seed
ds %>% temporal_pattern("bmi") # random every time
ds %>% over_waves("bmi"); # 2, 4, 6
# check that values are the same across waves
ds %>%
  dplyr::group_by(id) %>%
  dplyr::summarize(unique = length(unique(bmi))) %>%
  dplyr::arrange(desc(unique)) # unique > 1 indicates change over wave
# grab the value for the first wave and forces it to all waves
ds <- ds %>%
  dplyr::group_by(id) %>%
  # compute median height across lifespan
  dplyr::mutate(
    bmi_bl    = dplyr::first(bmi),    # 
    bmi_med   = median(bmi, na.rm =T) # computes the median height across lifespan
  ) %>%
  dplyr::ungroup()
# examine the difference
ds %>% view_temporal_pattern("bmi_med", 2)
ds %>% view_temporal_pattern("bmi", 2)

# ---- force-to-static-gait ---------------------------
ds %>% view_temporal_pattern("gait", 2) # with seed
ds %>% temporal_pattern("gait") # random every time
ds %>% over_waves("gait"); # 2, 4, 6
# check that values are the same across waves
ds %>%
  dplyr::group_by(id) %>%
  dplyr::summarize(unique = length(unique(gait))) %>%
  dplyr::arrange(desc(unique)) # unique > 1 indicates change over wave
# grab the value for the first wave and forces it to all waves
ds <- ds %>%
  dplyr::group_by(id) %>%
  # compute median height across lifespan
  dplyr::mutate(
    gait_bl    = dplyr::first(gait),    # 
    gait_med   = median(gait, na.rm =T) # computes the median height across lifespan
  ) %>%
  dplyr::ungroup()
# examine the difference
ds %>% view_temporal_pattern("gait_med", 2)
ds %>% view_temporal_pattern("gait", 2)



# ---- force-to-static-grip ---------------------------
ds %>% view_temporal_pattern("grip", 2) # with seed
ds %>% temporal_pattern("grip") # random every time
ds %>% over_waves("grip"); # 2, 4, 6
# check that values are the same across waves
ds %>%
  dplyr::group_by(id) %>%
  dplyr::summarize(unique = length(unique(grip))) %>%
  dplyr::arrange(desc(unique)) # unique > 1 indicates change over wave
# grab the value for the first wave and forces it to all waves
ds <- ds %>%
  dplyr::group_by(id) %>%
  # compute median height across lifespan
  dplyr::mutate(
    grip_bl    = dplyr::first(grip),    # 
    grip_med   = median(grip, na.rm =T) # computes the median height across lifespan
  ) %>%
  dplyr::ungroup()
# examine the difference
ds %>% view_temporal_pattern("grip_med", 2)
ds %>% view_temporal_pattern("grip", 2)

# ---- force-to-static-physact ---------------------------
ds %>% view_temporal_pattern("physact", 2) # with seed
ds %>% temporal_pattern("physact") # random every time
ds %>% over_waves("physact"); # 2, 4, 6
# check that values are the same across waves
ds %>%
  dplyr::group_by(id) %>%
  dplyr::summarize(unique = length(unique(physact))) %>%
  dplyr::arrange(desc(unique)) # unique > 1 indicates change over wave
# grab the value for the first wave and forces it to all waves
ds <- ds %>%
  dplyr::group_by(id) %>%
  # compute median height across lifespan
  dplyr::mutate(
    physact_bl    = dplyr::first(physact),    # 
    physact_med   = median(physact, na.rm =T) # computes the median height across lifespan
  ) %>%
  dplyr::ungroup()
# examine the difference
ds %>% view_temporal_pattern("physact_med", 2)
ds %>% view_temporal_pattern("physact", 2)





# ---- describe-before-encoding --------------------------------------------------------------
# if died==1, all subsequent focal_outcome==DEAD.
# during debuggin/testing use only a few ids, for manipulation use all
set.seed(43)
ids <- sample(unique(ds$id),3) # randomly select a few ids
# custom select a few ids that give different pattern of data. To be used for testing
ids <- c(33027) #,33027, 50101073, 6804844, 83001827 , 56751351, 13485298, 30597867)

# ---- into-long-format -------------------------
ds_long <- ds %>% 
  # dplyr::filter(id %in% ids) %>% # turn this off when using the entire sample
  dplyr::mutate(
    age_at_bl    = as.numeric(age_bl),
    age_at_death = as.numeric(age_death), 
    male      = as.logical(ifelse(!is.na(msex), msex=="1", NA_integer_)),
    edu       = as.numeric(educ)
  ) %>% 
  dplyr::select_(.dots = c(
    "id"             # personal identifier
    ,"male"          # gender
    ,"edu"           # years of education
    ,"age_bl"        # age at baseline
    ,"age_at_death"  # age at death
    ,"died"          # death indicator
    ,"birth_year"    # year of birth 
    ,"htm_med"       # height in meters, median across observed across lifespan
    ,"bmi_med"       # Body Mass Index, median across observed across lifespan
    ,"physact_med"   # Physical activity, median across observed across lifespan
    # time-invariant above
    ,"fu_year"       # Follow-up year --- --- --- --- --- --- --- --- --- ---
    # time-variant below
    ,"date_at_visit" # perturbed date of visit
    ,"age_at_visit"  # age at cycle - fractional  
    ,"mmse"          # mini mental state exam (max =30)
    ,"cogn_global"   # global cognition
    ,"dementia"      # dementia diagnosis (?)
    ,"gait"          # Gait Speed in minutes per second (min/sec)
    ,"grip"          # Extremity strengtg in pounds (lbs)
    ,"htm"           # height in meters
    ,"bmi"           # Body Mass Index  in kilograms per meter squared (kg/msq)
    ,"physact"       # Physical activity (sum of 5 items)
    )
)
# save to disk for direct examination
# write.csv(d,"./data/shared/musti-state-dementia.csv")  


# inspect crated data object
ds_long %>% 
  dplyr::filter(id %in% ids) %>% 
  print()

# ---- attrition-effect ------------------------------
t <- table(ds_long[,"fu_year"], ds_long[,"died"]); t[t==0]<-".";t


# ----- mmmse-trajectories ----------------------
# raw_smooth_lines(ds_long, "mmse")

# ---- encode-missing-states ---------------------------
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
# inspect crated data object
ds_miss %>% 
  dplyr::filter(id %in% ids) %>% 
  print()
# ds_long %>% dplyr::glimpse()

# ---- encode-multi-states ------------------------------
encode_multistates <- function(
  d, # data frame in long format 
  outcome_name, # measure to compute live states
  age_name, # age at each wave
  age_death_name, # age of death
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

ds_ms <- encode_multistates(
  d                = ds_miss,
  outcome_name     = "mmse",
  age_name         = "age_at_visit",
  age_death_name   = "age_at_death",
  dead_state_value = 4
)
# set.seed(NULL)
# ids <- sample(unique(ds$id),1)
ids <- 50107169
ds_ms %>% 
  dplyr::filter(id %in% ids) %>% 
  print()

# ---- correct-values-at-death -----------------------
correct_values_at_death <- function(
  ds, # data frame in long format with multistates encoded
  outcome_name, # measure to correct value in
  dead_state_value # value that represents dead state
){
  ds[ds$state == dead_state_value, outcome_name] <- NA_real_
  return(ds)
}
  # manually correct values for data_at_visit
  ds_ms[ds_ms$state == 4, "date_at_visit"] <- NA # because of date format
  # automatically correct values for time-variant measures
  ds_ms <- ds_ms %>% correct_values_at_death("date_at_visit",4)
  ds_ms <- ds_ms %>% correct_values_at_death("dementia",4)
  ds_ms <- ds_ms %>% correct_values_at_death("cogn_global",4)
  ds_ms <- ds_ms %>% correct_values_at_death("gait",4)
  ds_ms <- ds_ms %>% correct_values_at_death("grip",4)
  ds_ms <- ds_ms %>% correct_values_at_death("htm",4)
  ds_ms <- ds_ms %>% correct_values_at_death("bmi",4)
  # ds_ms <- ds_ms %>% correct_values_at_death("income_40",4)
  # ds_ms <- ds_ms %>% correct_values_at_death("cogact_old",4)
  # ds_ms <- ds_ms %>% correct_values_at_death("socact_old",4)
  # ds_ms <- ds_ms %>% correct_values_at_death("soc_net",4)
  # ds_ms <- ds_ms %>% correct_values_at_death("social_isolation",4)

  # TODO: automate this step
ds_ms %>% 
  dplyr::filter(id %in% ids) %>% 
  print()

# ---- add-firstobs-flag -----------------------------
(N  <- length(unique(ds_ms$id)))
subjects <- as.numeric(unique(ds_ms$id))
# Add first observation indicator
# this creates a new dummy variable "firstobs" with 1 for the first wave
cat("\nFirst observation indicator is added.\n")
offset <- rep(NA,N)
for(i in 1:N){offset[i] <- min(which(ds_ms$id==subjects[i]))}
firstobs <- rep(0,nrow(ds_ms))
firstobs[offset] <- 1
ds_ms <- cbind(ds_ms ,firstobs=firstobs)
print(head(ds_ms))


# ---- inspect-created-multistates ----------------------------------
# compare before and after ms encoding
view_id <- function(ds1,ds2,id){
  cat("Data set A:","\n")
  print(ds1[ds1$id==id,])
  cat("\nData set B","\n")
  print(ds2[ds2$id==id,])
}
# view a random person for sporadic inspections
set.seed(39)
ids <- sample(unique(ds_miss$id),1)
# ids <- 68914513
view_id(ds_long, ds_miss, ids)
view_id(ds_miss, ds_ms, ids)
view_id(ds_long, ds_ms, ids)

# ----- transitions-matrix -----------------------------
# simple frequencies of states
table(ds_ms$state)
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
# dto[["ms_mmse"]][["long"]]    <- ds_long
dto[["ms_mmse"]][["missing"]] <- ds_miss
dto[["ms_mmse"]][["multi"]]   <- ds_ms
names(dto$ms_mmse)
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


























