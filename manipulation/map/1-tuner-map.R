# The purpuse of this script is apply project-local transformations and explore measures
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
# What the script intakes
path_input <- "./data/unshared/derived/0-dto.rds"
# What the script produces
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
# MAP study is already in long format, but others may not be
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
