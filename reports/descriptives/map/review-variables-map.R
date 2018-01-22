# knitr::stitch_rmd(script="./reports/review-variables/map/review-variables-map.R", output="./reports/review-variables/map/review-variables-map.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/functions-common.R") # used in multiple reports
source("./scripts/graph-presets.R")    # fonts, colors, themes
source("./scripts/graph-general.R")    # simple, elemental displays
source("./scripts/graph-specific.R")   # complex, composite displays

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
library(knitr)    # enable better tables
# Verify these packages are available on the machine, 
# but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")# For asserting conditions meet expected patterns.

# ---- declare-globals ---------------------------------------------------------
path_input <- "./data/unshared/derived/dto-3-valid.rds"
# estimated models (will be) stored  here
path_input_ds_estimation <- "./data/unshared/derived/models/ds_estimation.rds"
digits = 2
options(
  origin="1970-01-01"
)
# ---- load-data ---------------------------------------------------------------
dto <- readRDS(path_input) 
names(dto)
# 1st element - dto$raw     - raw data, as obtained from ialsa-study-curator for RUSH-MAP study
# 2nd element - dto$meta    - meta data, info about variables
# 3rd element - dto$greeted - dataset produced by the greeter script
# 4th element - dto$tuned   - dataset produced by the tuner script
# 5th element - dto$encoded - dataset produced by the encoder script
   # dto$encoded is a list consisting of three data frames representing encoding steps
   # dto$encoded$missing    # dataset after encoding missing states (-1, -2)
   # dto$encoded$multistate # dataset after encoding multistates (1,2,3,4)
   # dto$encoded$corrected  # dataset after correcting for longitudinal values
# 6th element - dto$valid   - dataset produced by validator script
# contains only valid case that have been CLEARED to enter the estimation routine
# however, what actually ENTERED model estimation is preserved immediately before msm call
# and stored along side model estimation results (produced by the first part of the `modeler` script):
ds_estimation <- readRDS(path_input_ds_estimation) 
# review locations
print(path_input)
print(path_input_ds_estimation)
names(dto) # inspect componets in the data transfer object (dto)
names(dto$encoded) # phases of the encoding procedure
# create aliases for quick access
# full version of data before any subsetting, filtering or transformations
ds_greeted <- dto$greeted %>% tibble::as_tibble()
# prepared for modeling, only valid cases. all variables needed in place.
ds_valid   <- dto$valid %>% tibble::as_tibble()


# ---- meta-table --------------------------------------------------------
dto$meta %>%  
  dplyr::select(-url, -notes) %>% 
  dplyr::mutate(type = ordered(type)) %>% 
  DT::datatable(
    class   = 'cell-border stripe',
    caption = "This is a dynamic table of the metadata file. Edit at `./data/meta/map/meta-data-map.csv",
    filter  = "top",
    options = list(pageLength = 6, autoWidth = TRUE)
  )



# ---- inspect-data-1 -------------------------------------------------------------
# this is how we can interact with the `dto` to call and graph data and metadata
dto$meta %>% 
  dplyr::filter(type=="demographic") %>% 
  dplyr::select(name,name_new,label)

dto$valid %>% dplyr::glimpse(90)

# ---- inspect-data-2 -------------------------------------------------------------
# identifiy the difference between eligible cases and those used in estimation (2 steps):
# identify cases CLEARED for modeling
valid_ids <- 
  dto$valid %>% 
  dplyr::distinct(id) %>% 
  as.list() %>% unlist() %>% as.vector()

valid_ids %>% length()

# identify cases USED in estimation
modeled_ids <- 
  ds_estimation %>% 
  dplyr::distinct(id) %>% 
  as.list() %>% unlist() %>% as.vector()

modeled_ids %>% length()

# Has the model in question used all legal cases? In other words, how many cases did not enter estimation adn are unique to `dto$valid`?
(unique_to_valid <- setdiff(modeled_ids, valid_ids))

# Of all valid cases cleared for estimation
(n_valid <- valid_ids %>% length())
# How many were used during estimation of this model?
(n_modeled <- n_valid - length( setdiff(modeled_ids, valid_ids) ) )
# which means that ds_estimation dropped
(n_valid - n_modeled)
# eligible cases from the dto$valid state of the data

# ---- tweak-data --------------------------------------------------------------
# variablables - only those relevant to the research question
# listing variables is useful to remind the contents and update the order
# this listing was adapted from the initial mention in `1-tuner-map.R`
ds <- dto$valid %>% 
  dplyr::filter(id %in% modeled_ids) %>% 
  dplyr::mutate_(
    "age_at_visit" = "age" # `msm` required this variable to have standard name `age`
  ) %>% 
  dplyr::select_(
    "id"             # personal identifier
    ,"male"          # gender
    ,"edu"           # years of education
    ,"birth_year"    # year of birth 
    ,"died"          # death indicator
    ,"age_at_death"  # age at death
    ,"age_at_bl"     # age at baseline
    # Temporally flattened meausures: median across observed lifespan
    ,"htm_med"       # height in meters
    ,"bmi_med"       # Body Mass Index
    ,"physact_med"   # Physical activity
    ,"gait_med"      # Gait Speed in minutes per second (min/sec)
    ,"grip_med"      # Extremity strength in pounds (lbs)
    # Temporally flattened meausures: at baseline
    ,"htm_bl"        # height in meters
    ,"bmi_bl"        # Body Mass Index
    ,"physact_bl"    # Physical activity
    ,"gait_bl"       # Gait Speed in minutes per second (min/sec)
    ,"grip_bl"       # Extremity strength in pounds (lbs)
    # time-invariant above
    ,"wave"          # Follow-up year --- --- --- --- --- --- --- --- --- ---
    ,"fu_year"       # Follow-up year --- --- --- --- --- --- --- --- --- ---
    # time-variant below
    ,"firstobs"      # indicator of first observation for a person
    ,"date_at_visit" # perturbed date of visit
    ,"age_at_visit"  # age at cycle - fractional  
    ,"age"           # same as above, but standardized name for `msm` consumption
    ,"htm"           # height in meters
    ,"bmi"           # Body Mass Index  in kilograms per meter squared (kg/msq)
    ,"physact"       # Physical activity (sum of 5 items)
    ,"gait"          # Gait Speed in minutes per second (min/sec)
    ,"grip"          # Extremity strength in pounds (lbs)
    ,"cogn_global"   # global cognition
    ,"dementia"      # dementia diagnosis (?)
    ,"mmse"          # mini mental state exam (max =30)
    ,"state"         # multistate variable created for model
  ) 


# ---- inspect-data-3 -----------------------------------------
head(ds)
ds$id %>% unique() %>% length() # number of unique ids in ds
ds %>% dplyr::glimpse(90)
ds %>% dplyr::filter(id==33027) %>% as.data.frame()
ds %>% dplyr::filter(id==402800) %>% as.data.frame()

# subset a smaller cohort for testing
set.seed(41)
sample_size <- 100
ids <- sample(ds$id,300)
# d <- ds %>% dplyr::filter(id %in% ids) 
# ds <- ds %>% dplyr::filter(id %in% ids) 

# ---- basic-table --------------------------------------------------------------

# ---- basic-graphs --------------------------------------------------------------

# ---- age-0-intervals -----------
# Time intervals in data:
# the age difference between timepoint for each individual
intervals <- matrix(NA,nrow(ds),2)
for(i in 2:nrow(ds)){
  if(ds$id[i]==ds$id[i-1]){
    intervals[i,1] <- ds$id[i]
    intervals[i,2] <- ds$age[i]-ds$age[i-1]
  }
  intervals <- as.data.frame(intervals)
  colnames(intervals) <- c("id", "interval")
}
cat("\n Minimum interval length : ",min(intervals[,2], na.rm=T))
cat("\n Maximum interval length : ", max(intervals[,2], na.rm=T))
# the age difference between timepoint for each individual
intervals <- intervals[!is.na(intervals[,2]),] # Remove NAs:
cat("\nTime intervals between observations within individuals:\n")
print(round(quantile(intervals[,2]),digits))

# Info on age and time between observations:
cat("\n Graphs of age distribution :\n")
opar<-par(mfrow=c(1,3), mex=0.8,mar=c(5,5,3,1))
hist(ds$age[ds$firstobs==1],col="red",xlab="Age at baseline in years",main="")
hist(ds$age,col="blue",xlab="Age in data in years",main="")
hist(intervals[,2],col="green",xlab="Time intervals in data in years",main="")
opar<-par(mfrow=c(1,1), mex=0.8,mar=c(5,5,2,1))


# ---- age-1 ----------------------------------------------------------------
ds %>% histogram_continuous("age_at_bl", bin_width=1)

# ---- age-2 ----------------------------------------------------------------
ds %>% histogram_continuous("birth_year", bin_width=1)

# ---- age-3 ----------------------------------------------------------------
ds %>% histogram_continuous("age_at_death", bin_width=1)

# ---- age-4 ----------------------------------------------------------------
ds %>% dplyr::distinct(id,died) %>% histogram_discrete("died")
# Age at baseline for those who we know have not died yet
ds %>% dplyr::filter(died==0) %>% histogram_continuous("age_at_bl", bin_width=1)
# Age at baseline for those who we know have died
ds %>% dplyr::filter(died==1) %>% histogram_continuous("age_at_bl", bin_width=1)


# ---- edu-1 -----------------------------------------------------------
# The measure of educate was taken only once - at baseline
# shows (1) sample's descriptives of the measure at each wave
#       (2) attrition of sample on this measurement
ds %>% 
  dplyr::filter(!is.na(edu)) %>% 
  dplyr::group_by_("fu_year") %>% 
  dplyr::summarize(average_years_edu=mean(edu),
                   SD=sd(edu),
                   observed_n = n())


# ---- height-1 -----------------------------------------------------------
# The measure of height is taken at each wave, let's reivew  data for a few cases
ds %>% view_temporal_pattern("htm",     4)
ds %>% view_temporal_pattern("htm_bl",  4)
ds %>% view_temporal_pattern("htm_med", 4)
# Examine ditribution of unique values and summary statistics at each wave
ds %>% dplyr::mutate(htm     = round(htm,1))     %>% over_waves("htm"); 
ds %>% dplyr::mutate(htm_bl  = round(htm_bl,1))  %>% over_waves("htm_bl");
ds %>% dplyr::mutate(htm_med = round(htm_med,1)) %>% over_waves("htm_med"); 

# ---- height-2 -----------------------------------------------------------
# Distribution of temporally flattened measures of height
ds %>% TabularManifest::histogram_continuous("htm_bl", bin = .05)
ds %>% TabularManifest::histogram_continuous("htm_med", bin = .05)


# ---- bmi-1 -----------------------------------------------------------
# The measure of BMI is taken at each wave, let's reivew  data for a few cases
ds %>% view_temporal_pattern("bmi",     4)
ds %>% view_temporal_pattern("bmi_bl",  4)
ds %>% view_temporal_pattern("bmi_med", 4)
# Examine ditribution of unique values and summary statistics at each wave
ds %>% dplyr::mutate(bmi     = round(bmi,0))     %>% over_waves("bmi"); 
ds %>% dplyr::mutate(bmi_bl  = round(bmi_bl,0))  %>% over_waves("bmi_bl");
ds %>% dplyr::mutate(bmi_med = round(bmi_med,0)) %>% over_waves("bmi_med"); 



# ---- gait-1 ---------------------------------------------
# The measure of `gait` was taken at each wave, let's reivew  data for a few cases
ds %>% view_temporal_pattern("gait",     4)
ds %>% view_temporal_pattern("gait_bl",  4)
ds %>% view_temporal_pattern("gait_med", 4)
# Examine ditribution of unique values and summary statistics at each wave
ds %>% dplyr::mutate(gait     = round(gait,0))     %>% over_waves("gait"); 
ds %>% dplyr::mutate(gait_bl  = round(gait_bl,0))  %>% over_waves("gait_bl");
ds %>% dplyr::mutate(gait_med = round(gait_med,0)) %>% over_waves("gait_med"); 

# ---- gait-2 ---------------------------------------------
# distribution of scores at baseline
ds %>% 
  dplyr::filter(fu_year == 0) %>% 
  TabularManifest::histogram_continuous("gait", bin_width =  .05)
# ---- gait-3 ---------------------------------------------
# observed trajectories 
ds %>%   dplyr::filter(id %in% ids) %>% raw_smooth_lines_v2( "gait")



# ---- grip-1 ---------------------------------------------
# The measure of `grip` was taken at each wave, let's reivew  data for a few cases
ds %>% view_temporal_pattern("grip",     4)
ds %>% view_temporal_pattern("grip_bl",  4)
ds %>% view_temporal_pattern("grip_med", 4)
# Examine ditribution of unique values and summary statistics at each wave
ds %>% dplyr::mutate(grip     = round(grip,0))     %>% over_waves("grip"); 
ds %>% dplyr::mutate(grip_bl  = round(grip_bl,0))  %>% over_waves("grip_bl");
ds %>% dplyr::mutate(grip_med = round(grip_med,0)) %>% over_waves("grip_med"); 


# ---- grip-2 ---------------------------------------------
# distribution of scores at baseline
ds %>% 
  dplyr::filter(fu_year == 0) %>% 
  TabularManifest::histogram_continuous("grip", bin_width =  .05)

# ---- grip-3 ---------------------------------------------
# observed trajectories 
ds %>%   dplyr::filter(id %in% ids) %>% raw_smooth_lines_v2( "grip")


# ---- mmse-1 ---------------------------------------------
# Examine ditribution of unique values and summary statistics at each wave
ds %>% dplyr::mutate(mmse     = round(mmse,0))     %>% over_waves("mmse"); 

# ---- mmse-2 ---------------------------------------------
# distribution of scores at baseline
ds %>% 
  dplyr::filter(fu_year == 0) %>% 
  TabularManifest::histogram_continuous("mmse", bin_width = 1)

# ---- mmse-3 ---------------------------------------------
# observed trajectories 
ds %>%   dplyr::filter(id %in% ids) %>% raw_smooth_lines_v2( "mmse")



# ----- cognition-global-1 -----------------------
ds %>% 
  dplyr::select(id,fu_year, cogn_global) %>% 
  dplyr::filter(!is.na(cogn_global)) %>%
  dplyr::group_by(fu_year) %>% 
  dplyr::summarize(average_global_cognition = round(mean(cogn_global),3),
                   sd = sprintf("%0.2f",sd(cogn_global)), 
                   observed_n =n()) 

# ----- cognition-global-2 -----------------------
# distribution of scores at baseline
ds %>% 
  dplyr::filter(fu_year == 0) %>% 
  TabularManifest::histogram_continuous("cogn_global", bin_width =  .05)

# ----- cognition-global-3 -----------------------
ds %>% 
  dplyr::filter(id %in% ids) %>% 
  raw_smooth_lines_v2("cogn_global")

# ----- dementia-1 -------------------------------------------------
dd <- ds %>% 
  dplyr::filter(!is.na(dementia)) %>% 
  dplyr::group_by_("fu_year") %>% 
  dplyr::summarize(percent_diagnosed=mean(dementia),
                   observed_n = n()) 
dd

g <- ggplot2::ggplot(dd, aes_string(x="fu_year",y="percent_diagnosed")) 
g <- g + geom_line(na.rm = T)
g <- g + main_theme
g

dd <- ds %>% 
  dplyr::filter(!is.na(dementia)) %>% 
  dplyr::mutate(age_cat = cut(age_at_visit,breaks = 10)) %>% 
  dplyr::group_by_("age_cat") %>%
  dplyr::summarize(percent_diagnosed=mean(dementia),
                   observed_n = n())  
dd

g <- ggplot2::ggplot(dd, aes_string(x="age_cat",y="percent_diagnosed")) 
g <- g + geom_bar(stat="identity")
g <- g + main_theme
g

# ---- publish ---------------------------------------
path_report_1 <- "./reports/descriptives/map/review-variables-map.Rmd"
# path_report_2 <- "./reports/*/report_2.Rmd"
# allReports <- c(path_report_1,path_report_2)
allReports <- c(path_report_1)

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




## support code below




# ----- test ----------
ds %>% histogram_continuous("age_at_death", bin_width=1)

ds %>% histogram_discrete("msex")

# print an elemental graph
d %>% basic_line_v2("cogn_global", "fu_year", "salmon", .9, .1, T)
# print complex graph of various applications of elemental graphs
d %>% raw_smooth_lines_v2("cogn_global")
# d %>% raw_smooth_lines_v2("cogact_old")
# d %>% raw_smooth_lines_v2("socact_old")
# d %>% raw_smooth_lines_v2("soc_net")
# ds %>% raw_smooth_lines_v2("soc_net",line_size=.3, line_alpha=.2)

ds %>% 
  dplyr::filter(fu_year == 0) %>% 
  TabularManifest::histogram_continuous("gait")

# ---- testing --------------------------

psych::summary.psych(ds)

library(dplyr)
library(ggplot2)
ids <- d$id %>%  unique() %>% sample(1)
d %>% 
  dplyr::filter(id %in% ids) %>% 
  print()

d <- ds %>% 
  group_by(id) %>% 
  mutate(unique = length(unique(income_40))) %>% 
  ungroup() %>% 
  select(id,fu_year,income_40, unique) %>% 
  arrange(desc(unique)) %>% 
  as.data.frame()
head(d)
table(d$unique)

g <- ggplot2::ggplot(ds, aes(x=fu_year, y=age_at_visit)) +
       geom_point(aes(color=male)) +
  facet_(.~ income_40) 
g

d <- ds %>% 
  # filter(fu_year == 0)
  dplyr::select(everything())

g <- d %>% 
  ggplot(aes(x=gait))+
  geom_histogram()+
  # geom_histogram(aes(y = (..count..)/sum(..count..))) + 
  # scale_y_continuous(labels=percent)
  facet_wrap(~fu_year)+
  theme_bw()
g


g <- d %>% 
  # ggplot(aes(x = fu_year, y = gait))+
  # ggplot(aes(x = age_at_visit, y = gait))+
  ggplot(aes(x = age_at_visit, y = grip))+
  geom_line(aes(group = id), alpha = .3 )+
  geom_smooth(method = "loess", se = T, alpha = .2)+
  # geom_smooth(aes(group = id), alpha = .3 )+
  # geom_smooth(aes(group = id), alpha = .1, method = "lm", se = F )+
  # geom_smooth(aes(group = id), alpha = .1, method = "lm", se = F )+
  # geom_point(shape = 21, fill = NULL, size = 2)+
  theme_bw()
g
  






