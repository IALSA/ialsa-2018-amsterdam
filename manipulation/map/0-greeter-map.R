# The purpose of this script is to create a data object (dto) which will hold all data and metadata.
# Run the lines below to stitch a basic html output. 
# knitr::stitch_rmd(
#   script="./manipulation/map/0-ellis-island-map.R",
#   output="./manipulation/map/stitched-output/0-ellis-island-map.md"
# )
# The above lines are executed only when the file is run in RStudio, !! NOT when an Rmd/Rnw file calls it !!

rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  
# Ideally, no real operations are performed in these sourced scripts. 
source("./scripts/functions-common.R") # used in multiple reports

# ---- load-packages ----------------------------------------------
# Attach packages so their functions don't need to be qualified when used
# See more : http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # Pipes
library(ggplot2) # Graphs
# library(tidyverse)
# Functions of these packages will need to be qualified when used
# See more: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("tidyr") #  data manipulation
requireNamespace("dplyr") # f() names conflict with other packages (esp. base, stats, and plyr).
requireNamespace("testit") # for asserting conditions meet expected patterns.

# ---- declare-globals ----------------------------------------------
# reach out to the curator for a dataset prepared for general consumption
path_input  <- "../MAP/data-unshared/derived/dto.rds"
# this is where the output of the this script will be stored:
path_output <- "./data/unshared/derived/dto-0-greeted.rds"


# point to the local metadata to be used for this project (specific consumption)
# metadata_path_input <- "./data/shared/meta/map/meta-data-map.csv" 
metadata_path_input <- "./data/shared/meta/map/meta-data-map-2017-12-28.csv" 

# ---- load-data ------------------------------------------------
# load data objects
dto      <- readRDS(path_input)
lapply(dto,names)
# dto already contains meta data (a generic one), but you should develop/load a local one for greater control
meta_data_local <- read.csv(metadata_path_input, stringsAsFactors=F, header=T)
# How to create: you would start with the meta-data provided with the curator and make custom adjustment

# ---- inspect-data ----------------------------------------------
# inspect loaded data objects (using basic demographic variables )
ds <- as.data.frame(dto$unitData) # assing alias to use in this session
length(unique(ds$id))  # sample size, number of respondents
ds %>% names()
# t <- table(ds[,"fu_year"], ds[,"died"]); t[t==0]<-".";t 
# t <- table(ds[,"msex"], ds[,"race"], useNA = "always"); t[t==0]<-".";t 
# t <- table(ds[,"educ"], ds[,"race"]); t[t==0]<-".";t 



# ---- chose-metadata -----------------------------------------------

# meta_data <- dto$metaData # original, supplied by the ialsa-study-curator
# alternatively, load a local meta-data file that adds on to the generic version from the curator
meta_data <- meta_data_local

## This chunk chooses the file to be used as project's meta-data file

# ----- view-metadata-1 ---------------------------------------------
# 
meta_data %>% 
  dplyr::arrange(type, construct, name) %>% 
  dplyr::select(name, name_new, label, construct, longitudinal) %>% 
  # head() %>% 
  knitr::kable()


# ----- apply-meta-data-1 -------------------------------------
# create ds that would articulate the renaming rules to be applied
ds_renamings <- 
  dplyr::left_join(
    # 1st object
    tibble::tibble(
      "name_raw" = ds %>% names()
    ),
    # 2nd object
    meta_data %>% 
      dplyr::select(name, name_new),
    # by clause
    by = c("name_raw"="name")
    ) %>% 
  # additional tranformations
  dplyr::mutate(
    # create values for variable names in the future data set
    renamed = ifelse(is.na(name_new),name_raw,name_new)
  )
names(ds) <- ds_renamings$renamed
ds %>% dplyr::glimpse()

# transfer changes to dto
ds <- ds %>% dplyr::filter(study == "MAP ") # in case we have multiple studies (not really relevant here)
table(ds$study) # should be only one study
dto[["unitData"]] <- ds


# ----- tweak-data -----------------------
# apply general tweaks on  unit data
ds <- ds 

# ---- assemble-data-object-dto-1 --------------------------------------
# reconstruct the dto to be used in this project
dto <- list()
# the first element of data transfer object contains unit data
dto[["unitData"]] <- ds
# the second element of data transfer object contains meta data
dto[["metaData"]] <-  meta_data # new, local meta-data!!, disable if using original metadata
# verify and glimpse
# dto[["unitData"]] %>% dplyr::glimpse()
# dto[["metaData"]] %>% dplyr::glimpse()

# ---- save-to-disk ------------------------------------------------------------

# Save as a compressed, binary R dataset.  
# It's no longer readable with a text editor, but it saves metadata (eg, factor information).
saveRDS(dto, file=path_output, compress="xz")

# ---- object-verification ------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
dto <- readRDS(path_output)
names(dto)
# 1st element - unit(person) level data
# 2nd element - meta data, info about variables
knitr::kable(names_labels(dto$unitData))












