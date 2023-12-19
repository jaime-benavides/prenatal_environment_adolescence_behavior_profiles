# read ids from cohort subjects
# Jaime Benavides 6/9/23
# First step to load packages etc.
rm(list=ls())
# 1a Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'init_directory_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

data <- read_sav("/home/jbenavides/maklab/scratch/data/health/amy_r01_aim1/Typesamp_only.sav")  
mn_cohort_participants_sids <- data$SID[(which(!is.na(data$ENRLSTAT)))]
saveRDS(mn_cohort_participants_sids, paste0(generated.data.folder, "mn_cohort_participants_sids.rds"))
