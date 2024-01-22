# create descriptives for the paper
# Jaime Benavides 6/8/23
# First step to load packages etc.
rm(list=ls())
# 1a Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'init_directory_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

exposures <- readRDS(paste0(generated.data.folder, "exposures_for_profiles_rev_shs_rev_valid_part.rds"))
exposures_pren <- exposures[exposures$month == 0,]

# add covariates

data_path <- "/home/jbenavides/maklab/scratch/data/health/amy_r01_aim1/raw_data/"
ccceh_data_path <- paste0(data_path, "CCCEH_Data/")

# read covariates 
covariates <- readRDS(paste0(generated.data.folder, "covariates.rds"))

## Table 1 and ## Table S2
data <- dplyr::left_join(exposures_pren, covariates, by = "SID")
summary(data)


# descriptives
# sex
data %>% count('GENDER')
# ethnicity
data %>% count('ethnicity')

# age 
min(data$age, na.rm = T)
max(data$age, na.rm = T)
mean(data$age, na.rm = T)
sd(data$age, na.rm = T)

# maternal education
data %>% count('mat_ed_lvl')

# maternal adhd
mean(data$TSC_H, na.rm = T)
sd(data$TSC_H, na.rm = T)
min(data$TSC_H, na.rm = T)
max(data$TSC_H, na.rm = T)
# maternal IQ
mean(data$T3QT, na.rm = T)
sd(data$T3QT, na.rm = T)
min(data$T3QT, na.rm = T)
max(data$T3QT, na.rm = T)

# home quality environment
mean(data$HOMETOT, na.rm = T)
sd(data$HOMETOT, na.rm = T)
min(data$HOMETOT, na.rm = T)
max(data$HOMETOT, na.rm = T)
# birth weight
mean(data$B11, na.rm = T)
sd(data$B11, na.rm = T)
min(data$B11, na.rm = T)
max(data$B11, na.rm = T)

# exposure profiles N=438
mon <- 0
na_level <- 50
scale <- "TRUE"
case <- "na_50_reduced_rev_grav_corr_rev_shs_rev_valid_part"
expo_prep <- readRDS(paste0(generated.data.folder, "data_row_comp_", case, ".rds"))


data <- dplyr::left_join(expo_prep, covariates, by = "SID")
summary(data)


# descriptives
# sex
data %>% count('GENDER')
# ethnicity
data %>% count('ethnicity')

# age 
min(data$age, na.rm = T)
max(data$age, na.rm = T)
mean(data$age, na.rm = T)
sd(data$age, na.rm = T)

# maternal education
mat_ed <- data %>% count('mat_ed_lvl')
mat_ed$perc <- mat_ed$freq / length(which(!is.na(data$mat_ed_lvl))) * 100
# maternal adhd
mean(data$TSC_H, na.rm = T)
sd(data$TSC_H, na.rm = T)
min(data$TSC_H, na.rm = T)
max(data$TSC_H, na.rm = T)
# maternal IQ
mean(data$T3QT, na.rm = T)
sd(data$T3QT, na.rm = T)
min(data$T3QT, na.rm = T)
max(data$T3QT, na.rm = T)

# home quality environment
mean(data$HOMETOT, na.rm = T)
sd(data$HOMETOT, na.rm = T)
min(data$HOMETOT, na.rm = T)
max(data$HOMETOT, na.rm = T)
# birth weight
mean(data$B11, na.rm = T)
sd(data$B11, na.rm = T)
min(data$B11, na.rm = T)
max(data$B11, na.rm = T)

# behavior profiles N=322
mon <- 192
na_level <- 75
scale <- "TRUE"
case <- paste0(mon/12, "_yrs_na_", na_level)
# data prepared at A_01_pcp_outcomes_rrmc_grid_search
outc_prep <- readRDS(paste0(generated.data.folder, "data_with_ids_outc_",mon/12, "_yrs_na_", na_level ,"_scale", "_", scale, ".rds"))


data <- dplyr::left_join(outc_prep, covariates, by = "SID")
## Table S5 and ## Table S6
summary(data)


# descriptives
# sex
data %>% count('GENDER')
# ethnicity
data %>% count('ethnicity')

# age 
min(data$age, na.rm = T)
max(data$age, na.rm = T)
mean(data$age, na.rm = T)
sd(data$age, na.rm = T)

# maternal education
mat_ed <- data %>% count('mat_ed_lvl')
mat_ed$perc <- mat_ed$freq / length(which(!is.na(data$mat_ed_lvl))) * 100
# maternal adhd
mean(data$TSC_H, na.rm = T)
sd(data$TSC_H, na.rm = T)
min(data$TSC_H, na.rm = T)
max(data$TSC_H, na.rm = T)
# maternal IQ
mean(data$T3QT, na.rm = T)
sd(data$T3QT, na.rm = T)
min(data$T3QT, na.rm = T)
max(data$T3QT, na.rm = T)

# home quality environment
mean(data$HOMETOT, na.rm = T)
sd(data$HOMETOT, na.rm = T)
min(data$HOMETOT, na.rm = T)
max(data$HOMETOT, na.rm = T)
# birth weight
mean(data$B11, na.rm = T)
sd(data$B11, na.rm = T)
min(data$B11, na.rm = T)
max(data$B11, na.rm = T)
