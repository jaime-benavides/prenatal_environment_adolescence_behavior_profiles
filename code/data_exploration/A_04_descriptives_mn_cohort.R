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
prenatal <- read_csv(paste0(ccceh_data_path, "PRENATAL.csv")) # b11
prenatal <- prenatal[,c("SID","A04_0", "A18_0")]
prenatal <- prenatal %>% dplyr::rename(mat_ed_lvl = A04_0, ethnicity = A18_0)
bchart <- read_csv(paste0(ccceh_data_path, "BCHART.csv")) # b11
bchart <- bchart[,c("SID", "B11")]
caars <- read_csv(paste0(ccceh_data_path, "CAARS.csv"))
caars <- caars[,c("SID", "TSC_H")] 
caars <- caars[which(complete.cases(caars)),]
caars <- caars[-which(duplicated(caars$SID)),]
wasi <- read_csv(paste0(ccceh_data_path, "WASI.csv")) 
colnames(wasi)[1] <- "SID"
wasi <- wasi[,c("SID", "TEST_Y", "DOB_Y", "AGE_Y", "AGE_M", "WASI_PRI_C", "WASI_VCI_C")]
wasi$age <- wasi$TEST_Y - wasi$DOB_Y
wasi$months <- wasi$AGE_Y*12 + wasi$AGE_M
wasi <- wasi[,c("SID", "age", "DOB_Y", "WASI_PRI_C", "WASI_VCI_C")] 
wasi <- wasi[which(complete.cases(wasi)),]
wasi <- wasi[-which(duplicated(wasi$SID)),]
toni <- haven::read_sav(paste0(ccceh_data_path, "TONI3.sav"))
toni<- toni[c("SID", "T3QT")]
gender <- read_csv(paste0(ccceh_data_path, "GENDER.csv")) 
gender <- gender[,c("SID", "GENDER")]
wisc <- read_csv(paste0(ccceh_data_path, "WISC.csv"))  # 1,086 x 69 16-18Y: Wechsler Intelligence Scale for Children IV, FSIQ, all scores
wisc <- wisc[which(wisc$MONTHS==84),] # 523 subjects
wisc <- wisc[,c("SID", "WSC_DS", "WSC_COD")]
home <- read_csv(paste0(ccceh_data_path, "HOME.csv"))  # 545 x 19 CEH: Early Childhood Home Inventory
home <- home[,c("SID", "HOMETOT")]

prenatal$ethnicity <- as.character(prenatal$ethnicity)

prenatal$ethnicity[prenatal$ethnicity == "3"] <- "0" #Dominican

prenatal$ethnicity[prenatal$ethnicity == "4"] <- "0" # other hispanic

prenatal$ethnicity[prenatal$ethnicity == "5"] <- "1" # african american
prenatal$ethnicity <- as.numeric(prenatal$ethnicity)


sids <- unique(c(home$SID, wisc$SID, wasi$SID, prenatal$SID, bchart$SID, caars$SID, gender$SID, toni$SID))
sids <- sids[order(sids)]
covariates <- data.frame(SID = sids)
covariates <- covariates %>%
  dplyr::left_join(gender, by = c("SID")) %>%
  dplyr::left_join(prenatal, by = c("SID")) %>%
  dplyr::left_join(wisc, by = c("SID"))  %>%
  dplyr::left_join(wasi, by = c("SID")) %>%
  dplyr::left_join(home, by = c("SID")) %>%
  dplyr::left_join(bchart, by = c("SID")) %>%
  dplyr::left_join(caars, by = c("SID")) %>%
  dplyr::left_join(toni, by = c("SID"))


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
