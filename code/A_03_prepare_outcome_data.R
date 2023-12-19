# First step to load packages etc.
rm(list=ls())
.libPaths(c(.libPaths(), "/home/jbenavides/R/x86_64-pc-linux-gnu-library/4.1"))
library(rvest)
library(qdapRegex)
library(stringi)
# 1a Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'0_01_init_directory_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

# data paths
data_path <- "/home/jbenavides/maklab/scratch/data/health/amy_r01_aim1/raw_data/"
ccceh_data_path <- paste0(data_path, "CCCEH_Data/")
ysr_data_path <- paste0(data_path, "/YSR_foranalysis/AR_EK_YSRs/")

file_names <- system(paste0("ls ", ccceh_data_path), intern = TRUE)
file_names <- gsub(".csv", "",file_names)
table_names <- file_names[-which(grepl( ".sav", file_names, fixed = TRUE))]
codebook <- readRDS(paste0(generated.data.folder, "codebook_mn.rds"))

# Prodromal Questionnaire - Brief Version, 

# , Swanson, Nolan, and Pelham (SNAP-IV) Questionnaire, 
# Dupaul Barkley ADHD Rating Scale, 
# SES questionnaire at age 16 (SES16), 
# and demographic questionnaires at every age. 
# load ccceh_data
# cov? CEH: Babys Medical Record Review bchart <- read_csv(paste0(ccceh_data_path, "BCHART.csv")) # 710 x 30   
# bchart <- read_csv(paste0(ccceh_data_path, "BCHART.csv")) # to obtain sid in ysr_14
# exp48hr <- read_csv(paste0(ccceh_data_path, "EXP48HR.csv"))  # 756 x 468
# cov? month12 <- read_csv(paste0(ccceh_data_path, "MONTH12.csv"))  # 609 x 779
# cov? month84 <- read_csv(paste0(ccceh_data_path, "MONTH84.csv"))  # 522 x 694

# cov sgm6 <- read_csv(paste0(ccceh_data_path, "SGM6.csv"))  # 496 x 5
# cov wisc16 <- read_csv(paste0(ccceh_data_path, "WISC16.csv"))  # 317 x 30
# cov wppsi_sw <- read_csv(paste0(ccceh_data_path, "WPPSI_SW.csv")) # 391 x 68 / sibid 111 sid 368?

# cov home <- read_csv(paste0(ccceh_data_path, "HOME.csv"))  # 545 x 19
# cov month24 <- read_csv(paste0(ccceh_data_path, "MONTH24.csv")) # 566 x 779
# cov ses16 <- read_csv(paste0(ccceh_data_path, "SES16.csv"))  # 288 x 179
# cov: tanner <- read_csv(paste0(ccceh_data_path, "TANNER.csv"))  # 473 x 21
# cov Wechsler Intelligence Scale for Children wisc <- read_csv(paste0(ccceh_data_path, "WISC.csv"))  # 1,086 x 69
# cov year09 <- read_csv(paste0(ccceh_data_path, "YEAR09.csv"))  # 476 x 711

# CAARS Self-Report Long Version Score Sheet
# The Conners' Adult ADHD Rating Scales–Self Report: Long Version (CAARS–S:L) 
# adult to provide valuable information about themselves.
# cov: caars <- read_csv(paste0(ccceh_data_path, "CAARS.csv"))  # 628 x 37

# cov? month36 <- read_csv(paste0(ccceh_data_path, "MONTH36.csv"))  # 562 x 779
# cov? prenatal <- read_csv(paste0(ccceh_data_path, "PRENATAL.csv"))  # 845 x 975
# cov? specific gravity sgm1 <- read_csv(paste0(ccceh_data_path, "SGM1.csv"))  # 562 x 5

# cov wppsi <- read_csv(paste0(ccceh_data_path, "WPPSI.csv"))  # 543 x 15
# cov year11 <- read_csv(paste0(ccceh_data_path, "YEAR11.csv"))  #  378 x 748
# cov maternal food insecurity mfi_mom <- read_csv(paste0(ccceh_data_path, "MFI_MOM.csv"))  # 995 x 29
# cov month60 <- read_csv(paste0(ccceh_data_path, "MONTH60.csv"))  # 551 x 680
# cov puberty <- read_csv(paste0(ccceh_data_path, "PUBERTY.csv"))  # 578 x 18
# cov? specific gravity sgm5 <- read_csv(paste0(ccceh_data_path, "SGM5.csv"))  # 429 x 5
# cov wasi <- read_csv(paste0(ccceh_data_path, "WASI.csv"))  # 364 x 55
# cov wppsi_iii <- read_csv(paste0(ccceh_data_path, "WPPSI_III.csv"))  
# cov year14 <- read_csv(paste0(ccceh_data_path, "YEAR14.csv"))  # 355 x 844

# ADHD 16 / 16-18Y DuPaul Barkley ADHD RATING SCALE IV
adhd16 <- read_csv(paste0(ccceh_data_path, "ADHD16.csv")) # 248 x 56
adhd16$month <- 192 # equivalent to 16 years old, as a proxy of the 16-18 years old label, it would be more accurate to subtract born date to the visit date 
adhd16 <- adhd16[,c("SID", "month",  "ADHDC01",  "ADHDC02",  "ADHDC03",  "ADHDC04",
                    "ADHDC05",  "ADHDC06",  "ADHDC07",  "ADHDC08",  "ADHDC09",  "ADHDC10",  "ADHDC11",  "ADHDC12",
                    "ADHDC13",  "ADHDC14",  "ADHDC15",  "ADHDC16",  "ADHDC17",  "ADHDC18",
                    "ADHDW01",  "ADHDW02",  "ADHDW03",  "ADHDW04",  "ADHDW05",
                    "ADHDW06",  "ADHDW07",  "ADHDW08",  "ADHDW09",  "ADHDW10",  "ADHDW11",  "ADHDW12",  "ADHDW13",
                    "ADHDW14",  "ADHDW15",  "ADHDW16",  "ADHDW17",  "ADHDW18", "ADHDCTOT", "ADHDCIA", "ADHDCHI", "ADHDWTOT", "ADHDWIA", "ADHDWHI")]
# in all cases 0 is better and 3 is worst, ADHDC means current and ADHDW means worst ever
inattention_ind <- c("01", "03", "05", "07", "09", "11", "13", "15","17")
hyperact_ind <- c("02", "04", "06", "08", "10", "12", "14", "16", "18")
adhd16$ADHDCIA <- rowSums(adhd16[,paste0("ADHDC", inattention_ind)])
adhd16$ADHDCHI <- rowSums(adhd16[,paste0("ADHDC", hyperact_ind)])
adhd16$ADHDCTOT <- adhd16$ADHDCIA + adhd16$ADHDCHI
adhd16$ADHDWIA <- rowSums(adhd16[,paste0("ADHDW", inattention_ind)])
adhd16$ADHDWHI <- rowSums(adhd16[,paste0("ADHDW", hyperact_ind)])
adhd16$ADHDWTOT <- adhd16$ADHDWIA + adhd16$ADHDWHI
saveRDS(adhd16, paste0(generated.data.folder, "adhd16.rds"))

# adhd description
adhd_vars <- colnames(adhd16)[-c(1,2)]
adhd_vars_desc <- codebook$variable_description[match(adhd_vars, codebook$variable_name)]
adhd_desc <- data.frame(variable_name = adhd_vars, 
                        variable_description = adhd_vars_desc, 
                        outcome = adhd_vars, 
                        month = adhd16$month[1], 
                        family = "Attention-deficit/hyperactivity disorder", 
                        table_name = "ADHD16", 
                        table_description = "16-18Y DuPaul Barkley ADHD RATING SCALE IV")

# Youth Risk Behavioral Surveillance System (YRBSS)
yrb <- openxlsx::read.xlsx(paste0(data_path, "YRBS21mod.xlsx"))
# questions are yes/no yes = 1 in the dataset and no = 2, i am summing them in a 0-1 scale
# cigarretes Q35
# alcohol Q46
# marihuana Q54
# cocaine Q58
# sniffed glue Q60
# heroine Q62
# methamphetamines Q64
# ecstasy Q66

subset_ks <- c("SIDnum", "Q35", "Q46", "Q54", "Q58", "Q60", "Q62", "Q64", "Q66")
yrb <- yrb[,subset_ks]
yrb[,-1] <- abs(yrb[,-1] - 2)
yrb$risky_drug_use <- rowSums(yrb[,-1], na.rm = T)
yrb <- yrb[,c("SIDnum", "risky_drug_use")]
colnames(yrb)[1] <- "SID"
yrb$month <- 192

saveRDS(yrb, paste0(generated.data.folder, "yrbss16.rds"))
yrbss_desc <- data.frame(variable_name = "risky_drug_use", 
                          variable_description = "items sum Q35,Q46,Q54,Q58,Q60,Q62,Q64,Q66", 
                          outcome = "risky_drug_use", 
                          month = 192, 
                          family = "YRBSS", 
                          table_name = "YRBSS", 
                          table_description = "Youth Risk Behavioral Surveillance System (YRBSS)")

# Timeline Follow-Back Interview
time16 <- read_csv(paste0(ccceh_data_path, "TIME16.csv"))  # 1,192 x 42
# check the number of subjects who have at least one instance different than zero
time16_loc <- time16[,c(1, 3, 7:36)]
time16_loc[which(time16_loc$SUBS == 1),"substance"] <- "Drinks"
time16_loc[which(time16_loc$SUBS == 2),"substance"] <- "Cigarettes"
time16_loc[which(time16_loc$SUBS == 3),"substance"] <- "Joints"
time16_loc[which(time16_loc$SUBS == 4),"substance"] <- "Other drugs"
time16_loc$sum <- rowSums(time16_loc[,c(3,32)], na.rm = TRUE)
time16_loc_sum <- time16_loc[,c("SID", "substance", "sum")] %>% 
  pivot_wider(names_from = substance, values_from = sum)
length(which(time16_loc_sum$Drinks > 0)) / nrow(time16_loc_sum) # 3.3%
length(which(time16_loc_sum$Cigarettes > 0)) / nrow(time16_loc_sum) # 1.3%
length(which(time16_loc_sum$Joints > 0)) / nrow(time16_loc_sum) # 9.6%
length(which(time16_loc_sum$`Other drugs` > 0)) / nrow(time16_loc_sum) # 0.3%
colnames(time16_loc_sum)[5] <- "other_drugs"
time16_loc_sum$month <- 16*12
saveRDS(time16_loc_sum, paste0(generated.data.folder, "time16.rds"))

# time16 description
time16_vars <- colnames(time16_loc_sum)[-c(1,6)]
time16_vars_desc <- c("Sum daily Drinks", "Sum daily Cigarettes", "Sum daily Joints", "Sum daily other drugs")
time16_desc <- data.frame(variable_name = time16_vars, 
                        variable_description = time16_vars_desc, 
                        outcome = time16_vars, 
                        month = time16_loc_sum$month[1], 
                        family = "Drug use", 
                        table_name = "TIME16", 
                        table_description = "16-18 yr: Timeline Followback")


# added days by sum and organized by drug
# Kiddie-Schedule for Affective Disorders and Schizophrenia (KSADS)   

ksads16 <- read_csv(paste0(ccceh_data_path, "KSADS16.csv"))  # 221 x 164
# for each column give unable to code NA
# KS...
# ks1-51 disorders present?
# (1) "Not Present"
# (2) "Possible"
# (3) "Probable"
# (4) "Definite"
# (9) "Unable to Code"
# transform 9 values to NA

# KS11 ADHD Predominantly Inattentive
# KS12 ADHD predominantly Hyperactive/Impulsive
# KS19 "19 Psychosis"
# KS37 
# KS38
# KS39

# visual check which are not varying on the sample, all
subset_ks <- c("KS11", "KS12", "KS19", "KS37", "KS38", "KS39")


# KSA... are year of onset, in an initial approach I leave them out
# KSC...current disorder are mostly na so I leave them out for now
ksads16_loc <- ksads16[,c(1, 5:55)] # there is no value 9 so no need to transform to NA
ksads16_loc <- ksads16_loc[complete.cases(ksads16_loc), ]
ksads16_loc$month <- 16*12
ksads16_loc <- ksads16_loc[,c("SID", "month", subset_ks)]
saveRDS(ksads16_loc, paste0(generated.data.folder, "ksads16.rds"))

# ksads description
ksads16_vars <- colnames(ksads16_loc)[-which(colnames(ksads16_loc) %in% c("SID", "month"))]
ksads16_vars_desc <- codebook$variable_description[match(ksads16_vars, codebook$variable_name)]
ksads16_vars_desc <- gsub(" ", "_", substring(gsub("([0-9])","",ksads16_vars_desc), 2))

ksads16_desc <- data.frame(variable_name = ksads16_vars, 
                          variable_description = ksads16_vars_desc, 
                          outcome = ksads16_vars_desc, 
                          month = ksads16_loc$month[1], 
                          family = "Affective Disorders and Schizophrenia", 
                          table_name = codebook$dta_table_name[which(codebook$variable_name %in% ksads16_vars)], 
                          table_description = codebook$data_table_description[which(codebook$variable_name %in% ksads16_vars)])


# Youth Self Report (YSR) 14 years and 16 years
# N= 223 and there are both raw and scored values
ysr14_scored <- read_csv(paste0(ccceh_data_path, "YSR14_Scored_Data_03_25_2019.csv")) # 82 x 93
# i will not use ysr14 until I have sid
ysr16_scored  <- read_csv(paste0(ccceh_data_path, "YSR_P50_Scored_Data_2021.05.20_SIDsAdded.csv"))
which(duplicated(ysr16_scored$SID)) # there is only one duplicated SID=954 at rows 222 241, I keep the oldest to make difference with 14
ysr16_scored <- ysr16_scored[-222,]
ysr16_scored <- ysr16_scored[-which(ysr16_scored$SID < 0),]# also drop sid -888

# ysr14 does not contain  SID but it has a common identifier with ysr16_scored
ysr14_clean <-ysr14_scored %>%
  mutate(SID = as.numeric(str_sub(AssessedPersonId, start = 2, nchar(AssessedPersonId) -3))-1000) %>% #SIDs are all shown with +1000 but we know this doesnt align with SIDs in the format_YSR14 tables
  mutate(SID = ifelse(nchar(as.character(SID)) >= 5, SID-9000, SID)) 

# I will start working with problems that are loading in the same direction, and the total 
ysr14_clean <- ysr14_clean[,c("SID", colnames(ysr14_clean)[which(endsWith(colnames(ysr14_clean), "_Total"))])]


# 
# ysr16_scored_test <- ysr16_scored %>%
#   mutate(SID_test = as.numeric(str_sub(AssessedPersonId, start = 2, 5))-1000)%>%
#   mutate(SID_test = ifelse(nchar(as.character(SID_test)) >= 5, SID_test-9000, SID_test))



ysr14_clean$month <- 168
ysr14_subset <- ysr14_clean[,c("SID", "month", "Attention_Problems_Total", "Thought_Problems_Total",
                               "Internalizing_Problems_Total", "Externalizing_Problems_Total")]
# attention problems, thought problems, internalizing problems, externalizing problems
saveRDS(ysr14_subset, paste0(generated.data.folder, "yrs14.rds"))

# total problems total is much less than the sum of the other problems, I will start with specific scales of each problem type
ysr16_clean <- ysr16_scored[,c("SID", "Anxious__Depressed_Total", "Withdrawn__Depressed_Total", 
                              "Somatic_Complaints_Total", "Social_Problems_Total", "Thought_Problems_Total", 
                              "Attention_Problems_Total", "Rule_Breaking_Behavior_Total", "Aggressive_Behavior_Total",
                              "Internalizing_Problems_Total", "Externalizing_Problems_Total", "Depressive_Problems_Total", "Total_Problems_Total")]
ysr16_clean$month <- 192
ysr16_subset <- ysr16_clean[,c("SID", "month", "Attention_Problems_Total", "Thought_Problems_Total",
                               "Internalizing_Problems_Total", "Externalizing_Problems_Total")]
saveRDS(ysr16_subset, paste0(generated.data.folder, "yrs16.rds"))


ysr16_subset[,colnames(ysr14_subset)[which(!(colnames(ysr14_subset)  %in% colnames(ysr16_subset)))]] <- NA

ysr_subset <- rbind(ysr14_subset, ysr16_subset)
saveRDS(ysr_subset, paste0(generated.data.folder, "yrs_14_16.rds"))

# YSR 14 description
ysr14_vars <- colnames(ysr14_subset)[-which(colnames(ysr14_subset) %in% c("SID", "month"))]
ysr14_vars_desc <- paste0(gsub("_Total", "", gsub("__", "_", ysr14_vars)), " total")

ysr14_desc <- data.frame(variable_name = ysr14_vars, 
                           variable_description = ysr14_vars_desc, 
                           outcome = gsub("_Total", "", gsub("__", "_", ysr14_vars)), 
                           month = ysr14_subset$month[1], 
                           family = "Behavioral problems", 
                           table_name = "YSR14_Scored_Data_03_25_2019.csv", 
                           table_description = "Youth self report Behavior checklist (14)")

# YSR 16 description
ysr16_vars <- colnames(ysr16_subset)[-which(colnames(ysr14_subset) %in% c("SID", "month"))]
ysr16_vars_desc <- paste0(gsub("_Total", "", gsub("__", "_", ysr16_vars)), " total")

ysr16_desc <- data.frame(variable_name = ysr16_vars, 
                         variable_description = ysr16_vars_desc, 
                         outcome = gsub("_Total", "", gsub("__", "_", ysr16_vars)), 
                         month = ysr16_subset$month[1], 
                         family = "Behavioral problems (YSR)", 
                         table_name = "YSR_P50_Scored_Data_2021.05.20_SIDsAdded.csv", 
                         table_description = "Youth self report Behavior checklist (16)")

# CONNERS Parent Rating Scale (todo: update scales from here)
conners <- read_csv(paste0(ccceh_data_path, "CONNERS.csv"))  # 1342 x 125
conners[which(conners$SUBS == 1), "substance"] <- "Drinks"
conners <- dplyr::rename(conners, month = MONTHS)

conners_loc <- conners[, c(1, 3, which(endsWith(colnames(conners), "_RAW")))]
vars <- c("SID", "month", "PCA_RAW", "PCB_RAW", "PCC_RAW", "PCD_RAW", "PCE_RAW", "PCF_RAW", "PCG_RAW", "PCH_RAW")
conners_loc <- conners_loc[,vars]
saveRDS(conners_loc, paste0(generated.data.folder, "conners.rds"))
# T-Scores describe test performance in terms of the examinee's relative position in the distribution of test scores
# i start with raw scores

# conners description
conners_time_points <- unique(conners_loc$month)
conners_desc <- data.frame()
for (p in 1:length(conners_time_points)){
conners_vars <- colnames(conners_loc)[-c(1,2)]
conners_vars_desc <- substring(codebook[match(colnames(conners_loc)[-c(1,2)], codebook$variable_name), "variable_description"],4)
conners_desc_loc <- data.frame(variable_name = conners_vars, 
                         variable_description = conners_vars_desc, 
                         outcome = gsub(" Raw Score", "", conners_vars_desc), 
                         month = conners_time_points[p], 
                         family = "Behavioral problems (CBRS)", 
                         table_name = "CONNERS", 
                         table_description = "Conners Parent Rating Scale")
conners_desc <- rbind(conners_desc, conners_desc_loc)
}

# 16-18Y Self-Control Scale
scs16 <- read_csv(paste0(ccceh_data_path, "SCS16.csv"))  # 303 x 46
# items direction
increasing_prob_n <- c("02", "03", "04", "06", "08", "09", "10", "11",
                       "12", "14", "17", "19", "20", "21", "23", "25", "28", "29", "31", "32", "33", "34", "35") # higher values represent higher problems
decreasing_prob_n <- c("01", "05", "07", "13", "15", "16", "18", "22", "24",
                       "26", "27", "30", "36") # lower values represent higher problems
# change items in decreasing_prob_n to be aligned with high values high problems 
col_same <- paste0("SCS_",increasing_prob_n) 
col_chg <- paste0("SCS_",decreasing_prob_n) 
scs16[,col_same] <- (scs16[,col_same]-1)/4
scs16[,col_chg] <- (1-(scs16[,col_chg]-1)/4)
scs16$month <- 16*12
scs16 <- scs16[,c(1,47,5:40)]
scs16$total_self_control_prob <- rowSums(scs16[,3:38], na.rm = TRUE)
saveRDS(scs16, paste0(generated.data.folder, "scs16_rev.rds"))

# SCS 16 description
scs16_vars <- colnames(scs16)[-c(1,2)]
scs16_vars_desc <-gsub(" ", "_", substring(gsub("([0-9])","",codebook[match(scs16_vars[-which(scs16_vars == "total_self_control_prob")], codebook$variable_name), "variable_description"]), 2))
scs16_desc <- data.frame(variable_name = scs16_vars, 
                         variable_description = c(scs16_vars_desc, "total_self_control"), 
                         outcome = c(scs16_vars_desc, "total_self_control"), 
                         month = scs16$month[1], 
                         family = "Self-Control (SCS)", 
                         table_name = "SCS16", 
                         table_description = "16-18Y Self-Control Scale")

# save description
outcome_description <- rbind(adhd_desc, time16_desc, ksads16_desc, ysr14_desc, ysr16_desc, conners_desc, scs16_desc, yrbss_desc)
saveRDS(outcome_description, paste0(generated.data.folder, "outcome_description.rds"))
