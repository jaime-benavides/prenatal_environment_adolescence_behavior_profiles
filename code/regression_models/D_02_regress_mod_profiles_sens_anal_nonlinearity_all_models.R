rm(list=ls())
.libPaths(c(.libPaths(), "/home/jbenavides/R/x86_64-pc-linux-gnu-library/4.1"))
# 1a Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'0_01_init_directory_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

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
# WASI_PRI_C Perceptual Reasoning Composite Score / WASI_VCI_C Verbal Comprehension Composite Score
gender <- read_csv(paste0(ccceh_data_path, "GENDER.csv")) 
gender <- gender[,c("SID", "GENDER")]
wisc <- read_csv(paste0(ccceh_data_path, "WISC.csv"))  # 1,086 x 69 16-18Y: Wechsler Intelligence Scale for Children IV, FSIQ, all scores
dt <- wisc %>%
  dplyr::group_by(MONTHS) %>%
  dplyr::summarise(n = n())
pdf(paste0(output.folder, "wisc_age_n.pdf"), height=4, width=6)
grid.table(dt, rows = NULL)
dev.off()
wisc <- wisc[which(wisc$MONTHS==84),] # 523 subjects
# WSC_BYR "Birth Year"
# "WSC_DS" Digit Span scaled score the more the better
# WSC_COD "Coding scaled score the more the better
wisc <- wisc[,c("SID", "WSC_DS", "WSC_COD")]
home <- read_csv(paste0(ccceh_data_path, "HOME.csv"))  # 545 x 19 CEH: Early Childhood Home Inventory
#HOMETOT HOMETOT "Total Score" the more the better
home <- home[,c("SID", "HOMETOT")]

# create a dataframe that includes all the combinations of SID contained in the above datasets

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

## read profiles
# exposure

# case na_50_reduced_rev exposure_profiles <- readRDS(paste0(generated.data.folder, "exposure_pcp_fa_profiles_scores_", case_expo, "_n_433.rds"))
exposure_profiles <- readRDS(paste0(generated.data.folder, "exposure_pcp_fa_profiles_scores_na_50_reduced_rev_grav_corr_rev_shs_rev_valid_part_n_438.rds"))
# outcome
case_outc <- "16_yrs_na_75"
outcome_profiles <- readRDS(paste0(generated.data.folder, "outcome_pcp_fa_profiles_scores_16_yrs_na_75_n_322_rev_scs.rds"))

sid_match <- outcome_profiles$SID[which(outcome_profiles$SID  %in% exposure_profiles$SID)]

outcome_profiles_match <- outcome_profiles[which(outcome_profiles$SID %in% sid_match),]
exposure_profiles_match <- exposure_profiles[which(exposure_profiles$SID %in% sid_match),]

colnames(exposure_profiles_match) <- c("SID", "exposure_prof_2", "exposure_prof_3", "exposure_prof_1")
exposure_profiles_match <- exposure_profiles_match[,c("SID", "exposure_prof_1", "exposure_prof_2", "exposure_prof_3")]
colnames(outcome_profiles_match) <- c("SID", "outcome_prof_3", "outcome_prof_1", "outcome_prof_2")
outcome_profiles_match <- outcome_profiles_match[,c("SID", "outcome_prof_1", "outcome_prof_2", "outcome_prof_3")]
profiles <- dplyr::left_join(exposure_profiles_match, outcome_profiles_match, by = "SID")
data <- dplyr::left_join(profiles, covariates, by = "SID")
# standardize
# leave out gender, which is categorical
my.min <- unlist(lapply(data,function(x) min(x,na.rm=T))) 
my.max <- unlist(lapply(data,function(x) max(x,na.rm=T))) 
non_scale <- which(my.max - my.min == 1)
data[,-c(which(colnames(data) %in% c("SID")),non_scale)] = apply(data[,-c(which(colnames(data) %in% c("SID")),non_scale)], 2, function(a) a/sd(a, na.rm = T))

mod_outc_1_expo_1 <- gam(outcome_prof_1 ~ s(exposure_prof_1)+ GENDER + age + mat_ed_lvl + WSC_DS + WSC_COD + WASI_PRI_C + WASI_VCI_C + HOMETOT + B11 + TSC_H + ethnicity + T3QT, 
          data = data, 
          na.action = na.omit)

mod_outc_1_expo_2 <- gam(outcome_prof_1 ~ s(exposure_prof_2)+ GENDER + age + mat_ed_lvl + WSC_DS + WSC_COD + WASI_PRI_C + WASI_VCI_C + HOMETOT + B11 + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_1_expo_3 <- gam(outcome_prof_1 ~ s(exposure_prof_3)+ GENDER + age + mat_ed_lvl + WSC_DS + WSC_COD + WASI_PRI_C + WASI_VCI_C + HOMETOT + B11 + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_2_expo_1 <- gam(outcome_prof_2 ~ s(exposure_prof_1)+ GENDER + age + mat_ed_lvl + WSC_DS + WSC_COD + WASI_PRI_C + WASI_VCI_C + HOMETOT + B11 + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_2_expo_2 <- gam(outcome_prof_2 ~ s(exposure_prof_2)+ GENDER + age + mat_ed_lvl + WSC_DS + WSC_COD + WASI_PRI_C + WASI_VCI_C + HOMETOT + B11 + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_2_expo_3 <- gam(outcome_prof_2 ~ s(exposure_prof_3)+ GENDER + age + mat_ed_lvl + WSC_DS + WSC_COD + WASI_PRI_C + WASI_VCI_C + HOMETOT + B11 + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_3_expo_1 <- gam(outcome_prof_3 ~ s(exposure_prof_1)+ GENDER + age + mat_ed_lvl + WSC_DS + WSC_COD + WASI_PRI_C + WASI_VCI_C + HOMETOT + B11 + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_3_expo_2 <- gam(outcome_prof_3 ~ s(exposure_prof_2)+ GENDER + age + mat_ed_lvl + WSC_DS + WSC_COD + WASI_PRI_C + WASI_VCI_C + HOMETOT + B11 + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_3_expo_3 <- gam(outcome_prof_3 ~ s(exposure_prof_3)+ GENDER + age + mat_ed_lvl + WSC_DS + WSC_COD + WASI_PRI_C + WASI_VCI_C + HOMETOT + B11 + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mynamestheme <- theme(
  plot.title = element_text(family = "Helvetica", hjust = 0.5, size = (22)),
  axis.title = element_text(family = "Helvetica", size = (30)),
  axis.text = element_text(family = "Helvetica", size = (30))
)

## Figure S1

# configure plots
# general config
sm <- "s(exposure_prof_1)"
x_name <- "Exposure Profile 1"
y_name <- "Behavior Profile 1"

name_plot <- ""
mod <- mod_outc_1_expo_1
ci_col <- "blue"
png(paste0(output.folder, "nonline_sens_anal_med_mod_outc_1_expo_1_rev_2.png"), 900, 460)
p<- gratia::draw(mod,
                 select = sm,  
                 ci_col = ci_col) &  geom_hline(yintercept=0, linetype="dashed", color = "grey", size = 1.5) & 
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) & 
  coord_cartesian(ylim = c(-1, 1)) & 
  scale_x_continuous(expand = c(0, 0)) & 
  ggtitle(name_plot) & 
  xlab(x_name) & 
  ylab(y_name) & 
  theme_bw() &
  mynamestheme
p
dev.off()

sm <- "s(exposure_prof_2)"
x_name <- "Exposure Profile 2"
y_name <- "Behavior Profile 1"

# Street/Sidewalk / main analysis
name_plot <- ""
mod <- mod_outc_1_expo_2
ci_col <- "blue"
png(paste0(output.folder, "nonline_sens_anal_med_mod_outc_1_expo_2_rev_2.png"), 900, 460)
p<- gratia::draw(mod,
                 select = sm,  
                 ci_col = ci_col) &  geom_hline(yintercept=0, linetype="dashed", color = "grey", size = 1.5) & 
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) & 
  coord_cartesian(ylim = c(-1, 1)) & 
  scale_x_continuous(expand = c(0, 0)) & 
  ggtitle(name_plot) & 
  xlab(x_name) & 
  ylab(y_name) & 
  theme_bw() &
  mynamestheme
p
dev.off()


sm <- "s(exposure_prof_3)"
x_name <- "Exposure Profile 3"
y_name <- "Behavior Profile 1"

# Street/Sidewalk / main analysis
name_plot <- ""
mod <- mod_outc_1_expo_3
ci_col <- "blue"
png(paste0(output.folder, "nonline_sens_anal_med_mod_outc_1_expo_3_rev_2.png"), 900, 460)
p<- gratia::draw(mod,
                 select = sm,  
                 ci_col = ci_col) &  geom_hline(yintercept=0, linetype="dashed", color = "grey", size = 1.5) & 
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) & 
  coord_cartesian(ylim = c(-1, 1)) & 
  scale_x_continuous(expand = c(0, 0)) & 
  ggtitle(name_plot) & 
  xlab(x_name) & 
  ylab(y_name) & 
  theme_bw() &
  mynamestheme
p
dev.off()


sm <- "s(exposure_prof_1)"
x_name <- "Exposure Profile 1"
y_name <- "Behavior Profile 2"


name_plot <- ""
mod <- mod_outc_2_expo_1
ci_col <- "blue"
png(paste0(output.folder, "nonline_sens_anal_med_mod_outc_2_expo_1_rev_2.png"), 900, 460)
p<- gratia::draw(mod,
                 select = sm,  
                 ci_col = ci_col) &  geom_hline(yintercept=0, linetype="dashed", color = "grey", size = 1.5) & 
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) & 
  coord_cartesian(ylim = c(-1, 1)) & 
  scale_x_continuous(expand = c(0, 0)) & 
  ggtitle(name_plot) & 
  xlab(x_name) & 
  ylab(y_name) & 
  theme_bw() &
  mynamestheme
p
dev.off()

sm <- "s(exposure_prof_2)"
x_name <- "Exposure Profile 2"
y_name <- "Behavior Profile 2"

# Street/Sidewalk / main analysis
name_plot <- ""
mod <- mod_outc_2_expo_2
ci_col <- "blue"
png(paste0(output.folder, "nonline_sens_anal_med_mod_outc_2_expo_2_rev_2.png"), 900, 460)
p<- gratia::draw(mod,
                 select = sm,  
                 ci_col = ci_col) &  geom_hline(yintercept=0, linetype="dashed", color = "grey", size = 1.5) & 
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) & 
  coord_cartesian(ylim = c(-1, 1)) & 
  scale_x_continuous(expand = c(0, 0)) & 
  ggtitle(name_plot) & 
  xlab(x_name) & 
  ylab(y_name) & 
  theme_bw() &
  mynamestheme
p
dev.off()


sm <- "s(exposure_prof_3)"
x_name <- "Exposure Profile 3"
y_name <- "Behavior Profile 2"

# Street/Sidewalk / main analysis
name_plot <- ""
mod <- mod_outc_2_expo_3
ci_col <- "blue"
png(paste0(output.folder, "nonline_sens_anal_med_mod_outc_2_expo_3_rev_2.png"), 900, 460)
p<- gratia::draw(mod,
                 select = sm,  
                 ci_col = ci_col) &  geom_hline(yintercept=0, linetype="dashed", color = "grey", size = 1.5) & 
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) & 
  coord_cartesian(ylim = c(-1, 1)) & 
  scale_x_continuous(expand = c(0, 0)) & 
  ggtitle(name_plot) & 
  xlab(x_name) & 
  ylab(y_name) & 
  theme_bw() &
  mynamestheme
p
dev.off()



sm <- "s(exposure_prof_1)"
x_name <- "Exposure Profile 1"
y_name <- "Behavior Profile 3"


name_plot <- ""
mod <- mod_outc_3_expo_1
ci_col <- "blue"
png(paste0(output.folder, "nonline_sens_anal_med_mod_outc_3_expo_1_rev_2.png"), 900, 460)
p<- gratia::draw(mod,
                 select = sm,  
                 ci_col = ci_col) &  geom_hline(yintercept=0, linetype="dashed", color = "grey", size = 1.5) & 
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) & 
  coord_cartesian(ylim = c(-1, 1)) & 
  scale_x_continuous(expand = c(0, 0)) & 
  ggtitle(name_plot) & 
  xlab(x_name) & 
  ylab(y_name) & 
  theme_bw() &
  mynamestheme
p
dev.off()

sm <- "s(exposure_prof_2)"
x_name <- "Exposure Profile 2"
y_name <- "Behavior Profile 3"

# Street/Sidewalk / main analysis
name_plot <- ""
mod <- mod_outc_3_expo_2
ci_col <- "blue"
png(paste0(output.folder, "nonline_sens_anal_med_mod_outc_3_expo_2_rev_2.png"), 900, 460)
p<- gratia::draw(mod,
                 select = sm,  
                 ci_col = ci_col) &  geom_hline(yintercept=0, linetype="dashed", color = "grey", size = 1.5) & 
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) & 
  coord_cartesian(ylim = c(-1, 1)) & 
  scale_x_continuous(expand = c(0, 0)) & 
  ggtitle(name_plot) & 
  xlab(x_name) & 
  ylab(y_name) & 
  theme_bw() &
  mynamestheme
p
dev.off()


sm <- "s(exposure_prof_3)"
x_name <- "Exposure Profile 3"
y_name <- "Behavior Profile 3"

# Street/Sidewalk / main analysis
name_plot <- ""
mod <- mod_outc_3_expo_3
ci_col <- "blue"
png(paste0(output.folder, "nonline_sens_anal_med_mod_outc_3_expo_3_rev_2.png"), 900, 460)
p<- gratia::draw(mod,
                 select = sm,  
                 ci_col = ci_col) &  geom_hline(yintercept=0, linetype="dashed", color = "grey", size = 1.5) & 
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) & 
  coord_cartesian(ylim = c(-1, 1)) & 
  scale_x_continuous(expand = c(0, 0)) & 
  ggtitle(name_plot) & 
  xlab(x_name) & 
  ylab(y_name) & 
  theme_bw() &
  mynamestheme
p
dev.off()
