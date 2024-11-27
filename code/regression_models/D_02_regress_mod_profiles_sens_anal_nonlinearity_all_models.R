rm(list=ls())
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'0_01_init_directory_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

data_path <- "/home/jbenavides/maklab/scratch/data/health/amy_r01_aim1/raw_data/"
ccceh_data_path <- paste0(data_path, "CCCEH_Data/")

# read covariates 
covariates <- readRDS(paste0(generated.data.folder, "covariates.rds"))

## read profiles
# exposure
exposure_profiles <- readRDS(paste0(generated.data.folder, "exposure_pcp_fa_profiles_scores_na_50_reduced_rev_grav_corr_rev_shs_rev_valid_part_n_438.rds"))
# outcome
case_outc <- "16_yrs_na_75" # GC: this is the only appearance of the object ‘case_outc’ in this script.
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
data[,-c(which(colnames(data) %in% c("SID")),non_scale)] = apply(data[,-c(which(colnames(data) %in% c("SID")),non_scale)], 2, function(a) a/sd(a, na.rm = T)) # GC: if you like it more concise you may use: =="SID" instead of %in% c("SID")
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

name_plot <- ""
mod <- mod_outc_1_expo_2
ci_col <- "blue"
png(paste0(output.folder, "nonline_sens_anal_med_mod_outc_1_expo_2_rev_2.png"), 900, 460)
p<- gratia::draw(mod,
                 select = sm,  
                 ci_col = ci_col) &  geom_hline(yintercept=0, linetype="dashed", color = "grey", size = 1.5) & 
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

name_plot <- ""
mod <- mod_outc_1_expo_3
ci_col <- "blue"
png(paste0(output.folder, "nonline_sens_anal_med_mod_outc_1_expo_3_rev_2.png"), 900, 460)
p<- gratia::draw(mod,
                 select = sm,  
                 ci_col = ci_col) &  geom_hline(yintercept=0, linetype="dashed", color = "grey", size = 1.5) & 
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

name_plot <- ""
mod <- mod_outc_2_expo_2
ci_col <- "blue"
png(paste0(output.folder, "nonline_sens_anal_med_mod_outc_2_expo_2_rev_2.png"), 900, 460)
p<- gratia::draw(mod,
                 select = sm,  
                 ci_col = ci_col) &  geom_hline(yintercept=0, linetype="dashed", color = "grey", size = 1.5) & 
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

name_plot <- ""
mod <- mod_outc_2_expo_3
ci_col <- "blue"
png(paste0(output.folder, "nonline_sens_anal_med_mod_outc_2_expo_3_rev_2.png"), 900, 460)
p<- gratia::draw(mod,
                 select = sm,  
                 ci_col = ci_col) &  geom_hline(yintercept=0, linetype="dashed", color = "grey", size = 1.5) & 
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

name_plot <- ""
mod <- mod_outc_3_expo_2
ci_col <- "blue"
png(paste0(output.folder, "nonline_sens_anal_med_mod_outc_3_expo_2_rev_2.png"), 900, 460)
p<- gratia::draw(mod,
                 select = sm,  
                 ci_col = ci_col) &  geom_hline(yintercept=0, linetype="dashed", color = "grey", size = 1.5) & 
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

name_plot <- ""
mod <- mod_outc_3_expo_3
ci_col <- "blue"
png(paste0(output.folder, "nonline_sens_anal_med_mod_outc_3_expo_3_rev_2.png"), 900, 460)
p<- gratia::draw(mod,
                 select = sm,  
                 ci_col = ci_col) &  geom_hline(yintercept=0, linetype="dashed", color = "grey", size = 1.5) & 
  coord_cartesian(ylim = c(-1, 1)) & 
  scale_x_continuous(expand = c(0, 0)) & 
  ggtitle(name_plot) & 
  xlab(x_name) & 
  ylab(y_name) & 
  theme_bw() &
  mynamestheme
p
dev.off()



                                                                 
## Figure S1

# Configure plots
exposures <- c("exposure_prof_1", "exposure_prof_2", "exposure_prof_3")
behaviors <- c("behav_prof_1", "behav_prof_2", "behav_prof_3")

for (expo in exposures) {
  for (behav in behaviors) {
    # general config
    sm <- paste0("s(", expo, ")")
    No.expo <- substr(expo, nchar(expo), nchar(expo))
    No.behav <- substr(behav, nchar(behav), nchar(behav))
    x_name <- paste0("Exposure Profile ", No.expo)
    y_name <- paste0("Behavior Profile ", No.behav)
    
    name_plot <- ""
    mod_name <- paste0("mod_outc_",No.behav,"_expo_",No.expo)
    mod <- get(mod_name)
    ci_col <- "blue"
    plot_file <- paste0("nonline_sens_anal_med_mod_outc_",No.behav,"_expo_",No.expo,"_rev_2.png")
    png(paste0(output.folder, plot_file), 900, 460)
    p <- gratia::draw(mod,
                      select = sm,
                      ci_col = ci_col) &
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 1.5) &
      coord_cartesian(ylim = c(-1, 1)) &
      scale_x_continuous(expand = c(0, 0)) &
      ggtitle(name_plot) &
      xlab(x_name) &
      ylab(y_name) &
      theme_bw() &
      mynamestheme
    p
    dev.off()
  }
}
