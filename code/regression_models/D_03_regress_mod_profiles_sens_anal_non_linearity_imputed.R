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
case_expo <- "na_50_reduced_rev_grav_corr_rev_shs_rev_valid_part" # GC: seems to be a redundant line.
exposure_profiles <- readRDS(paste0(generated.data.folder, "exposure_pcp_fa_profiles_scores_na_50_reduced_rev_grav_corr_rev_shs_rev_valid_part_n_438.rds"))
# outcome
case_outc <- "16_yrs_na_75"
outcome_profiles <- readRDS(paste0(generated.data.folder, "outcome_pcp_fa_profiles_scores_", case_outc, "_n_322_rev_scs.rds"))

sid_match <- outcome_profiles$SID[which(outcome_profiles$SID  %in% exposure_profiles$SID)]

outcome_profiles_match <- outcome_profiles[which(outcome_profiles$SID %in% sid_match),]
exposure_profiles_match <- exposure_profiles[which(exposure_profiles$SID %in% sid_match),]

colnames(exposure_profiles_match) <- c("SID", "exposure_prof_2", "exposure_prof_3", "exposure_prof_1")
exposure_profiles_match <- exposure_profiles_match[,c("SID", "exposure_prof_1", "exposure_prof_2", "exposure_prof_3")]
colnames(outcome_profiles_match) <- c("SID", "outcome_prof_3", "outcome_prof_1", "outcome_prof_2") # GC: numbers in variables' names suffix are not in order (3-1-2) while in the next line there are (I'm not sure it has any meaning, but putting it here just in case...).
outcome_profiles_match <- outcome_profiles_match[,c("SID", "outcome_prof_1", "outcome_prof_2", "outcome_prof_3")]
profiles <- dplyr::left_join(exposure_profiles_match, outcome_profiles_match, by = "SID")
data <- dplyr::left_join(profiles, covariates, by = "SID")
# standardize
# leave out gender, which is categorical
my.min <- unlist(lapply(data,function(x) min(x,na.rm=T))) 
my.max <- unlist(lapply(data,function(x) max(x,na.rm=T))) 
non_scale <- which(my.max - my.min == 1)
data[,-c(which(colnames(data) %in% c("SID")),non_scale)] = apply(data[,-c(which(colnames(data) %in% c("SID")),non_scale)], 2, function(a) a/sd(a, na.rm = T))

# creating imputation set 
data_imput <- data[,-1]
predMat <- mice::make.predictorMatrix(data_imput)

outcome_vars <- colnames(profiles)[-1]
predMat[,outcome_vars] <- 0

## CREATING A METHODS VECTOR FOR IMPUTATIOM
impmethod <- mice::make.method(data_imput)

df_imput <- mice::mice(data_imput,
                       m = 7, #num_imput
                       maxit = 10, # play around after looking at imputed data sets 
                       # where = , # this can be used to skip imputations for selected missing values! not good bc then we dont get imputations for missing values for the same participant
                       meth = impmethod,
                       predMat, 
                       seed = 12) 

plot(df_imput)

mice::densityplot(df_imput)

mice::stripplot(df_imput, pch = c(21, 20), cex = c(1, 1.5))

mod_outc_1_expo_1 <- gam(outcome_prof_1 ~ s(exposure_prof_1) + GENDER + age + mat_ed_lvl + WSC_DS + WSC_COD + WASI_PRI_C + WASI_VCI_C + HOMETOT + B11 + TSC_H + ethnicity + T3QT, 
          data = data, 
          na.action = na.omit)

mod_outc_1_expo_2 <- gam(outcome_prof_1 ~ s(exposure_prof_2) + GENDER + age + mat_ed_lvl + WSC_DS + WSC_COD + WASI_PRI_C + WASI_VCI_C + HOMETOT + B11 + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_1_expo_3 <- gam(outcome_prof_1 ~ s(exposure_prof_3) + GENDER + age + mat_ed_lvl + WSC_DS + WSC_COD + WASI_PRI_C + WASI_VCI_C + HOMETOT + B11 + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_2_expo_1 <- gam(outcome_prof_2 ~ s(exposure_prof_1) + GENDER + age + mat_ed_lvl + WSC_DS + WSC_COD + WASI_PRI_C + WASI_VCI_C + HOMETOT + B11 + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_2_expo_2 <- gam(outcome_prof_2 ~ s(exposure_prof_2) + GENDER + age + mat_ed_lvl + WSC_DS + WSC_COD + WASI_PRI_C + WASI_VCI_C + HOMETOT + B11 + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_2_expo_3 <- gam(outcome_prof_2 ~ s(exposure_prof_3) + GENDER + age + mat_ed_lvl + WSC_DS + WSC_COD + WASI_PRI_C + WASI_VCI_C + HOMETOT + B11 + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_3_expo_1 <- gam(outcome_prof_3 ~ s(exposure_prof_1) + GENDER + age + mat_ed_lvl + WSC_DS + WSC_COD + WASI_PRI_C + WASI_VCI_C + HOMETOT + B11 + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_3_expo_1_imp <- with(df_imput, gam(outcome_prof_3 ~ s(exposure_prof_1) + GENDER + age + mat_ed_lvl + WSC_DS + WSC_COD + WASI_PRI_C + WASI_VCI_C + HOMETOT + B11 + TSC_H + ethnicity + T3QT))


mod_outc_3_expo_2 <- gam(outcome_prof_3 ~ s(exposure_prof_2) + GENDER + age + mat_ed_lvl + WSC_DS + WSC_COD + WASI_PRI_C + WASI_VCI_C + HOMETOT + B11 + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_3_expo_2_imp <- with(df_imput, gam(outcome_prof_3 ~ s(exposure_prof_2) + GENDER + age + mat_ed_lvl + WSC_DS + WSC_COD + WASI_PRI_C + WASI_VCI_C + HOMETOT + B11 + TSC_H + ethnicity + T3QT))



mod_outc_3_expo_3 <- gam(outcome_prof_3 ~ s(exposure_prof_3) + GENDER + age + mat_ed_lvl + WSC_DS + WSC_COD + WASI_PRI_C + WASI_VCI_C + HOMETOT + B11 + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mynamestheme <- theme(
  plot.title = element_text(family = "Helvetica", hjust = 0.5, size = (22)),
  axis.title = element_text(family = "Helvetica", size = (20)),
  axis.text = element_text(family = "Helvetica", size = (20))
)


sm <- "s(exposure_prof_1)"
x_name <- "Exposure P1: PAH_SHS_Stress"
y_name <- "Behavior P3"


name_plot <- ""
mod <- mod_outc_3_expo_1_imp

mod_1 <- gratia::smooth_estimates(mod$analyses[[1]])
mod_2 <- gratia::smooth_estimates(mod$analyses[[2]])
mod_3 <- gratia::smooth_estimates(mod$analyses[[3]])
mod_4 <- gratia::smooth_estimates(mod$analyses[[4]])
mod_5 <- gratia::smooth_estimates(mod$analyses[[5]])
mod_6 <- gratia::smooth_estimates(mod$analyses[[6]])
mod_7 <- gratia::smooth_estimates(mod$analyses[[7]])


png(paste0(output.folder, "nonline_sens_anal_mod_outc_3_expo_1_all_imputed_new.png"), 900, 460)
gratia::draw(mod_outc_3_expo_1) &  
  geom_line(data = mod_1, aes(x = exposure_prof_1, y = est, col = "red")) &
  geom_line(data = mod_2, aes(x = exposure_prof_1, y = est, col = "red")) &
  geom_line(data = mod_3, aes(x = exposure_prof_1, y = est, col = "red")) &
  geom_line(data = mod_4, aes(x = exposure_prof_1, y = est, col = "red")) &
  geom_line(data = mod_5, aes(x = exposure_prof_1, y = est, col = "red")) &
  geom_line(data = mod_6, aes(x = exposure_prof_1, y = est, col = "red")) &
  geom_line(data = mod_7, aes(x = exposure_prof_1, y = est, col = "red")) &
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey") & 
  scale_y_continuous(expand = c(0, 0)) & 
  scale_x_continuous(expand = c(0, 0)) & 
  ggtitle(name_plot) & 
  theme_bw() &
  xlab(x_name) & 
  ylab(y_name) & 
  ylim(-1,1) &
  mynamestheme 
dev.off()


sm <- "s(exposure_prof_2)"
x_name <- "Exp P2: BPs_Phthalates"
y_name <- "P3: All-Self survey"


name_plot <- ""
mod <- mod_outc_3_expo_2_imp

mod_1 <- gratia::smooth_estimates(mod$analyses[[1]])
mod_2 <- gratia::smooth_estimates(mod$analyses[[2]])
mod_3 <- gratia::smooth_estimates(mod$analyses[[3]])
mod_4 <- gratia::smooth_estimates(mod$analyses[[4]])
mod_5 <- gratia::smooth_estimates(mod$analyses[[5]])
mod_6 <- gratia::smooth_estimates(mod$analyses[[6]])
mod_7 <- gratia::smooth_estimates(mod$analyses[[7]])

# Figure S3
png(paste0(output.folder, "nonline_sens_anal_mod_outc_3_expo_2_all_imputed_rev_valid_part_rev_scs_more_conf.png"), 900, 460)
gratia::draw(mod_outc_3_expo_2) &  
  geom_line(data = mod_1, aes(x = exposure_prof_2, y = est, col = "red")) &
  geom_line(data = mod_2, aes(x = exposure_prof_2, y = est, col = "red")) &
  geom_line(data = mod_3, aes(x = exposure_prof_2, y = est, col = "red")) &
  geom_line(data = mod_4, aes(x = exposure_prof_2, y = est, col = "red")) &
  geom_line(data = mod_5, aes(x = exposure_prof_2, y = est, col = "red")) &
  geom_line(data = mod_6, aes(x = exposure_prof_2, y = est, col = "red")) &
  geom_line(data = mod_7, aes(x = exposure_prof_2, y = est, col = "red")) &
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey") & 
  scale_y_continuous(expand = c(0, 0)) & 
  scale_x_continuous(expand = c(0, 0)) & 
  ggtitle(name_plot) & 
  theme_bw() &
  xlab(x_name) & 
  ylab(y_name) & 
  ylim(-1,1) &
  mynamestheme 
dev.off()


