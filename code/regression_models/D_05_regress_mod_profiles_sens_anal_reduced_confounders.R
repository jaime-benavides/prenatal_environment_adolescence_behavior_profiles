rm(list=ls())
# 1a Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'0_01_init_directory_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

data_path <- "/home/jbenavides/maklab/scratch/data/health/amy_r01_aim1/raw_data/"
ccceh_data_path <- paste0(data_path, "CCCEH_Data/")

# read covariates 
covariates <- readRDS(paste0(generated.data.folder, "covariates.rds"))

# check age range of initial subset of cohort with administered interviews and surveys
sids_visit_16 <- readRDS(paste0(generated.data.folder, "sids_any_neurobehavioral_data.rds"))

summary(covariates[which(covariates$SID %in% sids_visit_16), "age"])
## read profiles
# exposure
case_expo <- "na_50_reduced_rev_grav_corr_rev_shs_rev_valid_part"
# case na_50_reduced_rev exposure_profiles <- readRDS(paste0(generated.data.folder, "exposure_pcp_fa_profiles_scores_", case_expo, "_n_433.rds"))
exposure_profiles <- readRDS(paste0(generated.data.folder, "exposure_pcp_fa_profiles_scores_na_50_reduced_rev_grav_corr_rev_shs_rev_valid_part_n_438.rds"))
# outcome
case_outc <- "16_yrs_na_75"
outcome_profiles <- readRDS(paste0(generated.data.folder, "outcome_pcp_fa_profiles_scores_", case_outc, "_n_322_rev_scs.rds"))
sid_match <- outcome_profiles$SID[which(outcome_profiles$SID %in% exposure_profiles$SID)]

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


mice::densityplot(df_imput)

mice::stripplot(df_imput, pch = c(21, 20), cex = c(1, 1.5))


mod_outc_1_expo_1 <- lm(outcome_prof_1 ~ exposure_prof_1 + GENDER + age + mat_ed_lvl + HOMETOT + TSC_H + ethnicity + T3QT,
                        data = data, 
                        na.action = na.omit)

mod_outc_1_expo_1_imp <- with(df_imput, lm(outcome_prof_1 ~ exposure_prof_1 + GENDER + age + mat_ed_lvl + HOMETOT + TSC_H + ethnicity + T3QT))


mod_outc_1_expo_2 <- lm(outcome_prof_1 ~ exposure_prof_2  + GENDER + age + mat_ed_lvl + HOMETOT + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_1_expo_2_imp <- with(df_imput, lm(outcome_prof_1 ~ exposure_prof_2  + GENDER + age + mat_ed_lvl + HOMETOT + TSC_H + ethnicity + T3QT))


mod_outc_1_expo_3 <- lm(outcome_prof_1 ~ exposure_prof_3  + GENDER + age + mat_ed_lvl + HOMETOT + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_1_expo_3_imp <- with(df_imput, lm(outcome_prof_1 ~ exposure_prof_3 + GENDER + age + mat_ed_lvl + HOMETOT + TSC_H + ethnicity + T3QT))


mod_outc_2_expo_1 <- lm(outcome_prof_2 ~ exposure_prof_1 ++ GENDER + age + mat_ed_lvl + HOMETOT + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_2_expo_1_imp <- with(df_imput, lm(outcome_prof_2 ~ exposure_prof_1  + GENDER + age + mat_ed_lvl + HOMETOT + TSC_H + ethnicity + T3QT))


mod_outc_2_expo_2 <- lm(outcome_prof_2 ~ exposure_prof_2 + GENDER + age + mat_ed_lvl + HOMETOT + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_2_expo_2_imp <- with(df_imput, lm(outcome_prof_2 ~ exposure_prof_2 + GENDER + age + mat_ed_lvl + HOMETOT + TSC_H + ethnicity + T3QT))


mod_outc_2_expo_3 <- lm(outcome_prof_2 ~ exposure_prof_3 + GENDER + age + mat_ed_lvl + HOMETOT + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_2_expo_3_imp <- with(df_imput, lm(outcome_prof_2 ~ exposure_prof_3 + GENDER + age + mat_ed_lvl + HOMETOT + TSC_H + ethnicity + T3QT))


mod_outc_3_expo_1 <- lm(outcome_prof_3 ~ exposure_prof_1 + GENDER + age + mat_ed_lvl + HOMETOT + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_3_expo_1_imp <- with(df_imput, lm(outcome_prof_3 ~ exposure_prof_1  + GENDER + age + mat_ed_lvl + HOMETOT + TSC_H + ethnicity + T3QT))


mod_outc_3_expo_2 <- lm(outcome_prof_3 ~ exposure_prof_2  + GENDER + age + mat_ed_lvl + HOMETOT + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_3_expo_2_imp <- with(df_imput, lm(outcome_prof_3 ~ exposure_prof_2  + GENDER + age + mat_ed_lvl + HOMETOT + TSC_H + ethnicity + T3QT))


mod_outc_3_expo_3 <- lm(outcome_prof_3 ~ exposure_prof_3  + GENDER + age + mat_ed_lvl + HOMETOT + TSC_H + ethnicity + T3QT, 
                        data = data, 
                        na.action = na.omit)

mod_outc_3_expo_3_imp <- with(df_imput, lm(outcome_prof_3 ~ exposure_prof_3 + GENDER + age + mat_ed_lvl + HOMETOT + TSC_H + ethnicity + T3QT))

png(paste0(output.folder, "models_result", "_sens_anal_red_counf_imputed_outc_1_expo_1.png"), 900, 460)
compare_model_imput(mod_outc_1_expo_1, mod_outc_1_expo_1_imp)
dev.off()
png(paste0(output.folder, "models_result", "_sens_anal_red_counf_imputed_outc_1_expo_2.png"), 900, 460)
compare_model_imput(mod_outc_1_expo_2, mod_outc_1_expo_2_imp)
dev.off()
png(paste0(output.folder, "models_result", "_sens_anal_red_counf_imputed_outc_1_expo_3.png"), 900, 460)
compare_model_imput(mod_outc_1_expo_3, mod_outc_1_expo_3_imp)
dev.off()
png(paste0(output.folder, "models_result", "_sens_anal_red_counf_imputed_outc_2_expo_1.png"), 900, 460)
compare_model_imput(mod_outc_2_expo_1, mod_outc_2_expo_1_imp)
dev.off()
png(paste0(output.folder, "models_result", "_sens_anal_red_counf_imputed_outc_2_expo_2.png"), 900, 460)
compare_model_imput(mod_outc_2_expo_2, mod_outc_2_expo_2_imp)
dev.off()
png(paste0(output.folder, "models_result", "_sens_anal_red_counf_imputed_outc_2_expo_3.png"), 900, 460)
compare_model_imput(mod_outc_2_expo_3, mod_outc_2_expo_3_imp)
dev.off()
png(paste0(output.folder, "models_result", "_sens_anal_red_counf_imputed_outc_3_expo_1.png"), 900, 460)
compare_model_imput(mod_outc_3_expo_1, mod_outc_3_expo_1_imp)
dev.off()
png(paste0(output.folder, "models_result", "_sens_anal_red_counf_imputed_outc_3_expo_2.png"), 900, 460)
compare_model_imput(mod_outc_3_expo_2, mod_outc_3_expo_2_imp)
dev.off()
png(paste0(output.folder, "models_result", "_sens_anal_red_counf_imputed_outc_3_expo_3.png"), 900, 460)
compare_model_imput(mod_outc_3_expo_3, mod_outc_3_expo_3_imp)
dev.off()

png(paste0(output.folder, "data_set_profiles_covariates_missinges_sens_anal_red_counf.png"), 900, 460)
ggmice::plot_pattern(data, 
                     square = FALSE, 
                     rotate = TRUE)
dev.off()




mods <- c("mod_outc_1_expo_1", "mod_outc_1_expo_2", "mod_outc_1_expo_3", 
          "mod_outc_2_expo_1", "mod_outc_2_expo_2", "mod_outc_2_expo_3",
          "mod_outc_3_expo_1", "mod_outc_3_expo_2", "mod_outc_3_expo_3")
Beta.fit <- numeric()
Beta.se <- numeric()
Beta.lci <- numeric()
Beta.uci <- numeric()
for(mds in 1:length(mods)){
  m <- get(mods[mds])
  Beta.fit[mds] <- summary(m)$coefficients[2,1]
  Beta.se[mds]  <- summary(m)$coefficients[2,2]
  Beta.lci[mds] <- Beta.fit[mds] - 1.96 * Beta.se[mds]
  Beta.uci[mds] <- Beta.fit[mds] + 1.96 * Beta.se[mds]
}
# Figure S6
model_res <- data.frame(model_name = mods, Beta.fit = Beta.fit, Beta.se = Beta.se, Beta.lci = Beta.lci, Beta.uci = Beta.uci)
model_res[,2:5] <- round(model_res[,2:5], 2)
pdf(paste0(output.folder, "model_summary_table_sens_anal_red_counf.pdf"), height=4, width=6)
grid.table(model_res, rows = NULL)
dev.off()
model_res$exposure_profile <- rep(c("exposure_profile_1", "exposure_profile_2", "exposure_profile_3"),3)
# plot all model results together
png(paste0(output.folder, "models_result", "_sens_anal_red_counf.png"), 900, 460)
model_res %>%
dplyr::mutate(estimate = Beta.fit, std.error = Beta.se) %>%
  dplyr::mutate(model_name = fct_relevel(model_name, 
                                   rev(c("mod_outc_1_expo_1", "mod_outc_1_expo_2", "mod_outc_1_expo_3",
                                     "mod_outc_2_expo_1", "mod_outc_2_expo_2", "mod_outc_2_expo_3",
                                     "mod_outc_3_expo_1", "mod_outc_3_expo_2", "mod_outc_3_expo_3")))) %>% 
  ggplot(aes(x = model_name, y = estimate, color = exposure_profile, shape=exposure_profile,
             ymin = estimate - 1.96*std.error,
             ymax = estimate + 1.96*std.error)) +
  geom_pointrange(size = 1.25) + theme_bw() +
  geom_linerange(size = 1.25) + 
  scale_shape_manual(values=c(1, 2, 5))+
  scale_color_manual(values=c('#bf43bd','#4398bf', '#251963'))+
  ylim(-0.3,0.42) + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  theme(legend.text = element_text(size=12), legend.title = element_text(size=14),
        axis.text.y = element_text(hjust = 1, size = 14),
              axis.text.x = element_text(size = 14), strip.background = element_rect(fill = "white"), 
              axis.title.x = element_blank(), axis.title.y = element_blank()) + coord_flip() +
  labs(y = "Estimate", x = "Model")
dev.off()


