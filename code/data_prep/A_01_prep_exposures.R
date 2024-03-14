# First step to load packages etc.
rm(list=ls())
library(rvest)
library(qdapRegex)
library(stringi)
library(rexposome)
# for PCP
library(PCPhelpers)
library(pcpr)
library(foreach)
library(tictoc) # for timing
library(plotly)    # for visualizing the gridsearches
library(progressr) # needed for progress bars with the new PCP gridsearch
library(tidyverse)

# 1a Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'0_01_init_directory_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

# load exposure data
df_mh <- readRDS(paste0(generated.data.folder, "material_hardship.rds"))
df_els <- readRDS(paste0(generated.data.folder, "mother_perceived_stress.rds"))
df_nq <- readRDS(paste0(generated.data.folder, "neighborhood_quality.rds"))
df_ipv <- readRDS(paste0(generated.data.folder, "intimate_partner_violence.rds"))
df_ss <- readRDS(paste0(generated.data.folder, "social_support.rds"))
df_dem <- readRDS(paste0(generated.data.folder, "demoralization.rds"))
df_disc <- readRDS(paste0(generated.data.folder, "discrimination.rds"))
df_smk <- readRDS(paste0(generated.data.folder, "smoke_cot_derivative_self_report.rds"))
df_bpa <- readRDS(paste0(generated.data.folder, "bisphenols_specific_gravity_corr.rds"))
df_bpa <- df_bpa[,-which(colnames(df_bpa) %in% c("M1SG","specific_gravity_corr"))]
df_phthal <- readRDS(paste0(generated.data.folder, "phthalates_specific_gravity_corr.rds")) # todo: rerun profiles with corrected # GC: Please don't forget this todo :)
df_phthal <- df_phthal[,-which(colnames(df_phthal) %in% c("M1SG","specific_gravity_corr"))]
df_phthal_lod <- readRDS(paste0(generated.data.folder, "phthalates_below_lod.rds"))
df_smk <- df_smk[,c("SID", "month", "E10", "E01", "smk_self")]
colnames(df_smk)[3] <- "SHS"
df_pah <- readRDS(paste0(generated.data.folder, "pah_total.rds"))
df_pah <- df_pah[,c("SID", "month", "totalpah")]
df_pbde <- readRDS(paste0(generated.data.folder, "pbde_imp.rds"))
df_pah_adducts <- readRDS(paste0(generated.data.folder, "pah_adducts.rds"))
df_pah_adducts <- df_pah_adducts[,-which(colnames(df_pah_adducts) == "madducts_below_lod")]
exposure_description <- readRDS(paste0(generated.data.folder, "exposure_description.rds"))
# create a dataframe that includes all the combinations of SID and month contained in the above datasets
sids <- unique(c(df_mh$SID, df_els$SID, df_nq$SID, df_ipv$SID, df_ss$SID, df_dem$SID, df_disc$SID, df_smk$SID,df_pah$SID, df_bpa$SID, df_phthal$SID, df_pbde$SID))
sids <- sids[order(sids)]
months <- unique(c(df_mh$month, df_els$month, df_nq$month, df_ipv$month, df_ss$month, df_dem$month, df_disc$month, df_smk$month,df_pah$month, df_bpa$month, df_phthal$month, df_pbde$month))
months <- months[order(months)]
exposures <- data.frame()
# create a dataframe that contains subject id and month for temporal reference
for(i in 1:length(sids)){
  df_loc <- data.frame(SID = sids[i], month = months)
  exposures <- rbind(exposures, df_loc)
}
# add exposures to the previous dataframe
exposures <- exposures %>%
  dplyr::left_join(df_mh, by = c("SID", "month")) %>%
  dplyr::left_join(df_els, by = c("SID", "month"))  %>%
  dplyr::left_join(df_nq, by = c("SID", "month")) %>%
  dplyr::left_join(df_ipv, by = c("SID", "month"))  %>%
  dplyr::left_join(df_ss, by = c("SID", "month")) %>%
  dplyr::left_join(df_dem, by = c("SID", "month"))  %>%
  dplyr::left_join(df_disc, by = c("SID", "month"))  %>%
  dplyr::left_join(df_smk, by = c("SID", "month")) %>%
  dplyr::left_join(df_pah, by = c("SID", "month")) %>%
  dplyr::left_join(df_bpa, by = c("SID", "month")) %>%
  dplyr::left_join(df_phthal, by = c("SID", "month")) %>%
  dplyr::left_join(df_pbde, by = c("SID", "month")) %>%
  dplyr::left_join(df_pah_adducts, by = c("SID", "month"))
# save exposures
saveRDS(exposures, paste0(generated.data.folder,"exposures.rds"))
