rm(list=ls())
# 1a Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'0_01_init_directory_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

library(ggfortify)
library(ggrepel)
library(kableExtra)
library(NMF)

# read raw data matrix
mon <- 0 # prenatal index to read exposures
na_level <- 50 # missing data inclusion criteria
scale <- "TRUE" # data scaled
case <- "na_50_reduced_rev_grav_corr_rev_shs_rev_valid_part" # name file
# read exposure data
expo_prep <- readRDS(paste0(generated.data.folder, "data_row_comp_", case, ".rds"))
# build matrix data 
data <- list("M" = expo_prep[,-c(which(colnames(expo_prep) %in% c("SID", "month")))]) %>% purrr::map(as.matrix)
# read data description
exposure_description <- readRDS(paste0(generated.data.folder, "exposure_description.rds"))

# read pcp results
pcp_outs <- readRDS(paste0(generated.data.folder, "pren_exposures_pcp_rrmc_na_", na_level, "_scale_TRUE_grav_rev_SHS_rev_valid_part.rds")) # pcp_outs
# give name to pcp run for plotting results
pcp_run <- paste0("rrmc_exposure_", mon/12, "_yrs_na_", na_level, "_scale_", scale, "rev_grav_corr_rev_SHS_rev_valid_part")

# variables need to be re-ordered for generating plots consistently with input data categories (first social stress, then air pollutants and dna adducts and then endocrine disruptors)
cn_orig <- colnames(pcp_outs$S)
# re-order columns
pcp_outs$S <- pcp_outs$S[,c("demoralization", "material_hardship", "SHS","totalpah", "madducts", "cadducts",
                            "MEHHP", "MECPP", "MEOHP", "MEHP", "MCPP", "MIBP", "MBP", 
                            "MBZP", "MEP", "UBPA")]
# save new order of columns
cn <- colnames(pcp_outs$S)
# get data description for exposure variables
exposure_description <- exposure_description[which(exposure_description$exposure %in% cn),]
exposure_description <- exposure_description[which(exposure_description$month == mon),]
cng <- exposure_description[,c("exposure", "family", "variable_description")]
cng <- cng[which(cng$exposure %in% cn),]


# assign variable names to low-rank matrix / we need this because the algorithm did not name them adequately
colnames(pcp_outs$L) <- cn_orig
# re-order as desired
pcp_outs$L <- pcp_outs$L[,c("demoralization", "material_hardship", "SHS","totalpah", "madducts", "cadducts",
                            "MEHHP", "MECPP", "MEOHP", "MEHP", "MCPP", "MIBP", "MBP", 
                            "MBZP", "MEP", "UBPA")]

colgroups_l <- data.frame(column_names = colnames(pcp_outs$L), 
                        family = exposure_description[match(colnames(pcp_outs$L), exposure_description$exposure), "family"])
colgroups_l[which(colgroups_l$column_names %in% c("demoralization", "material_hardship")),"family"] <- "social_stressors"
colgroups_l[which(colgroups_l$column_names %in% c("UBPA")),"family"] <- "bisphenol"
colgroups_l[which(colgroups_l$column_names %in% c("SHS")),"family"] <- "secondhand smoke"
colgroups_m <- data.frame(column_names = colnames(data$M), 
                          family = exposure_description[match(colnames(data$M), exposure_description$exposure), "family"])
colgroups_m[which(colgroups_m$column_names %in% c("demoralization", "material_hardship")),"family"] <- "social_stressors"
colgroups_m[which(colgroups_m$column_names %in% c("UBPA")),"family"] <- "bisphenol"
colgroups_m[which(colgroups_m$column_names %in% c("SHS")),"family"] <- "secondhand smoke"

factors <- 1:L.rank

# Orthogonal Model (want factors as independent from one another as possible, get uncorrelated results):
n <- nrow(pcp_outs$L)

orthos <- factors %>% purrr::map(~fa(pcp_outs$L, nfactors = ., n.obs = n, rotate = "varimax", scores = "regression"))

orthos %>% walk(print, digits = 2, sort = T)

ortho_ebics <- orthos %>% map_dbl(~.$EBIC)

best_fit <- which.min(ortho_ebics)

# oblique model 

obliq <- factors %>% purrr::map(~fa(pcp_outs$L, nfactors = ., n.obs = n, rotate = "oblimin", scores = "regression"))

obliq %>% walk(print, digits = 2, sort = T)

obliq_ebics <- obliq %>% map_dbl(~.$EBIC)

# ebic is same

best_fit <- which.min(ortho_ebics)

# Next we can look at the fit indices of our orthogonal models (lower EBIC = better fit). 

data.frame("Factors" = factors, "EBIC" = ortho_ebics) %>% kbl(caption = "Orthogonal Models: Fit Indices") %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center") %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), fixed_thead = T) %>%
  row_spec(best_fit, bold = T, color = "white", background = "#D7261E")

fa_model <- orthos[[best_fit]]
## Table S7
print(fa_model, digits = 2)

# variance explained by each factor (not included in manuscript, but I leave it here just in case it is used in revision)
png(paste0(output.folder, pcp_run, "_fa_explained.png"), 300, 400)
grid.table(round(fa_model$Vaccounted, 2))
dev.off()

# prepare loadings and scores for plotting and saving
loadings <- as_tibble(cbind(rownames(fa_model$loadings[]), fa_model$loadings[]))
colnames(loadings)[1] <- "Variable"
loadings <- loadings %>% mutate_at(colnames(loadings)[str_starts(colnames(loadings), "MR")], as.numeric)
loadings$Max <- colnames(loadings[, -1])[max.col(loadings[, -1], ties.method = "first")] # should be 2:5
scores <- as.tibble(cbind(rownames(fa_model$scores[]), fa_model$scores[])) %>% mutate_all(as.numeric)
scores$Max <- colnames(scores)[max.col(scores, ties.method = "first")]
fa_pats <- loadings %>% 
  dplyr::select(-Max, -Variable) %>% 
  mutate_all(as.numeric)

# reformat variable names for manuscript 
colgroups_l$column_names <- c("Demoralization", "Material_Hardship", "Secondhand_Smoke","Total_PAH", "Maternal_PAH_Adducts", "Cord_PAH_Adducts",
  "MEHHP", "MECPP", "MEOHP", "MEHP", "MCPP", "MiBP", "MnBP", "MBzP", "MEP", "Bisphenol_A")
colgroups_l$family <- c("Social_Stressors", "Social_Stressors", "Secondhand_Smoke","Air_Pollution", "PAH-DNA_Adducts", "PAH-DNA_Adducts",
                        "DEHP_Phthalates", "DEHP_Phthalates", "DEHP_Phthalates", "DEHP_Phthalates", "Non-DEHP_Phthalates", "Non-DEHP_Phthalates", "Non-DEHP_Phthalates", "Non-DEHP_Phthalates", "Non-DEHP_Phthalates", "Bisphenol_A")
fa_pats <- fa_pats %>% dplyr::select(sort(colnames(.))) %>% as.matrix()

## Figure 3 - Factor loadings for patterns of chemical and social stressor exposures.
for(p in 1:3){
png(paste0(output.folder, pcp_run, "_l_fa_", p, "patterns.png"), 1200, 460)
# function print_patterns_loc can be found at the functions folder
print_patterns_loc(fa_pats, colgroups = colgroups_l, pat_type = "factor", n = p, title = "FA factors",
                   size_line = 1.5, size_point = 3)
dev.off()
}

# for Table S7.  Loadings for exposure profiles
dat_round <- fa_pats
dat_round[,c("MR1", "MR2", "MR3")] <- round(dat_round[,c("MR1", "MR2", "MR3")],2)
dat_round <- as.data.frame(dat_round)
dat_round$var <- colgroups_l$column_names

png(paste0(output.folder, pcp_run, "_loadings_table.png"), 900, 700)
grid.table(dat_round)
dev.off()


## save scores
# assign SID
scores$SID <- expo_prep$SID
scores <- scores[,c("SID", "MR1", "MR2", "MR3")]
# save
saveRDS(scores, paste0(generated.data.folder, "exposure_pcp_fa_profiles_scores_", case, "_n_438.rds"))