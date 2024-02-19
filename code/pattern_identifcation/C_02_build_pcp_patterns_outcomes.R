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
mon <- 192
na_level <- 75
scale <- "TRUE"
case <- paste0(mon/12, "_yrs_na_", na_level)
# data prepared at A_01_pcp_outcomes_rrmc_grid_search
outc_prep <- readRDS(paste0(generated.data.folder, "data_with_ids_outc_",mon/12, "_yrs_na_", na_level ,"_scale", "_", scale, ".rds")) # GC: instead of 'mon/12, "_yrs_na_", na_level' may use the 'case' object that was defined before.
data <- readRDS(paste0(generated.data.folder, "data_outc_",mon/12, "_yrs_na_", na_level ,"_scale", "_", scale,".rds")) # GC: similar comment as in line 18.
# outcome_description <- readRDS(paste0(generated.data.folder, "desc_outc_",mon/12, "_yrs_na_", na_level ,".rds")) # GC: similar comment as in line 18.
outcome_description <- readRDS(paste0(generated.data.folder, "outcome_description.rds"))

# read pcp results 
pcp_outs <- readRDS(paste0(generated.data.folder, "pcp_rrmc_outcome_", mon/12, "_yrs_na_", na_level,  "scale", scale, "_rev_scs.rds")) # pcp_outs # GC: similar comment as in line 18.
# give name to pcp run for plotting results
pcp_run <- paste0("rrmc_outcome_", mon/12, "_yrs_na_", na_level, "_scale_", scale, "_rev_scs") # GC: similar comment as in line 18.
 
# organize name and data categories
cn <- colnames(pcp_outs$S)
outcome_description <- outcome_description[which(outcome_description$variable_name %in% cn),]
outcome_description <- outcome_description[which(outcome_description$month == mon),]
cng <- outcome_description[,c("outcome", "family", "variable_name")]
cng <- cng[which(cng$variable_name %in% cn),]
colnames(pcp_outs$L) <- colnames(pcp_outs$S)

# re-order and rename variables
order <- c("ADHDCTOT", "ADHDCIA", "ADHDCHI", "ADHDWTOT", "ADHDWIA", "ADHDWHI",
           "total_self_control_prob", 
           "Externalizing_Problems_Total", "Internalizing_Problems_Total", "Thought_Problems_Total", "Attention_Problems_Total", 
           "risky_drug_use",
           "other_drugs", "Joints", "Cigarettes", "Drinks",
           "KS37", "KS38", "KS39", "KS19", "KS11", "KS12" 
)
# low-rank matrix
l_mat <- pcp_outs$L
l_mat <- l_mat[,match(order, colnames(l_mat))]
# sparse data matrix
s_mat <- pcp_outs$S
s_mat <- s_mat[,match(order, colnames(s_mat))]
# raw input data matrix
m_mat <- data$M
m_mat <- m_mat[,match(order, colnames(m_mat))]

new_column_names <- c("ADHDCTOT", "ADHDCIA", "ADHDCHI", "ADHDWTOT", "ADHDWIA", "ADHDWHI",  # GC: Are these the new variables' names? It seems that some variables were renamed and have clearer names now, but the ADHD variables remain the same (and not very clear).
                      "Total_self_control_prob", 
                      "Externalizing_Problems_Total", "Internalizing_Problems_Total", "Thought_Problems_Total", "Attention_Problems_Total", 
                      "Risky_drug_use",
                      "Other_drugs", "Joints", "Cigarettes", "Drinks",
                      "KS_Tobacco", "KS_Alcohol", "KS_Substance", "KS_Psychosis", "KS_Inattentive", "KS_hyperact_impulsive")
colnames(l_mat) <- new_column_names
colnames(m_mat) <- new_column_names
colnames(s_mat) <- new_column_names



colgroups_l <- data.frame(column_names = colnames(pcp_outs$L), 
                        family = outcome_description[match(colnames(pcp_outs$L), outcome_description$variable_name), "family"])
colgroups_m <- data.frame(column_names = colnames(data$M), 
                          family = outcome_description[match(colnames(data$M), outcome_description$variable_name), "family"])

factors <- 1:L.rank # GC: similar comment as in C_01 script: I am not sure whether L.rank was defined earlier.
# run factor analysis
# Orthogonal Model (want factors as independent from one another as possible, get uncorrelated results):
n <- nrow(pcp_outs$L)
orthos <- factors %>% purrr::map(~fa(pcp_outs$L, nfactors = ., n.obs = n, rotate = "varimax", scores = "regression"))
orthos %>% walk(print, digits = 2, sort = T)
ortho_ebics <- orthos %>% map_dbl(~.$EBIC)
best_fit <- which.min(ortho_ebics)

# Next we can look at the fit indices of our orthogonal models (lower EBIC = better fit). 
fa_model <- orthos[[best_fit]]
## Table S8 Loadings for behavior profiles
print(fa_model, digits = 2)

# run oblique model to assess if it improves the performance compared to orthogonal model
obliq <- factors %>% purrr::map(~fa(pcp_outs$L, nfactors = ., n.obs = n, rotate = "oblimin", scores = "regression"))
obliq %>% walk(print, digits = 2, sort = T)
obliq_ebics <- obliq %>% map_dbl(~.$EBIC)
best_fit <- which.min(ortho_ebics) # oblique model ebic is same to orthogonal
loadings <- as_tibble(cbind(rownames(fa_model$loadings[]), fa_model$loadings[]))
colnames(loadings)[1] <- "Variable"
loadings <- loadings %>% mutate_at(colnames(loadings)[str_starts(colnames(loadings), "MR")], as.numeric)
loadings$Max <- colnames(loadings[, -1])[max.col(loadings[, -1], ties.method = "first")] # should be 2:5
scores <- as.tibble(cbind(rownames(fa_model$scores[]), fa_model$scores[])) %>% mutate_all(as.numeric)
scores$Max <- colnames(scores)[max.col(scores, ties.method = "first")]
fa_pats <- loadings %>% 
  dplyr::select(-Max, -Variable) %>% 
  mutate_all(as.numeric)
fa_pats <- fa_pats %>% dplyr::select(sort(colnames(.))) %>% as.matrix()

# combine description and loadings
dat <- cbind(colgroups_l, fa_pats)

# re-order and rename input variables for plotting Figure 4
order <- c("ADHDCTOT", "ADHDCIA", "ADHDCHI", "ADHDWTOT", "ADHDWIA", "ADHDWHI", # GC: the 'order' vector was already defined in lines 37-43.
           "total_self_control_prob", 
           "Externalizing_Problems_Total", "Internalizing_Problems_Total", "Thought_Problems_Total", "Attention_Problems_Total", 
           "risky_drug_use",
           "other_drugs", "Joints", "Cigarettes", "Drinks",
           "KS37", "KS38", "KS39", "KS19", "KS11", "KS12" 
)
dat <- dat[match(order, dat$column_names),]

new_column_names <- c("ADHDCTOT", "ADHDCIA", "ADHDCHI", "ADHDWTOT", "ADHDWIA", "ADHDWHI", # GC: the 'new_column_names' vector was already defined in lines 54-59.
                               "Total_self_control_prob", 
                               "Externalizing_Problems_Total", "Internalizing_Problems_Total", "Thought_Problems_Total", "Attention_Problems_Total", 
                               "Risky_drug_use",
                               "Other_drugs", "Joints", "Cigarettes", "Drinks",
                               "KS_Tobacco", "KS_Alcohol", "KS_Substance", "KS_Psychosis", "KS_Inattentive", "KS_hyperact_impulsive")
dat$column_names <- new_column_names

## Figure 4 - Factor loadings for patterns of behaviors
for(p in 1:3){
png(paste0(output.folder, pcp_run, "_l_fa_", p, "patterns_rev_scs_renames.png"), 1250, 460)
print_patterns_loc(dat[,c("MR1", "MR2", "MR3")], colgroups = dat[,c("column_names", "family")], pat_type = "factor", n = p, title = "FA factors", 
                   size_line = 1.5, size_point = 3, ylim_min = -1, ylim_max = 1.01)
dev.off()
}

# for Table S8.  Loadings for behavior profiles
dat_round <- dat
dat_round[,c("MR1", "MR2", "MR3")] <- round(dat_round[,c("MR1", "MR2", "MR3")],2)

png(paste0(output.folder, pcp_run, "_loadings_table.png"), 900, 700)
grid.table(dat_round)
dev.off()

## save scores
# assign SID
scores$SID <- outc_prep$SID
scores <- scores[,c("SID", "MR1", "MR2", "MR3")]
# save
saveRDS(scores, paste0(generated.data.folder, "outcome_pcp_fa_profiles_scores_", case, "_n_", nrow(scores), "_rev_scs.rds"))
