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
na_level <- 50
scale <- "TRUE"
case <- paste0(mon/12, "_yrs_na_", na_level)
# data prepared at A_01_pcp_outcomes_rrmc_grid_search
outc_prep <- readRDS(paste0(generated.data.folder, "data_with_ids_outc_",mon/12, "_yrs_na_", na_level ,"_scale", "_", scale, ".rds")) # GC: may use 'case' instead of 'mon/12, "_yrs_na_", na_level'.
data <- readRDS(paste0(generated.data.folder, "data_outc_",mon/12, "_yrs_na_", na_level ,"_scale", "_", scale,".rds")) # GC: may use 'case' instead of 'mon/12, "_yrs_na_", na_level'.
outcome_description <- readRDS(paste0(generated.data.folder, "outcome_description.rds"))

# read pcp results (todo harmonize descriptions) # GC: please don't forget this todo here.
pcp_outs <- readRDS(paste0(generated.data.folder, "pcp_rrmc_outcome_", mon/12, "_yrs_na_", na_level,  "scale", scale, "_rev_scs.rds")) # pcp_outs # GC: may use 'case' instead of 'mon/12, "_yrs_na_", na_level'.
pcp_run <- paste0("rrmc_outcome_", mon/12, "_yrs_na_", na_level, "_scale_", scale) # GC: may use 'case' instead of 'mon/12, "_yrs_na_", na_level'.
cn <- colnames(pcp_outs$S)
outcome_description <- outcome_description[which(outcome_description$variable_name %in% cn),]
outcome_description <- outcome_description[which(outcome_description$month == mon),]
cng <- outcome_description[,c("outcome", "family", "variable_name")]
cng <- cng[which(cng$variable_name %in% cn),]
colnames(pcp_outs$L) <- colnames(pcp_outs$S)

colgroups_l <- data.frame(column_names = colnames(pcp_outs$L), 
                        family = outcome_description[match(colnames(pcp_outs$L), outcome_description$variable_name), "family"])
colgroups_m <- data.frame(column_names = colnames(data$M), 
                          family = outcome_description[match(colnames(data$M), outcome_description$variable_name), "family"])

factors <- 1:L.rank # GC: similar to previous scripts, just checking whether L.rank was defined earlier.

# run factor analysis
# Orthogonal Model (want factors as independent from one another as possible, get uncorrelated results):
n <- nrow(pcp_outs$L)
orthos <- factors %>% purrr::map(~fa(pcp_outs$L, nfactors = ., n.obs = n, rotate = "varimax", scores = "regression"))
orthos %>% walk(print, digits = 2, sort = T)
ortho_ebics <- orthos %>% map_dbl(~.$EBIC)
best_fit <- which.min(ortho_ebics)
fa_model <- orthos[[best_fit]]
print(fa_model, digits = 2)
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
order <- c("ADHDCTOT", "ADHDCIA", "ADHDCHI", "ADHDWTOT", "ADHDWIA", "ADHDWHI",
           "total_self_control_prob", 
           "Externalizing_Problems_Total", "Internalizing_Problems_Total", "Thought_Problems_Total", "Attention_Problems_Total", 
           "risky_drug_use",
           "other_drugs", "Joints", "Cigarettes", "Drinks",
           "KS37", "KS38", "KS39", "KS19", "KS11", "KS12" 
)
dat <- dat[match(order, dat$column_names),]

for(p in 1:3){
png(paste0(output.folder, pcp_run, "_l_fa_", p, "patterns_rev2.png"), 1250, 460)
print_patterns_loc(dat[,c("MR1", "MR2", "MR3")], colgroups = dat[,c("column_names", "family")], pat_type = "factor", n = p, title = "FA factors", size_line = 1.5, size_point = 3,
                   ylim_min = -1, ylim_max = 1.01)
dev.off()
}

## save scores
# assign SID
scores$SID <- outc_prep$SID
scores <- scores[,c("SID", "MR1", "MR2", "MR3")]
# save
saveRDS(scores, paste0(generated.data.folder, "outcome_pcp_fa_profiles_scores_", case, "_n_", nrow(scores), ".rds"))
