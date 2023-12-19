rm(list=ls())
.libPaths(c(.libPaths(), "/home/jbenavides/R/x86_64-pc-linux-gnu-library/4.1", 
            "/home/jbenavides/R/x86_64-pc-linux-gnu-library/4.2"))
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
outc_prep <- readRDS(paste0(generated.data.folder, "data_with_ids_outc_",mon/12, "_yrs_na_", na_level ,"_scale", "_", scale, ".rds"))
data <- readRDS(paste0(generated.data.folder, "data_outc_",mon/12, "_yrs_na_", na_level ,"_scale", "_", scale,".rds"))
# outcome_description <- readRDS(paste0(generated.data.folder, "desc_outc_",mon/12, "_yrs_na_", na_level ,".rds"))
outcome_description <- readRDS(paste0(generated.data.folder, "outcome_description.rds"))

# read pcp results (todo harmonize descriptions)
pcp_outs <- readRDS(paste0(generated.data.folder, "pcp_rrmc_outcome_", mon/12, "_yrs_na_", na_level,  "scale", scale, "_rev_scs.rds")) # pcp_outs
pcp_run <- paste0("rrmc_outcome_", mon/12, "_yrs_na_", na_level, "_scale_", scale, "_rev_scs")
cn <- colnames(pcp_outs$S)
outcome_description <- outcome_description[which(outcome_description$variable_name %in% cn),]
outcome_description <- outcome_description[which(outcome_description$month == mon),]
cng <- outcome_description[,c("outcome", "family", "variable_name")]
cng <- cng[which(cng$variable_name %in% cn),]
colnames(pcp_outs$L) <- colnames(pcp_outs$S)
L.df <- pcp_outs$L %>% 
  as.data.frame()   %>% 
  pivot_longer(cols = cn, names_to = "variable_name") %>%
  right_join(., cng, by = "variable_name")

S.df <- pcp_outs$S %>% 
  as.data.frame()   %>% 
  pivot_longer(cols = cn, names_to = "variable_name") %>%
  right_join(., cng, by = "variable_name")


L.hm <- heatmaply::heatmaply(pcp_outs$L, Colv = F, Rowv = F, labRow = NULL,
                  cexRow = 100,
                  showticklabels = c(T, F), main = "L matrix")
# saved by hand paste0(output.folder, "s_matrix_heatmap", pcp_run ,".png")
L.hm


raw.hm <- heatmaply::heatmaply(data$M, Colv = F, Rowv = F, labRow = NULL,
                             cexRow = 100,
                             showticklabels = c(TRUE, FALSE), main = "Raw exposures matrix",
)

raw.hm

order <- c("ADHDCTOT", "ADHDCIA", "ADHDCHI", "ADHDWTOT", "ADHDWIA", "ADHDWHI",
           "total_self_control_prob", 
           "Externalizing_Problems_Total", "Internalizing_Problems_Total", "Thought_Problems_Total", "Attention_Problems_Total", 
           "risky_drug_use",
           "other_drugs", "Joints", "Cigarettes", "Drinks",
           "KS37", "KS38", "KS39", "KS19", "KS11", "KS12" 
)
l_mat <- pcp_outs$L
l_mat <- l_mat[,match(order, colnames(l_mat))]

m_mat <- data$M
m_mat <- m_mat[,match(order, colnames(m_mat))]

s_mat <- pcp_outs$S
s_mat <- s_mat[,match(order, colnames(s_mat))]
# informant <- c("Child", 
#                "Child", "Child", "Child", "Child", "Child",
#                )
new_column_names <- c("ADHDCTOT", "ADHDCIA", "ADHDCHI", "ADHDWTOT", "ADHDWIA", "ADHDWHI",
                      "Total_self_control_prob", 
                      "Externalizing_Problems_Total", "Internalizing_Problems_Total", "Thought_Problems_Total", "Attention_Problems_Total", 
                      "Risky_drug_use",
                      "Other_drugs", "Joints", "Cigarettes", "Drinks",
                      "KS_Tobacco", "KS_Alcohol", "KS_Substance", "KS_Psychosis", "KS_Inattentive", "KS_hyperact_impulsive")
colnames(l_mat) <- new_column_names
colnames(m_mat) <- new_column_names
colnames(s_mat) <- new_column_names

S.hm <- heatmaply::heatmaply(s_mat, Colv = F, Rowv = F, labRow = NULL,
                             cexRow = 100,
                             showticklabels = c(T, F), main = "S matrix")

S.hm

# m matrix correlations (for presentation)
graph_title <- paste0("raw_outcome_", mon/12, "_yrs_na_", na_level, ": L Pearson correlation")
png(paste0(output.folder, pcp_run, "_raw_matrix_correlations_rev_scs_rev.png"), 900, 460)
m_mat %>% GGally::ggcorr(., method = c("pairwise.complete.obs", "pearson"),
                         label = T, label_size = 3, label_alpha = T,
                         hjust = 1, nbreaks = 10, limits = TRUE,
                         size = 4, layout.exp = 5) + ggtitle(graph_title)
dev.off()

# L matrix correlations:
graph_title <- paste0("pcp_rrmc_outcome_", mon/12, "_yrs_na_", na_level, ": L Pearson correlation")
png(paste0(output.folder, pcp_run, "_l_matrix_correlations_rev_scs_rev.png"), 900, 460)
l_mat %>% GGally::ggcorr(., method = c("pairwise.complete.obs", "pearson"),
                              label = T, label_size = 3, label_alpha = T,
                              hjust = 1, nbreaks = 10, limits = TRUE,
                              size = 4, layout.exp = 5) + ggtitle(graph_title)
dev.off()

# S matrix correlations:
graph_title <- paste0("pcp_rrmc_outcome_", mon/12, "_yrs_na_", na_level, ": S Pearson correlation")
png(paste0(output.folder, pcp_run, "_s_matrix_correlations_rev_scs.png"), 900, 460)
pcp_outs$S %>% GGally::ggcorr(., method = c("pairwise.complete.obs", "pearson"),
                                   limits = F, label = T, label_size = 3, label_alpha = T,
                                   size = 3, layout.exp = 1) + ggtitle(graph_title)
dev.off()

# png(paste0(output.folder, "raw_outcome_mat_corr_L_", mon/12, "_yrs_na_", na_level,  "_scale", scale,  "rev_scs.png"), 900, 460)
# data$M %>% GGally::ggcorr(., method = c("pairwise.complete.obs", "pearson"),
#                           limits = F, label = T, label_size = 3, label_alpha = T, hjust = 0.75,
#                           size = 3, layout.exp = 1) + ggtitle(paste0("raw_outcome_", mon/12, "_yrs_na_", na_level))
# dev.off()

png(paste0(output.folder, "outcome_mat_corr_S_", mon/12, "_yrs_na_", na_level,  "_scale", scale,  "rev_scs.png"), 900, 460)
data$M %>% GGally::ggcorr(., method = c("pairwise.complete.obs", "pearson"),
                          limits = F, label = T, label_size = 3, label_alpha = T, hjust = 0.75,
                          size = 3, layout.exp = 1) + ggtitle(paste0("raw_outcome_", mon/12, "_yrs_na_", na_level))
dev.off()


# PCA
ranktol <- 1e-04
L.rank <- Matrix::rankMatrix(pcp_outs$L, tol = ranktol)
scale_flag <- FALSE
pcs <- paste0("PC", 1:L.rank)

colgroups_l <- data.frame(column_names = colnames(pcp_outs$L), 
                        family = outcome_description[match(colnames(pcp_outs$L), outcome_description$variable_name), "family"])
colgroups_m <- data.frame(column_names = colnames(data$M), 
                          family = outcome_description[match(colnames(data$M), outcome_description$variable_name), "family"])

L.eda <-PCPhelpers::eda(pcp_outs$L, pcs = pcs, cor_lbl = T, scale_flag = scale_flag, colgroups = colgroups_l, rowgroups = NULL)

M.eda <-PCPhelpers::eda(data$M, pcs = pcs, cor_lbl = T, scale_flag = scale_flag, colgroups = colgroups_m, rowgroups = NULL)

M.eda$var  %>%
  as_image(file = paste0(output.folder, pcp_run, "_raw_pca_variance.png"))

png(paste0(output.folder, pcp_run, "_raw_pca_loads.png"), 900, 460)
M.eda$load
dev.off()

factors <- 1:L.rank

orthos_raw <- factors %>% purrr::map(~fa(data$M, nfactors = ., n.obs = n, rotate = "varimax", scores = "regression"))
#The estimated weights for the factor scores are probably incorrect.  Try a different factor score estimation method.

L.eda$var  %>%
  as_image(file = paste0(output.folder, pcp_run, "_l_pca_variance.png"))

png(paste0(output.folder, pcp_run, "_l_pca_loads.png"), 900, 460)
L.eda$load
dev.off()

png(paste0(output.folder, pcp_run, "_l_pca_biplot1.png"), 900, 460)
L.eda$biplot1
dev.off()

png(paste0(output.folder, pcp_run, "_l_pca_biplot2.png"), 900, 460)
L.eda$biplot2
dev.off()

png(paste0(output.folder, pcp_run, "_l_pca_biplot3.png"), 900, 460)
L.eda$biplot3
dev.off()
# factor analysis
# Orthogonal Model (want factors as independent from one another as possible, get uncorrelated results):
n <- nrow(pcp_outs$L)

orthos <- factors %>% purrr::map(~fa(pcp_outs$L, nfactors = ., n.obs = n, rotate = "varimax", scores = "regression"))

orthos %>% walk(print, digits = 2, sort = T)

ortho_ebics <- orthos %>% map_dbl(~.$EBIC)

best_fit <- which.min(ortho_ebics)

# Next we can look at the fit indices of our orthogonal models (lower EBIC = better fit). 

data.frame("Factors" = factors, "EBIC" = ortho_ebics) %>% kbl(caption = "Orthogonal Models: Fit Indices") %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center") %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), fixed_thead = T) %>%
  row_spec(best_fit, bold = T, color = "white", background = "#D7261E")

fa_model <- orthos[[best_fit]]

print(fa_model, digits = 2)

# check correlation from oblique model

obliq <- factors %>% purrr::map(~fa(pcp_outs$L, nfactors = ., n.obs = n, rotate = "oblimin", scores = "regression"))

obliq %>% walk(print, digits = 2, sort = T)

obliq_ebics <- obliq %>% map_dbl(~.$EBIC)

best_fit <- which.min(ortho_ebics)

loadings <- as_tibble(cbind(rownames(fa_model$loadings[]), fa_model$loadings[]))
colnames(loadings)[1] <- "Variable"

loadings <- loadings %>% mutate_at(colnames(loadings)[str_starts(colnames(loadings), "MR")], as.numeric)

loadings$Max <- colnames(loadings[, -1])[max.col(loadings[, -1], ties.method = "first")] # should be 2:5


loadings %>% kbl(caption = "Loadings") %>% kable_classic(full_width = F, html_font = "Cambria", position = "center") %>% 
  kable_styling(bootstrap_options = c("hover", "condensed"), fixed_thead = T) %>% scroll_box(width = "100%", height = "400px") 


scores <- as.tibble(cbind(rownames(fa_model$scores[]), fa_model$scores[])) %>% mutate_all(as.numeric)
# colnames(scores)[1] <- "Subject_id"
# scores[,1] <- rownames(fa_model$scores[])

scores$Max <- colnames(scores)[max.col(scores, ties.method = "first")]

scores %>% kbl(caption = "Scores") %>% kable_classic(full_width = F, html_font = "Cambria", position = "center") %>% 
  kable_styling(bootstrap_options = c("hover", "condensed"), fixed_thead = T) %>% scroll_box(width = "100%", height = "400px")

fa_pats <- loadings %>% 
  dplyr::select(-Max, -Variable) %>% 
  mutate_all(as.numeric)

fa_pats <- fa_pats %>% dplyr::select(sort(colnames(.))) %>% as.matrix()

               
dat <- cbind(colgroups_l, fa_pats)

order <- c("ADHDCTOT", "ADHDCIA", "ADHDCHI", "ADHDWTOT", "ADHDWIA", "ADHDWHI",
           "total_self_control_prob", 
           "Externalizing_Problems_Total", "Internalizing_Problems_Total", "Thought_Problems_Total", "Attention_Problems_Total", 
           "risky_drug_use",
           "other_drugs", "Joints", "Cigarettes", "Drinks",
           "KS37", "KS38", "KS39", "KS19", "KS11", "KS12" 
)
dat <- dat[match(order, dat$column_names),]
# informant <- c("Child", 
#                "Child", "Child", "Child", "Child", "Child",
#                )
new_column_names <- c("ADHDCTOT", "ADHDCIA", "ADHDCHI", "ADHDWTOT", "ADHDWIA", "ADHDWHI",
                               "Total_self_control_prob", 
                               "Externalizing_Problems_Total", "Internalizing_Problems_Total", "Thought_Problems_Total", "Attention_Problems_Total", 
                               "Risky_drug_use",
                               "Other_drugs", "Joints", "Cigarettes", "Drinks",
                               "KS_Tobacco", "KS_Alcohol", "KS_Substance", "KS_Psychosis", "KS_Inattentive", "KS_hyperact_impulsive")
dat$column_names <- new_column_names
p <- 2
png(paste0(output.folder, pcp_run, "_l_fa_", p, "patterns_rev_scs_renames.png"), 1250, 460)
print_patterns_loc(dat[,c("MR1", "MR2", "MR3")], colgroups = dat[,c("column_names", "family")], pat_type = "factor", n = p, title = "FA factors", 
                   size_line = 1.5, size_point = 3, ylim_min = -1, ylim_max = 1.01)
dev.off()

# for table 
dat_round <- dat
dat_round[,c("MR1", "MR2", "MR3")] <- round(dat_round[,c("MR1", "MR2", "MR3")],2)

png(paste0(output.folder, pcp_run, "_loadings_table.png"), 900, 700)
grid.table(dat_round)
dev.off()
### Factor Correlation
scores %>% dplyr::select(-c(Max)) %>% corr.test() %>% print(short=FALSE)

### Visualize Data
png(paste0(output.folder, pcp_run, "_l_fa_loadings_1_2_f_rev_scs.png"), 900, 460)
loadings %>% 
  ggplot(aes(x = MR1, y = MR2, col = colgroups_l$family)) + 
  geom_point() + geom_label_repel(aes(label = Variable, col = colgroups_l$family),
                                  box.padding   = 0.35,
                                  point.padding = 0.5,
                                  segment.color = 'grey50') + 
  theme(legend.position = "bottom") +
  labs(title = "Variable Loadings on First and Second Factors")
dev.off()

if ("MR3" %in% colnames(loadings)) {
  png(paste0(output.folder, pcp_run, "_l_fa_loadings_1_3_f_rev_scs.png"), 900, 460)
  loadings %>% 
    ggplot(aes(x = MR1, y = MR3, col = colgroups_l$family)) + 
    geom_point() + geom_label_repel(aes(label = Variable, col = colgroups_l$family),
                                    box.padding   = 0.35,
                                    point.padding = 0.5,
                                    segment.color = 'grey50') + 
    theme(legend.position = "bottom") +
    labs(title = "Variable Loadings on First and Third Factors")
  dev.off()
}


plot_loadings <- loadings %>% dplyr::select(-Max) %>% gather(key = "Factor", value = "Loading", -Variable) %>% mutate(Factor = Factor)

png(paste0(output.folder, pcp_run, "_l_fa_var_loadings_rev_scs.png"), 900, 460)
plot_loadings %>% 
  ggplot(aes(x = Factor, y = Loading, fill = Factor)) + geom_col(position = "dodge") +
  facet_wrap(~ Variable) + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() + geom_hline(yintercept = 0, size = 0.2) +
  labs(title = "Variable Loadings on All Factors")
dev.off()

png(paste0(output.folder, pcp_run, "_l_fa_scores.png_rev_scs"), 900, 460)
scores %>% ggplot(aes(x = Max, fill = Max)) + geom_bar() +
  labs(x = "Factors", y = "Number of Individuals", title = "Number with Highest Scores per Factor") +
  theme(legend.position = "none")
dev.off()

scores %>% dplyr::group_by(Max) %>% dplyr::summarise(n())

png(paste0(output.folder, pcp_run, "_l_fa_scores_dens_rev_scs.png"), 900, 460)
scores %>% gather(key = "factor", value = "score", -Max) %>% dplyr::select(-Max) %>% 
  ggplot(aes(x = score)) + geom_density() + facet_grid(factor~.) 
dev.off()

# not running nmf because l matrix has 6% of values below zero

## save scores

# assign SID
scores$SID <- outc_prep$SID
scores <- scores[,c("SID", "MR1", "MR2", "MR3")]
# save
saveRDS(scores, paste0(generated.data.folder, "outcome_pcp_fa_profiles_scores_", case, "_n_", nrow(scores), "_rev_scs.rds"))
#saveRDS(scores, paste0(generated.data.folder, "outcome_pcp_fa_profiles_scores_", case, "_n_", nrow(scores), "rev1.rds"))
