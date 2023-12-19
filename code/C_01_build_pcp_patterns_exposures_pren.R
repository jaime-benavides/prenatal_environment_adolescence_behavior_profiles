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
mon <- 0
na_level <- 50
scale <- "TRUE"
case <- "na_50_reduced_rev_grav_corr_rev_shs_rev_valid_part"
expo_prep <- readRDS(paste0(generated.data.folder, "data_row_comp_", case, ".rds"))
data <- list("M" = expo_prep[,-c(which(colnames(expo_prep) %in% c("SID", "month")))]) %>% purrr::map(as.matrix)
# outcome_description <- readRDS(paste0(generated.data.folder, "desc_outc_",mon/12, "_yrs_na_", na_level ,".rds"))
exposure_description <- readRDS(paste0(generated.data.folder, "exposure_description.rds"))

# read pcp results (todo harmonize descriptions)
pcp_outs <- readRDS(paste0(generated.data.folder, "pren_exposures_pcp_rrmc_na_", na_level, "_scale_TRUE_grav_rev_SHS_rev_valid_part.rds")) # pcp_outs
pcp_run <- paste0("rrmc_exposure_", mon/12, "_yrs_na_", na_level, "_scale_", scale, "rev_grav_corr_rev_SHS_rev_valid_part")

cn_orig <- colnames(pcp_outs$S)
pcp_outs$S <- pcp_outs$S[,c("demoralization", "material_hardship", "SHS","totalpah", "madducts", "cadducts",
                            "MEHHP", "MECPP", "MEOHP", "MEHP", "MCPP", "MIBP", "MBP", 
                            "MBZP", "MEP", "UBPA")]
cn <- colnames(pcp_outs$S)
exposure_description <- exposure_description[which(exposure_description$exposure %in% cn),]
exposure_description <- exposure_description[which(exposure_description$month == mon),]
cng <- exposure_description[,c("exposure", "family", "variable_description")]
cng <- cng[which(cng$exposure %in% cn),]



colnames(pcp_outs$L) <- cn_orig
pcp_outs$L <- pcp_outs$L[,c("demoralization", "material_hardship", "SHS","totalpah", "madducts", "cadducts",
                            "MEHHP", "MECPP", "MEOHP", "MEHP", "MCPP", "MIBP", "MBP", 
                            "MBZP", "MEP", "UBPA")]
L.df <- pcp_outs$L %>% 
  as.data.frame()   %>% 
  pivot_longer(cols = cn, names_to = "exposure") %>%
  right_join(., cng, by = "exposure")

S.df <- pcp_outs$S %>% 
  as.data.frame()   %>% 
  pivot_longer(cols = cn, names_to = "exposure") %>%
  right_join(., cng, by = "exposure")


L.hm <- heatmaply::heatmaply(pcp_outs$L, Colv = F, Rowv = F, labRow = NULL,
                  cexRow = 100,
                  showticklabels = c(T, F), main = "L matrix")
# saved by hand paste0(output.folder, "s_matrix_heatmap", pcp_run ,".png")
L.hm

S.hm <- heatmaply::heatmaply(pcp_outs$S, Colv = F, Rowv = F, labRow = NULL,
                             cexRow = 100,
                             showticklabels = c(T, F), main = "S matrix")

S.hm


# L matrix correlations:
graph_title <- paste0("pcp_rrmc_exposure_", mon/12, "_yrs_na_", na_level, ": L Pearson correlation")
png(paste0(output.folder, pcp_run, "_l_matrix_corr.png"), 900, 460)
pcp_outs$L %>% GGally::ggcorr(., method = c("pairwise.complete.obs", "pearson"),
                              label = T, label_size = 3, label_alpha = T,
                              hjust = 1, nbreaks = 10, limits = TRUE,
                              size = 4, layout.exp = 5) + ggtitle(graph_title)
dev.off()

# S matrix correlations:
graph_title <- paste0("pcp_rrmc_exposure_", mon/12, "_yrs_na_", na_level, ": S Pearson correlation")
png(paste0(output.folder, pcp_run, "_s_matrix_corr.png"), 900, 460)
pcp_outs$S %>% GGally::ggcorr(., method = c("pairwise.complete.obs", "pearson"),
                              label = T, label_size = 3, label_alpha = T,
                              hjust = 1, nbreaks = 10, limits = TRUE,
                              size = 4, layout.exp = 5) + ggtitle(graph_title)
dev.off()


# PCA
ranktol <- 1e-04
L.rank <- Matrix::rankMatrix(pcp_outs$L, tol = ranktol)
scale_flag <- FALSE
pcs <- paste0("PC", 1:L.rank)

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

L.eda <-PCPhelpers::eda(pcp_outs$L, pcs = pcs, cor_lbl = T, scale_flag = scale_flag, colgroups = colgroups_l, rowgroups = NULL)

M.eda <-PCPhelpers::eda(data$M, pcs = pcs, cor_lbl = T, scale_flag = scale_flag, colgroups = colgroups_m, rowgroups = NULL)

M.eda$var  %>%
  as_image(file = paste0(output.folder, pcp_run, "_raw_pca_variance_grav_corr.png"))

png(paste0(output.folder, pcp_run, "_raw_pca_loads_grav_corr_rev_shs.png"), 900, 460)
M.eda$load
dev.off()

factors <- 1:L.rank

L.eda$var  %>%
  as_image(file = paste0(output.folder, pcp_run, "_l_pca_variance_grav_corr_rev_shs.png"))

png(paste0(output.folder, pcp_run, "_l_pca_loads_grav_corr_rev_shs.png"), 900, 460)
L.eda$load
dev.off()

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

print(fa_model, digits = 2)


png(paste0(output.folder, pcp_run, "_fa_explained.png"), 300, 400)
grid.table(round(fa_model$Vaccounted, 2))
dev.off()

loadings <- as_tibble(cbind(rownames(fa_model$loadings[]), fa_model$loadings[]))
colnames(loadings)[1] <- "Variable"

loadings <- loadings %>% mutate_at(colnames(loadings)[str_starts(colnames(loadings), "MR")], as.numeric)

loadings$Max <- colnames(loadings[, -1])[max.col(loadings[, -1], ties.method = "first")] # should be 2:5


loadings %>% kbl(caption = "Loadings") %>% kable_classic(full_width = F, html_font = "Cambria", position = "center") %>% 
  kable_styling(bootstrap_options = c("hover", "condensed"), fixed_thead = T) %>% scroll_box(width = "100%", height = "400px") 


scores <- as.tibble(cbind(rownames(fa_model$scores[]), fa_model$scores[])) %>% mutate_all(as.numeric)

scores$Max <- colnames(scores)[max.col(scores, ties.method = "first")]

scores %>% kbl(caption = "Scores") %>% kable_classic(full_width = F, html_font = "Cambria", position = "center") %>% 
  kable_styling(bootstrap_options = c("hover", "condensed"), fixed_thead = T) %>% scroll_box(width = "100%", height = "400px")

fa_pats <- loadings %>% 
  dplyr::select(-Max, -Variable) %>% 
  mutate_all(as.numeric)

# reformat variable names for manuscript todo
colgroups_l$column_names <- c("Demoralization", "Material_Hardship", "Secondhand_Smoke","Total_PAH", "Maternal_PAH_Adducts", "Cord_PAH_Adducts",
  "MEHHP", "MECPP", "MEOHP", "MEHP", "MCPP", "MiBP", "MnBP", "MBzP", "MEP", "Bisphenol_A")
colgroups_l$family <- c("Social_Stressors", "Social_Stressors", "Secondhand_Smoke","Air_Pollution", "PAH-DNA_Adducts", "PAH-DNA_Adducts",
                        "DEHP_Phthalates", "DEHP_Phthalates", "DEHP_Phthalates", "DEHP_Phthalates", "Non-DEHP_Phthalates", "Non-DEHP_Phthalates", "Non-DEHP_Phthalates", "Non-DEHP_Phthalates", "Non-DEHP_Phthalates", "Bisphenol_A")
fa_pats <- fa_pats %>% dplyr::select(sort(colnames(.))) %>% as.matrix()
p <- 3 # from 1 to 3
png(paste0(output.folder, pcp_run, "_l_fa_", p, "patterns.png"), 1200, 460)
print_patterns_loc(fa_pats, colgroups = colgroups_l, pat_type = "factor", n = p, title = "FA factors",
                   size_line = 1.5, size_point = 3)
dev.off()

# for table 
dat_round <- fa_pats
dat_round[,c("MR1", "MR2", "MR3")] <- round(dat_round[,c("MR1", "MR2", "MR3")],2)
dat_round <- as.data.frame(dat_round)
dat_round$var <- colgroups_l$column_names

png(paste0(output.folder, pcp_run, "_loadings_table.png"), 900, 700)
grid.table(dat_round)
dev.off()

### Factor Correlation
scores %>% dplyr::select(-c(Max)) %>% corr.test() %>% print(short=FALSE)

# ### Visualize Data
# png(paste0(output.folder, pcp_run, "_l_fa_loadings_1_2_f_grav_corr.png"), 900, 460)
# loadings %>% 
#   ggplot(aes(x = MR1, y = MR2, col = colgroups_l$family)) + 
#   geom_point() + geom_label_repel(aes(label = Variable, col = colgroups_l$family),
#                                   box.padding   = 0.35,
#                                   point.padding = 0.5,
#                                   segment.color = 'grey50') + 
#   theme(legend.position = "bottom") +
#   labs(title = "Variable Loadings on First and Second Factors")
# dev.off()
# 
# if ("MR3" %in% colnames(loadings)) {
#   png(paste0(output.folder, pcp_run, "_l_fa_loadings_1_3_f_grav_corr.png"), 900, 460)
#   loadings %>% 
#     ggplot(aes(x = MR1, y = MR3, col = colgroups_l$family)) + 
#     geom_point() + geom_label_repel(aes(label = Variable, col = colgroups_l$family),
#                                     box.padding   = 0.35,
#                                     point.padding = 0.5,
#                                     segment.color = 'grey50') + 
#     theme(legend.position = "bottom") +
#     labs(title = "Variable Loadings on First and Third Factors")
#   dev.off()
# }
# 
# 
# plot_loadings <- loadings %>% dplyr::select(-Max) %>% gather(key = "Factor", value = "Loading", -Variable) %>% mutate(Factor = Factor)
# 
# png(paste0(output.folder, pcp_run, "_l_fa_var_loadings.png"), 900, 460)
# plot_loadings %>% 
#   ggplot(aes(x = Factor, y = Loading, fill = Factor)) + geom_col(position = "dodge") +
#   facet_wrap(~ Variable) + 
#   theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
#   coord_flip() + geom_hline(yintercept = 0, size = 0.2) +
#   labs(title = "Variable Loadings on All Factors")
# dev.off()
# 
# png(paste0(output.folder, pcp_run, "_l_fa_scores.png"), 900, 460)
# scores %>% ggplot(aes(x = Max, fill = Max)) + geom_bar() +
#   labs(x = "Factors", y = "Number of Individuals", title = "Number with Highest Scores per Factor") +
#   theme(legend.position = "none")
# dev.off()
# 
# scores %>% dplyr::group_by(Max) %>% dplyr::summarise(n())
# 
# png(paste0(output.folder, pcp_run, "_l_fa_scores_dens.png"), 900, 460)
# scores %>% gather(key = "factor", value = "score", -Max) %>% dplyr::select(-Max) %>% 
#   ggplot(aes(x = score)) + geom_density() + facet_grid(factor~.) 
# dev.off()

# not running nmf because l matrix has 6% of values below zero

## save scores

# assign SID
scores$SID <- expo_prep$SID
scores <- scores[,c("SID", "MR1", "MR2", "MR3")]
# save
saveRDS(scores, paste0(generated.data.folder, "exposure_pcp_fa_profiles_scores_", case, "_n_438.rds"))

