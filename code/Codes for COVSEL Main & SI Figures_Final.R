install.packages("CovSel")
install.packages("cubature")
install.packages("dr")
install.packages("ggpubr")
install.packages("ggsci")
install.packages("ggtext")
install.packages("glue")
install.packages("haven")
install.packages("MASS")
install.packages("np")
install.packages("plotrix")
install.packages("RColorBrewer")
install.packages("scales")
install.packages("tidyverse")

library(CovSel)
library(cubature)
library(dr)
library(ggpubr)
library(ggsci)
library(ggtext)
library(glue)
library(haven)
library(MASS)
library(np)
library(plotrix)
library(RColorBrewer)
library(scales)
library(tidyverse)

setwd("")


# COVEL for Treatment 1 (Exposed non-participant)

matching<- read_dta("Varsfor_COVSEL_Final.dta")

# Select covarites for matrix
selected_vars <- c(
  "tmin_rabi_1995_2014", "tmax_rabi_1995_2014", "prec_rabi_1995_2014",
  "tmin_kharif_1995_2014", "tmax_kharif_1995_2014", "prec_kharif_1995_2014",
  "gps_resp_housealtitude", "agroeco_dummy_1", "agroeco_dummy_2",
  "dist_Hoshangabad", "dist_Jabalpur", "dist_Sehore", "dist_Shajapur",
  "puccaroad_distance", "inputmarket_distance", "cermarket_distance",
  "vegmarket_distance", "fruitmarket_distance", "pulsmarket_distance",
  "kvk_distance", "Distancetotheclosestgovernme", "Distancetotheclosestprivate",
  "Distancetotheclosestforestry", "dist_rangeoffice", "caste_ST", "caste_sc",
  "caste_obc", "hh_mem_total", "resp_age", "resp_schoolyr",
  "tot_land_operate_acre", "tot_land_operate_acre_2", "credit_aces",
  "dummy_irrigation_GW", "yr2000_tree_cover_percent"
)


# Create the matrix
my_matrix <- as.matrix(matching[, selected_vars])

# View the first few rows of the matrix
head(my_matrix)

ans1 <- cov.sel(T = matching$heard_SMAFF2, Y = matching$tree_grow_dummy, 
                X = my_matrix, type = "np",  alg = 3, 
                scope = NULL, alpha = 0.3, trace = 0,  
                thru=0.5, thro=0.25, thrc=100, method = "save", 
                na.action = "na.omit")
print(ans1)
summary(ans1)

ans2 <- cov.sel(T = matching$heard_SMAFF2, Y = matching$tot_tree, 
                X = my_matrix, type = "np",  alg = 3, 
                scope = NULL, alpha = 0.3, trace = 0,  
                thru=0.5, thro=0.25, thrc=100, method = "save", 
                na.action = "na.omit")
print(ans2)
summary(ans2)



ans3 <- cov.sel(T = matching$heard_SMAFF2 , Y = matching$per_hect_tree, 
                X = my_matrix, type = "np",  alg = 3, 
                scope = NULL, alpha = 0.3, trace = 0,  
                thru=0.5, thro=0.25, thrc=100, method = "save", 
                na.action = "na.omit")
print(ans3)

# Create complete cases index (to drop no tree growers)
complete_cases <- complete.cases(matching$ratio_hort_tot_tree, matching$heard_SMAFF2, my_matrix)

ans4 <- cov.sel(T = matching$heard_SMAFF2[complete_cases], Y = matching$ratio_hort_tot_tree[complete_cases], 
                X = my_matrix[complete_cases, ], type = "np",  alg = 3, 
                scope = NULL, alpha = 0.3, trace = 0,  
                thru=0.5, thro=0.25, thrc=100, method = "save", 
                na.action = "na.omit")
print(ans4)
summary(ans4)

complete_cases <- complete.cases(matching$ratio_timb_tot_tree, matching$heard_SMAFF2, my_matrix)
ans5 <- cov.sel(T = matching$heard_SMAFF2[complete_cases], Y = matching$ratio_timb_tot_tree[complete_cases], 
                X = my_matrix[complete_cases, ], type = "np",  alg = 3, 
                scope = NULL, alpha = 0.3, trace = 0,  
                thru=0.5, thro=0.25, thrc=100, method = "save", 
                na.action = "na.omit")
print(ans5)
summary(ans5)


complete_cases <- complete.cases(matching$ratio_bmbo_tot_tree, matching$heard_SMAFF2, my_matrix)
ans6 <- cov.sel(T = matching$heard_SMAFF2[complete_cases], Y = matching$ratio_bmbo_tot_tree[complete_cases], 
                X = my_matrix[complete_cases, ], type = "np",  alg = 3, 
                scope = NULL, alpha = 0.3, trace = 0,  
                thru=0.5, thro=0.25, thrc=100, method = "save", 
                na.action = "na.omit")
print(ans6)
summary(ans6)
summary(ans6)


# COVEL for Treatment 2 (Participant)

matching<- read_dta("Varsfor_COVSEL_Final.dta")

# Select covarites for matrix
selected_vars <- c(
  "tmin_rabi_1995_2014", "tmax_rabi_1995_2014", "prec_rabi_1995_2014",
  "tmin_kharif_1995_2014", "tmax_kharif_1995_2014", "prec_kharif_1995_2014",
  "gps_resp_housealtitude", "agroeco_dummy_1", "agroeco_dummy_2",
  "dist_Hoshangabad", "dist_Jabalpur", "dist_Sehore", "dist_Shajapur",
  "puccaroad_distance", "inputmarket_distance", "cermarket_distance",
  "vegmarket_distance", "fruitmarket_distance", "pulsmarket_distance",
  "kvk_distance", "Distancetotheclosestgovernme", "Distancetotheclosestprivate",
  "Distancetotheclosestforestry", "dist_rangeoffice", "caste_ST", "caste_sc",
  "caste_obc", "hh_mem_total", "resp_age", "resp_schoolyr",
  "tot_land_operate_acre", "tot_land_operate_acre_2", "credit_aces",
  "dummy_irrigation_GW", "yr2000_tree_cover_percent"
)


# Create the matrix
my_matrix <- as.matrix(matching[, selected_vars])

# View the first few rows of the matrix
head(my_matrix)


complete_cases <- complete.cases(matching$tree_grow_dummy, matching$partcp_SMAFF2, my_matrix)


ans1_b <- cov.sel(T = matching$partcp_SMAFF2[complete_cases], Y = matching$tree_grow_dummy[complete_cases], 
                  X = my_matrix[complete_cases, ], type = "np",  alg = 3, 
                  scope = NULL, alpha = 0.3, trace = 0,  
                  thru=0.5, thro=0.25, thrc=100, method = "save", 
                  na.action = "na.omit")
print(ans1_b)
summary(ans1_b)

# Create complete cases index
complete_cases <- complete.cases(matching$tot_tree, matching$partcp_SMAFF2, my_matrix)
ans2_b <- cov.sel(T = matching$partcp_SMAFF2[complete_cases], Y = matching$tot_tree[complete_cases], 
                  X = my_matrix[complete_cases, ], type = "np",  alg = 3, 
                  scope = NULL, alpha = 0.3, trace = 0,  
                  thru=0.5, thro=0.25, thrc=100, method = "save", 
                  na.action = "na.omit")
print(ans2_b)
summary(ans2_b)


# Create complete cases index
complete_cases <- complete.cases(matching$per_hect_tree, matching$partcp_SMAFF2, my_matrix)
ans3_b <- cov.sel(T = matching$partcp_SMAFF2[complete_cases], Y = matching$per_hect_tree[complete_cases], 
                  X = my_matrix[complete_cases, ], type = "np",  alg = 3, 
                  scope = NULL, alpha = 0.3, trace = 0,  
                  thru=0.5, thro=0.25, thrc=100, method = "save", 
                  na.action = "na.omit")
print(ans3_b)
summary(ans3_b)

# Create complete cases index
complete_cases <- complete.cases(matching$ratio_hort_tot_tree, matching$partcp_SMAFF2, my_matrix)
ans4_b <- cov.sel(T = matching$partcp_SMAFF2[complete_cases], Y = matching$ratio_hort_tot_tree[complete_cases], 
                  X = my_matrix[complete_cases, ], type = "np",  alg = 3, 
                  scope = NULL, alpha = 0.3, trace = 0,  
                  thru=0.5, thro=0.25, thrc=100, method = "save", 
                  na.action = "na.omit")
print(ans4_b)
summary(ans4_b)

complete_cases <- complete.cases(matching$ratio_timb_tot_tree, matching$partcp_SMAFF2, my_matrix)
ans5_b <- cov.sel(T = matching$partcp_SMAFF2[complete_cases], Y = matching$ratio_timb_tot_tree[complete_cases], 
                  X = my_matrix[complete_cases, ], type = "np",  alg = 3, 
                  scope = NULL, alpha = 0.3, trace = 0,  
                  thru=0.5, thro=0.25, thrc=100, method = "save", 
                  na.action = "na.omit")
print(ans5_b)
summary(ans5_b)


complete_cases <- complete.cases(matching$ratio_bmbo_tot_tree, matching$partcp_SMAFF2, my_matrix)
ans6_b <- cov.sel(T = matching$partcp_SMAFF2[complete_cases], Y = matching$ratio_bmbo_tot_tree[complete_cases], 
                  X = my_matrix[complete_cases, ], type = "np",  alg = 3, 
                  scope = NULL, alpha = 0.3, trace = 0,  
                  thru=0.5, thro=0.25, thrc=100, method = "save", 
                  na.action = "na.omit")
print(ans6_b)
summary(ans6_b)


# Figures in Main Text

df1 <- data.frame(X = factor(c("Small farmer","Large farmer",
                               "Small farmer","Large farmer"),
                             levels = c("Small farmer", "Large farmer")),
                  Y = c(29, 121, 38, 22),
                  Se = c(5, 22, 7.06, 3.84),
                  Z = c("Number of commercial trees grown", "Number of commercial trees grown", 
                        "Commercial trees grown per hectare", "Commercial trees grown per hectare"))

df1$Z <- factor(df1$Z, levels = c("Number of commercial trees grown", "Commercial trees grown per hectare"))

dodge_width <- 0.5

fig1 <- ggplot(df1, aes(x = Z, fill = X)) + 
  geom_bar(aes(y = ifelse(Z == "Number of commercial trees grown", Y, Y * 4)),  
           stat = "identity", position = position_dodge(width = dodge_width), width = 0.4) +  
  geom_errorbar(aes(ymin = ifelse(Z == "Number of commercial trees grown", Y - 1.96 * Se, (Y - 1.96 * Se) * 4), 
                    ymax = ifelse(Z == "Number of commercial trees grown", Y + 1.96 * Se, (Y + 1.96 * Se) * 4)), 
                width = 0.2, linewidth = 1, position = position_dodge(width = dodge_width), alpha = 0.5) +
  scale_fill_manual(values = c("#c8dcf0", "#2979b9")) +  
  scale_y_continuous(name = "Mean number", 
                     sec.axis = sec_axis(~./4, name = "Mean number")) +
  theme_bw() +
  guides(fill = "none") +  
  theme(
    axis.title.x = element_blank(),
    text = element_text(family = "sans", size = 20),
    axis.title.y = element_text(size = 20, margin = margin(r = 5)),
    axis.title.y.right = element_text(size = 20, margin = margin(l = 5)),
    axis.text = element_text(size = 20, color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 20),
    axis.text.y = element_text(size = 20),
    panel.grid.major = element_line(linewidth = 0.25),
    panel.grid.minor = element_blank()
  )

fig1

df2 <- data.frame(X = factor(c("Small farmer","Large farmer",
                               "Small farmer","Large farmer",
                               "Small farmer","Large farmer",
                               "Small farmer","Large farmer"),
                             levels = c("Small farmer", "Large farmer")),
                  Y = c(0.63, 0.77, 0.65, 0.63, 0.14, 0.15, 0.19, 0.20),
                  Se = c(0.02, 0.01, 0.02, 0.02, 0.01, 0.01, 0.02, 0.01),
                  Z = c("Farmers grew commercial trees", "Farmers grew commercial trees",
                        "Fruit trees among commercial trees", "Fruit trees among commercial trees",
                        "Timber trees among commercial trees", "Timber trees among commercial trees",
                        "Bamboo plants among commercial trees", "Bamboo plants among commercial trees"))

# Create 2-line versions of Z labels
df2$Z_2line <- factor(df2$Z, 
                      levels = c("Farmers grew commercial trees", 
                                 "Fruit trees among commercial trees", 
                                 "Timber trees among commercial trees", 
                                 "Bamboo plants among commercial trees"))

# Replace long labels with 2-line versions
levels(df2$Z_2line) <- c(
  "Farmers grew\ncommercial trees",
  "Fruit trees among\ncommercial trees",
  "Timber trees among\ncommercial trees",
  "Bamboo plants among\ncommercial trees"
)

dodge_width <- 0.7

fig2 <- ggplot(df2, aes(x = Z_2line, y = Y, fill = X)) + 
  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = 0.6) +
  geom_errorbar(aes(ymin = Y - 1.96 * Se, ymax = Y + 1.96 * Se), 
                width = 0.2, linewidth = 1, position = position_dodge(width = dodge_width), alpha = 0.5) +
  scale_fill_manual(values = c("#c8dcf0", "#2979b9")) +
  labs(y = "Mean fraction") +
  theme_bw() +
  guides(fill = guide_legend(title = NULL)) + 
  theme(
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 20),
    legend.key.size = unit(1.2, "cm"),
    legend.spacing.x = unit(0.8, "cm"),
    text = element_text(family = "sans", size = 20),
    axis.title.y = element_text(size = 20, margin = margin(r = 5)),
    axis.title.y.right = element_text(size = 20, margin = margin(l = 5)),
    axis.text = element_text(size = 20, color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 20),
    axis.text.y = element_text(size = 20),
    panel.grid.major = element_line(linewidth = 0.25),
    panel.grid.minor = element_blank()
  )

fig2

ggarrange(fig1, fig2,
          labels = c("A", "B"),
          nrow = 2, ncol = 1)
ggsave("Fig 1.png", height = 12, width = 15)


## Different scale for Y axia (tot tree, tree density)
df3 <- data.frame(X = c("Unexposed","Exposed non-participant", 
                        "Participant",
                        "Unexposed","Exposed non-participant", 
                        "Participant"),
                  Y = c(4, 8, 28, 1.69, 2.47, 9.74),
                  LL = c(4, 4, 15, 1.40, 1.90, 4.82),
                  UL = c(5, 11, 62, 2.14, 4.51, 18.11), 
                  Z = c("Number of commercial trees grown", "Number of commercial trees grown", "Number of commercial trees grown", 
                        "Commercial trees grown per hectare", "Commercial trees grown per hectare", "Commercial trees grown per hectare"))

df3$Z <- factor(df3$Z, levels = c("Number of commercial trees grown", "Commercial trees grown per hectare"))
df3$X <- factor(df3$X, levels = c("Unexposed", "Exposed non-participant", "Participant"))
dodge_width <- 0.7  # Adjust for proper bar separation

fig3 <- ggplot(df3, aes(x = Z, fill = X)) + 
  geom_bar(aes(y = ifelse(Z == "Number of commercial trees grown", Y, Y * 4)),  
           stat = "identity", position = position_dodge(width = dodge_width), width = 0.6) +
  geom_errorbar(aes(ymin = ifelse(Z == "Number of commercial trees grown", LL, LL * 4), 
                    ymax = ifelse(Z == "Number of commercial trees grown", UL, UL * 4)), 
                width = 0.2, size = 1, position = position_dodge(width = dodge_width), alpha = 0.5) +
  scale_fill_manual(values = c("Unexposed" = "#73b2d8", 
                               "Exposed non-participant" = "#2979b9", 
                               "Participant" = "#08306b")) +
  scale_y_continuous(name = "Median value", 
                     sec.axis = sec_axis(~./4, name = "Median value")) +
  theme_bw() +
  guides(fill = "none") +  # Remove the legend
  theme(
    axis.title.x = element_blank(),
    text = element_text(family = "sans", size = 20),
    axis.title.y = element_text(size = 20, margin = margin(r = 5)),
    axis.title.y.right = element_text(size = 20, margin = margin(l = 5)),
    axis.text = element_text(size = 20, color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 20),
    axis.text.y = element_text(size = 20),
    panel.grid.major = element_line(linewidth = 0.25),
    panel.grid.minor = element_blank()
  )

fig3

df4 <- data.frame(X = factor(c("Unexposed","Exposed non-participant", 
                               "Participant",
                               "Unexposed","Exposed non-participant", 
                               "Participant",
                               "Unexposed","Exposed non-participant", 
                               "Participant",
                               "Unexposed","Exposed non-participant", 
                               "Participant"),
                             levels = c("Unexposed","Exposed non-participant", "Participant")),
                  Y = c(0.69, 0.74, 0.83, 0.66, 0.65, 0.48, 0.12, 0.13, 0.32, 0.20,
                        0.22, 0.19),
                  Se = c(0.01, 0.03, 0.04, 0.02, 0.04, 0.05, 0.01, 0.02, 0.04, 0.01, 0.03, 0.04),
                  Z = c("Farmers grew commercial trees", "Farmers grew commercial trees", "Farmers grew commercial trees",
                        "Fruit trees among commercial trees", "Fruit trees among commercial trees",
                        "Fruit trees among commercial trees",
                        "Timber trees among commercial trees", "Timber trees among commercial trees", 
                        "Timber trees among commercial trees", 
                        "Bamboo plants among commercial trees", "Bamboo plants among commercial trees", 
                        "Bamboo plants among commercial trees"))

df4

df4$Z <- factor(df4$Z, levels = c("Farmers grew commercial trees", "Fruit trees among commercial trees", "Timber trees among commercial trees", "Bamboo plants among commercial trees"))

# Create 2-line versions of Z labels for df4 ONLY
df4$Z_2line <- factor(df4$Z, 
                      levels = c("Farmers grew commercial trees", 
                                 "Fruit trees among commercial trees", 
                                 "Timber trees among commercial trees", 
                                 "Bamboo plants among commercial trees"))

# Replace long labels with 2-line versions
levels(df4$Z_2line) <- c(
  "Farmers grew\ncommercial trees",
  "Fruit trees among\ncommercial trees",
  "Timber trees among\ncommercial trees",
  "Bamboo plants among\ncommercial trees"
)

dodge_width <- 0.7  # Adjust for proper spacing

fig4 <- ggplot(df4, aes(x = Z_2line, y = Y, fill = X)) + 
  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = 0.6) +  
  geom_errorbar(aes(ymin = Y - 1.96 * Se, ymax = Y + 1.96 * Se), 
                width = 0.2, size = 1, position = position_dodge(width = dodge_width), alpha = 0.5) +
  scale_fill_manual(values = c("Unexposed" = "#73b2d8", 
                               "Exposed non-participant" = "#2979b9", 
                               "Participant" = "#08306b")) +
  labs(y = "Mean fraction") +
  theme_bw() +
  guides(fill = guide_legend(title = NULL)) +
  theme(
    axis.title.x = element_blank(),
    legend.position = "bottom",
    text = element_text(family = "sans", size = 20),
    axis.title.y = element_text(size = 20, margin = margin(r = 5)),
    axis.text = element_text(size = 20, color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 20),
    axis.text.y = element_text(size = 20),
    # Enhanced legend like fig2
    legend.text = element_text(size = 20),
    legend.key.size = unit(1.2, "cm"),
    legend.spacing.x = unit(0.8, "cm"),
    legend.margin = margin(t = 10, b = 5),
    panel.grid.major = element_line(linewidth = 0.25), 
    panel.grid.minor = element_blank()
  )

fig4

ggarrange(fig3, fig4,
          labels = c("A", "B"),
          nrow = 2, ncol = 1)
ggsave("Fig 2.png", height = 12, width = 14)


## Causal impact Figs from IPWRA result ##

df5 <- data.frame(Y = c("Exposed non-participant",
                        "Exposed non-participant",
                        "Exposed non-participant",
                        "Exposed non-participant",
                        "Exposed non-participant",
                        "Exposed non-participant",
                        "Participant",
                        "Participant",
                        "Participant",
                        "Participant",
                        "Participant",
                        "Participant"),
                  ATET = c(6, 0, 5, 0, -13, 0,
                           19, 89, 77, -13, 80, -24),
                  LL = c(-4, -100, -87, -13,
                         -47, -33, 7, -11,
                         2, -31, 27, -71),
                  UL = c(17, 100, 97, 15,
                         20, 38, 30, 200,
                         152, 3, 133, 14),
                  Z = c(
                    "Farmers grew commercial trees",
                    "Number of commercial trees grown",
                    "Commercial trees grown per hectare",
                    "Fruit trees among commercial trees",
                    "Timber trees among commercial trees",
                    "Bamboo plants among commercial trees",
                    "Farmers grew commercial trees",
                    "Number of commercial trees grown",
                    "Commercial trees grown per hectare",
                    "Fruit trees among commercial trees",
                    "Timber trees among commercial trees",
                    "Bamboo plants among commercial trees"
                  )
)

# Create 2-line versions of Z labels
df5$Z_2line <- factor(df5$Z, 
                      levels = c("Fruit trees among commercial trees", 
                                 "Bamboo plants among commercial trees",
                                 "Farmers grew commercial trees", 
                                 "Commercial trees grown per hectare",
                                 "Timber trees among commercial trees", 
                                 "Number of commercial trees grown"))

df5$Z_2line <- factor(df5$Z, 
                      levels = c("Bamboo plants among commercial trees",
                                 "Fruit trees among commercial trees", 
                                 "Farmers grew commercial trees", 
                                 "Commercial trees grown per hectare",
                                 "Timber trees among commercial trees", 
                                 "Number of commercial trees grown"))

# Replace long labels with 2-line versions - FLIPPED ORDER to match levels
levels(df5$Z_2line) <- c(
  "Bamboo plants\namong commercial trees",
  "Fruit trees\namong commercial trees",
  "Farmers grew\ncommercial trees",
  "Commercial trees\ngrown per hectare",
  "Timber trees\namong commercial trees",
  "Number of commercial\ntrees grown"
)

# Create y positions
df5$y_position <- as.numeric(df5$Z_2line) + ifelse(df5$Y == "Participant", 0.2, -0.2)

fig5 <- ggplot(df5, aes(x = ATET, y = y_position, color = Y)) +
  geom_point(size = 3) +  
  geom_errorbar(aes(xmin = LL, xmax = UL), 
                width = 0.25, linewidth = 1.2,   
                position = position_dodge(width = 0.5), alpha = 0.7) +  
  # ATET values with NO decimal, larger font
  geom_text(aes(label = sprintf("%.0f", ATET)),
            position = position_dodge(width = 0.5), 
            vjust = ifelse(df5$Y == "Exposed non-participant", -0.5, 1.5),
            size = 7, fontface = "bold") +  
  geom_vline(xintercept = 0, linetype = "solid", linewidth = 0.5) +
  labs(y = "Tree growing outcomes", color = "") +  
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  # Use the 2-line labels
  scale_y_continuous(breaks = 1:length(levels(df5$Z_2line)), 
                     labels = levels(df5$Z_2line)) +
  theme_bw() +
  scale_color_manual(values = c("Exposed non-participant" = "#1E90FF", 
                                "Participant" = "#08306b")) +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),
    text = element_text(family = "sans", size = 26),
    axis.title.y = element_text(size = 26, margin = margin(r = 12)),
    axis.text = element_text(size = 26, color = "black"), 
    axis.text.y = element_text(size = 26, hjust = 1, lineheight = 0.9), 
    axis.text.x = element_text(size = 26),  
    # Enhanced legend with size 30
    legend.text = element_text(size = 30),  
    legend.key.size = unit(1.6, "cm"),      
    legend.spacing.x = unit(1.2, "cm"),     
    legend.margin = margin(t = 15, b = 10),  
    panel.grid.major = element_line(linewidth = 0.25),
    panel.grid.minor = element_blank(),  
    plot.margin = margin(15, 15, 15, 15)
  )
fig5

ggsave("Fig3.png", height = 18, width = 20,  dpi = 300)


#Figures for Supplementary Information

## Data file consists of village, percentage of hh exposed to (non-participants & participants) agroforestry program
agftreat<- read_dta("Agroforestry exposure by village.dta")
names(agftreat)

## note: pct_heard_SMAFF2 is % of household in each village is exposed to agroforestry program 

freq_data <- agftreat %>%
  mutate(
    pct_heard_SMAFF2 = round(pct_heard_SMAFF2, 0)  # Round to 0 decimals
  ) %>%
  count(pct_heard_SMAFF2)


freq_data$pct_heard_SMAFF2 <- factor(freq_data$pct_heard_SMAFF2,
                                     levels = sort(unique(freq_data$pct_heard_SMAFF2)))


fig6 <- ggplot(freq_data, aes(x = pct_heard_SMAFF2, y = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#73b2d8") +
  scale_x_discrete(
    labels = function(x) paste0(x, "%") 
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(),
    labels = scales::number_format(accuracy = 1)   # Whole numbers on y-axis
  ) +
  labs(x = "Farmers exposed to agroforestry program",
       y = "Frequency of number of villages") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)   
  )

fig6

ggsave("Fig SI 2.png", height = 5, width = 7.5)

## Planting dates for commercial trees planted by farmers  

treeage <- read_dta("Tree_Plant_Year.dta")
names(treeage)

fig7 <- ggplot(treeage, aes(x = treeplnt_year_1)) +
  geom_histogram(bins = 100, color = "white", fill = "#73b2d8") +
  labs(x = "Year of planting",
       y = "Frequency") +
  theme_bw()

fig7

ggsave("Fig SI 4.png", height = 5, width = 7.5)
