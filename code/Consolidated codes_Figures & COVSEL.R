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

# Main Figures

## Figure 1: Tree growing outcomes by land holding size

## Note: In df1, we use different scale for Y axia (tot tree, tree density)

df1<- data.frame(X=factor(c("Small farmer","Large farmer",
                            "Small farmer","Large farmer"),
                          levels = c("Small farmer", "Large farmer")),
                 Y=c(29, 121, 15, 9),
                 Se=c(5, 22, 2.75, 1.53),
                 Z= c("Number of commercial trees grown", "Number of commercial trees grown", 
                      "Commercial trees grown per acre", "Commercial trees grown per acre"))
df1
df1$Z <- factor(df1$Z, levels = c("Number of commercial trees grown", "Commercial trees grown per acre"))


dodge_width <- 0.5  # Increase spacing between bars

fig1 <- ggplot(df1, aes(x = Z, fill = X)) + 
  geom_bar(aes(y = ifelse(Z == "Number of commercial trees grown", Y, Y * 10)),  
           stat = "identity", position = position_dodge(width = dodge_width), width = 0.4) +  # Reduce bar width
  geom_errorbar(aes(ymin = ifelse(Z == "Number of commercial trees grown", Y - 1.96 * Se, (Y - 1.96 * Se) * 10), 
                    ymax = ifelse(Z == "Number of commercial trees grown", Y + 1.96 * Se, (Y + 1.96 * Se) * 10)), 
                width = 0.2, linewidth = 1, position = position_dodge(width = dodge_width), alpha = 0.5) +
  scale_fill_brewer(palette = "Pastel1") +
  scale_y_continuous(name = "Mean number", 
                     sec.axis = sec_axis(~./10, name = "Mean number")) +
  theme_bw() +
  guides(fill = "none") +  # Remove the legend
  theme(
    axis.title.x = element_blank(),
    text = element_text(family = "sans", size = 8), # Base font settings
    axis.title.y = element_text(size = 8, margin = margin(r = 5)), # Y-axis title
    axis.title.y.right = element_text(size = 8, margin = margin(l = 5)), # Right Y-axis title
    axis.text = element_text(size = 7, color = "black"), # Axis tick labels
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1), # X-axis text positioning
    panel.grid.major = element_line(linewidth = 0.25), # Thinner grid lines
    panel.grid.minor = element_blank() # No minor grid lines
  )



fig1


##  

df2<- data.frame(X=factor(c("Small farmer","Large farmer",
                            "Small farmer","Large farmer",
                            "Small farmer","Large farmer",
                            "Small farmer","Large farmer"),
                          levels = c("Small farmer", "Large farmer")),
                 Y=c(0.63, 0.77, 0.65, 0.63, 0.14, 0.15, 0.19, 0.20),
                 Se=c(0.02, 0.01, 0.02, 0.02, 0.01, 0.01, 0.02, 0.01),
                 Z= c("Farmers grew commercial trees", "Farmers grew commercial trees",
                      "Fruit trees among commercial trees", "Fruit trees among commercial trees",
                      "Timber trees among commercial trees", "Timber trees among commercial trees",
                      "Bamboo plants among commercial trees", "Bamboo plants among commercial trees"))
df2
df2$Z <- factor(df2$Z, levels = c("Farmers grew commercial trees", "Fruit trees among commercial trees", "Timber trees among commercial trees", "Bamboo plants among commercial trees"))

dodge_width <- 0.7  # Adjust bar separation

fig2 <- ggplot(df2, aes(x = Z, y = Y, fill = X)) + 
  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = 0.6) +  # Reduce bar width slightly
  geom_errorbar(aes(ymin = Y - 1.96 * Se, ymax = Y + 1.96 * Se), 
                width = 0.2, linewidth = 1, position = position_dodge(width = dodge_width), alpha = 0.5) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(y = "Mean fraction") +
  theme_bw() +
  guides(fill = guide_legend(title = NULL)) + # Remove legend title
  theme(
    axis.title.x = element_blank(),
    text = element_text(family = "sans", size = 8), # Base font settings
    axis.title.y = element_text(size = 8, margin = margin(r = 5)), # Y-axis title
    axis.title.y.right = element_text(size = 8, margin = margin(l = 5)), # Right Y-axis title
    axis.text = element_text(size = 7, color = "black"), # Axis tick labels
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1), # X-axis text positioning
    panel.grid.major = element_line(linewidth = 0.25), # Thinner grid lines
    panel.grid.minor = element_blank() # No minor grid lines
  )

fig2

ggarrange(fig1, fig2,
          labels = c("A", "B"),
          nrow = 2, ncol = 1)
ggsave("Fig. 1: Tree growing outcomes by land holding size: Raw data.png", height = 10, width = 12)


# Figure 2: Tree growing outcomes by exposure to agroforestry programs
## Note: Different scale for Y axia (tot tree, tree density)
df3<- data.frame(X=c("Not exposed","Knew of program", 
                     "Participated in program",
                     "Not exposed","Knew of program", 
                     "Participated in program"),
                 Y=c(4, 8, 28, 0.7, 1, 4.13),
                 LL=c(4, 4, 15, 0.58, 0.74, 2),
                 UL=c(5, 11, 62, 0.88, 1.82, 6.87), 
                 Z= c("Number of commercial trees grown", "Number of commercial trees grown","Number of commercial trees grown", 
                      "Commercial trees grown per acre", "Commercial trees grown per acre", "Commercial trees grown per acre"))
df3
df3$Z <- factor(df3$Z, levels = c("Number of commercial trees grown", "Commercial trees grown per acre"))
df3$X <- factor(df3$X, levels = c("Not exposed", "Knew of program", "Participated in program"))
dodge_width <- 0.7  # Adjust for proper bar separation

fig3 <- ggplot(df3, aes(x = Z, fill = X)) + 
  geom_bar(aes(y = ifelse(Z == "Number of commercial trees grown", Y, Y * 10)),  
           stat = "identity", position = position_dodge(width = dodge_width), width = 0.6) +  # Reduce bar width
  geom_errorbar(aes(ymin = ifelse(Z == "Number of commercial trees grown", LL, LL * 10), 
                    ymax = ifelse(Z == "Number of commercial trees grown", UL, UL * 10)), 
                width = 0.2, size = 1, position = position_dodge(width = dodge_width), alpha = 0.5) +
  scale_fill_manual(values = c("Not exposed" = "light green", 
                               "Knew of program" = "red", 
                               "Participated in program" = "blue")) +
  scale_y_continuous(name = "Median value", 
                     sec.axis = sec_axis(~./10, name = "Median value")) +
  theme_bw() +
  guides(fill = "none") +  # Remove the legend
  theme(
    axis.title.x = element_blank(),
    text = element_text(family = "sans", size = 8), # Base font settings
    axis.title.y = element_text(size = 8, margin = margin(r = 5)), # Y-axis title
    axis.title.y.right = element_text(size = 8, margin = margin(l = 5)), # Right Y-axis title
    axis.text = element_text(size = 7, color = "black"), # Axis tick labels
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1), # X-axis text positioning
    panel.grid.major = element_line(linewidth = 0.25), # Thinner grid lines
    panel.grid.minor = element_blank() # No minor grid lines
  )


fig3





df4<- data.frame(X=factor(c("Not exposed","Knew of program", 
                            "Participated in program",
                            "Not exposed","Knew of program", 
                            "Participated in program",
                            "Not exposed","Knew of program", 
                            "Participated in program",
                            "Not exposed","Knew of program", 
                            "Participated in program"),
                          levels = c("Not exposed","Knew of program", "Participated in program")),
                 Y=c(0.69, 0.74, 0.83, 0.66, 0.65, 0.48, 0.12, 0.13, 0.32, 0.20,
                     0.22, 0.19),
                 Se=c(0.01, 0.03, 0.04, 0.02, 0.04, 0.05, 0.01, 0.02, 0.04, 0.01, 0.03, 0.04),
                 Z= c("Farmers grew commercial trees", "Farmers grew commercial trees", "Farmers grew commercial trees",
                      "Fruit trees among commercial trees", "Fruit trees among commercial trees",
                      "Fruit trees among commercial trees",
                      "Timber trees among commercial trees", "Timber trees among commercial trees", 
                      "Timber trees among commercial trees", 
                      "Bamboo plants among commercial trees", "Bamboo plants among commercial trees", 
                      "Bamboo plants among commercial trees"))

df4

df4$Z <- factor(df4$Z, levels = c("Farmers grew commercial trees", "Fruit trees among commercial trees", "Timber trees among commercial trees", "Bamboo plants among commercial trees"))



dodge_width <- 0.7  # Adjust for proper spacing


fig4 <- ggplot(df4, aes(x = Z, y = Y, fill = X)) + 
  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = 0.6) +  # Reduce bar width slightly
  geom_errorbar(aes(ymin = Y - 1.96 * Se, ymax = Y + 1.96 * Se), 
                width = 0.2, size = 1, position = position_dodge(width = dodge_width), alpha = 0.5) +
  scale_fill_manual(values = c("Not exposed" = "light green", 
                               "Knew of program" = "red", 
                               "Participated in program" = "blue")) +
  labs(y = "Mean fraction") +
  theme_bw() +
  guides(fill = guide_legend(title = NULL)) + # Remove legend title
  theme(
    axis.title.x = element_blank(),
    text = element_text(family = "sans", size = 8), # Base font settings
    axis.title.y = element_text(size = 8, margin = margin(r = 5)), # Y-axis title
    axis.title.y.right = element_text(size = 8, margin = margin(l = 5)), # Right Y-axis title
    axis.text = element_text(size = 7, color = "black"), # Axis tick labels
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1), # X-axis text positioning
    panel.grid.major = element_line(linewidth = 0.25), # Thinner grid lines
    panel.grid.minor = element_blank() # No minor grid lines
  )


fig4

ggarrange(fig3, fig4,
          labels = c("A", "B"),
          nrow = 2, ncol = 1)
ggsave("Fig. 2: Tree growing outcomes by exposure to agroforestry programs: Raw data.png", height = 10, width = 12)


##Fig 3 IPWRA results: causal effects of exposure to agroforestry programs on tree growing outcomes



df5<- data.frame(Y=c("Knew of program",
                     "Knew of program",
                     "Knew of program",
                     "Knew of program",
                     "Knew of program",
                     "Knew of program",
                     "Participated in program",
                     "Participated in program",
                     "Participated in program",
                     "Participated in program",
                     "Participated in program",
                     "Participated in program"),
                 ATET=c(7.24, 28.57, 10.79, -13.33,
                        4.76, 0, 18.84, 157.14,
                        95.81, 100, -4.76, -20.31),
                 LL=c(-43.48, -100, -83.37, -46.66, 
                      -33.33, -140.62, 2.89, 0,
                      14.98, 46.66, -47.62, -39.06),
                 UL=c(18.84, 142.85, 105.06, 26.66,
                      42.85, 140.62, 33.33, 314.29,
                      176.65, 153.33, 33.33, -3.12),
                 Z= c("Farmers grew commercial trees", "Number of commercial trees grown", 
                      "Commercial trees grown per acre", "Timber trees among commercial trees", 
                      "Bamboo plants among commercial trees", "Fruit trees among commercial trees",
                      "Farmers grew commercial trees", "Number of commercial trees grown", 
                      "Commercial trees grown per acre", "Timber trees among commercial trees", 
                      "Bamboo plants among commercial trees", "Fruit trees among commercial trees"))




df5$Z <- factor(df5$Z, levels = c("Fruit trees among commercial trees", "Bamboo plants among commercial trees",
                                  "Farmers grew commercial trees", "Commercial trees grown per acre",
                                  "Timber trees among commercial trees", "Number of commercial trees grown" ))

## df5$Z <- factor(df5$Z, levels = df5$Z[df5$Y == "Knew of program"])
df5$y_position <- as.numeric(df5$Z) + ifelse(df5$Y == "Participated in program", 0.2, -0.2)

df5
## Make vertical distance between 2 treatment lines
## Sort the lines (min to max ) by T1- Heard about Govt. support in agroforestry]


fig5 <- ggplot(df5, aes(x = ATET, y = y_position, color = Y)) +
  geom_point() +
  geom_errorbar(aes(xmin = LL, xmax = UL), 
                width = 0.25, linewidth = 1, 
                position = position_dodge(width = 0.5), alpha = 0.3) +
  geom_text(aes(label = sprintf("%.2f", ATET)),  # Keep original ATET values inside the figure
            position = position_dodge(width = 0.5), vjust = ifelse(df5$Y == "Participated in program", -0.5, 1.5)) +
  geom_vline(xintercept = 0, linetype = "solid", size = 0.5) +
  labs(y = "Tree growing outcomes", color = "") +  # Empty string removes the legend title
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(breaks = 1:length(levels(df5$Z)), labels = levels(df5$Z)) +
  theme_bw() +
  scale_color_manual(values = c("Knew of program" = "red", 
                                "Participated in program" = "blue")) +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),
    text = element_text(family = "sans", size = 8),  # Base font settings
    axis.title.y = element_text(size = 8, margin = margin(r = 5)),  # Y-axis title
    axis.text = element_text(size = 7, color = "black"),  # Axis tick labels
    axis.text.y = element_text(hjust = 1),  # Right-align y-axis text
    legend.text = element_text(size = 7),  # Legend text size
    panel.grid.major = element_line(linewidth = 0.25),  # Thinner grid lines
    panel.grid.minor = element_blank(),  # No minor grid lines
    legend.margin = margin(t = -5, unit = "pt")  # Reduce space above legend
  )


fig5

ggsave("Fig. 3: Inverse probability-weighted regression adjustment (IPWRA) results.png", height = 10, width = 12)


#SI Figures

## Figure S1. Percentage of households exposed to government-supported agroforestry programs in surveyed villages

## Data file consists of village, percentage of hh heard and participated in agroforestry program
agftreat<- read_dta("Data_Agroforestry exposure by village.dta")
names(agftreat)


  

## note: pct_heard_SMAFF2 is % of household in each village is exposed to agroforestry program 
# 1. Round to 0 decimals, count frequencies, and sort
freq_data <- agftreat %>%
  mutate(
    pct_heard_SMAFF2 = round(pct_heard_SMAFF2, 0)  # Round to 0 decimals
  ) %>%
  count(pct_heard_SMAFF2) %>%
  arrange(desc(n))  # Sort by frequency (descending)



# 2. Plot with formatted x-axis (add % sign)

fig6 <- ggplot(freq_data, aes(x = reorder(pct_heard_SMAFF2, -n), y = n)) +
  geom_bar(stat = "identity", color = "white", fill = "blue") +
  scale_x_discrete(
    labels = function(x) paste0(x, "%")  # Add % sign to labels
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::number_format(accuracy = 1)) +  # Ensure y-axis has whole numbers
  labs(x = "Farmers exposed to agroforestry support", y = "Frequency of number of villages") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate labels for readability
  )
fig6

ggsave("Supplementary Fig 1.png", height = 5, width = 7.5)

## Figure S3. Planting dates for commercial trees planted by farmers  

treeage <- read_dta("Data_Tree Planting Year.dta")
names(treeage)

fig7<- ggplot(treeage, aes(x= treeplnt_year_1)) +
  geom_histogram(bins = 100, color = "white", fill = "blue") +
  labs(x= "Year of planting",
       y= "Frequency") +
  theme_bw()

fig7

ggsave("Supplementary Fig 3.png", height = 5, width = 7.5)




# COVEL for Treatment 1 (Knew of Program)

matching<- read_dta("Data_Variables for COVSEL Treatment 1.dta")

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
  "dummy_irrigation_GW"
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



ans3 <- cov.sel(T = matching$heard_SMAFF2 , Y = matching$per_acre_tree, 
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


# COVEL for Treatment 2 (Participated in Program)

matching<- read_dta("Data_Variables for COVSEL Treatment 2.dta")

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
  "dummy_irrigation_GW"
)


# Create the matrix
my_matrix <- as.matrix(matching[, selected_vars])

# View the first few rows of the matrix
head(my_matrix)

ans1_b <- cov.sel(T = matching$partcp_SMAFF2, Y = matching$tree_grow_dummy, 
                  X = my_matrix, type = "np",  alg = 3, 
                  scope = NULL, alpha = 0.3, trace = 0,  
                  thru=0.5, thro=0.25, thrc=100, method = "save", 
                  na.action = "na.omit")
print(ans1_b)
summary(ans1_b)

ans2_b <- cov.sel(T = matching$partcp_SMAFF2, Y = matching$tot_tree, 
                  X = my_matrix, type = "np",  alg = 3, 
                  scope = NULL, alpha = 0.3, trace = 0,  
                  thru=0.5, thro=0.25, thrc=100, method = "save", 
                  na.action = "na.omit")
print(ans2_b)
summary(ans2_b)



ans3_b <- cov.sel(T = matching$partcp_SMAFF2, Y = matching$per_acre_tree, 
                  X = my_matrix, type = "np",  alg = 3, 
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

## End of R script




