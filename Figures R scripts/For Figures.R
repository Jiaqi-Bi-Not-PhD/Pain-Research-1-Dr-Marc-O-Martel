library(haven)
library(ggplot2)
library(tidyverse)
library(lme4)
library(lmerTest)
data_temp <- read_sav("Dataset; 2025.01.06.sav")

source("~/Desktop/PhD Biostatistics/R Packages/ggmultilevel/R/extract_model.R")
source("~/Desktop/PhD Biostatistics/R Packages/ggmultilevel/R/plot_glmm.R")
source("~/Desktop/PhD Biostatistics/R Packages/ggmultilevel/R/plot_glmm_interact_contcat.R")
source("~/Desktop/PhD Biostatistics/R Packages/ggmultilevel/R/plot_glmm_interact_contcont.R")

model1 <- lmer(Comp_Lev1_All_CanCravingIndex ~ Comp_Lev1_All_Pain + (Comp_Lev1_All_Pain|ID), data = data_temp)
summary(model1)
plot_model1 <- plot_glmm(model = model1, data = data_temp, predictor = "Comp_Lev1_All_Pain", outcome = "Comp_Lev1_All_CanCravingIndex", grouping_var = "ID",
          family = "gaussian", y_scale = NULL,
          x_limits = NULL, y_limits = NULL,
          x_label = NULL, y_label = NULL,
          plot_title = NULL,
          x_breaks = NULL, y_breaks = NULL,
          x_num_size = 25, y_num_size = 25)
ggsave(plot_model1, dpi=1200, filename = "Figure; Lev1_AllPain vs Lev1_All_CanCravingIndex.tif", bg="white")

model1 <- lmer(Comp_Lev1_All_CanCravingIndex ~ Comp_Lev1_All_NA + (Comp_Lev1_All_NA|ID), data = data_temp)
summary(model1)
plot_model1 <- plot_glmm(model = model1, data = data_temp, predictor = "Comp_Lev1_All_NA", outcome = "Comp_Lev1_All_CanCravingIndex", grouping_var = "ID",
                         family = "gaussian", y_scale = NULL,
                         x_limits = NULL, y_limits = NULL,
                         x_label = NULL, y_label = NULL,
                         plot_title = NULL,
                         x_breaks = NULL, y_breaks = NULL,
                         x_num_size = 25, y_num_size = 25)
ggsave(plot_model1, dpi=1200, filename = "Figure; Comp_Lev1_All_NA vs Lev1_All_CanCravingIndex.tif", bg="white")

model1 <- lmer(Comp_Lev2_AggregDay_CanCravingIndex ~ Comp_Lev2_AggregDay_CanHedonic1 + (Comp_Lev2_AggregDay_CanHedonic1|ID), data = data_temp)
summary(model1)
plot_model1 <- plot_glmm(model = model1, data = data_temp, predictor = "Comp_Lev2_AggregDay_CanHedonic1", outcome = "Comp_Lev2_AggregDay_CanCravingIndex", grouping_var = "ID",
                         family = "gaussian", y_scale = NULL,
                         x_limits = NULL, y_limits = NULL,
                         x_label = NULL, y_label = NULL,
                         plot_title = NULL,
                         x_breaks = NULL, y_breaks = NULL,
                         x_num_size = 25, y_num_size = 25)
ggsave(plot_model1, dpi=1200, filename = "Figure; Comp_Lev2_AggregDay_CanHedonic1 vs Comp_Lev2_AggregDay_CanCravingIndex.tif", bg="white")

model1 <- lmer(Comp_Lev2_AggregDay_CanCravingIndex ~ Comp_Lev2_AggregDay_CanHedonic2 + (Comp_Lev2_AggregDay_CanHedonic2|ID), data = data_temp)
summary(model1)
plot_model1 <- plot_glmm(model = model1, data = data_temp, predictor = "Comp_Lev2_AggregDay_CanHedonic2", outcome = "Comp_Lev2_AggregDay_CanCravingIndex", 
                         grouping_var = "ID",
                         family = "gaussian", y_scale = NULL,
                         x_limits = NULL, y_limits = NULL,
                         x_label = NULL, y_label = NULL,
                         plot_title = NULL,
                         x_breaks = NULL, y_breaks = NULL,
                         x_num_size = 25, y_num_size = 25)
ggsave(plot_model1, dpi=1200, filename = "Figure; Comp_Lev2_AggregDay_CanHedonic2 vs Comp_Lev2_AggregDay_CanCravingIndex.tif", bg="white")

####################################################
source("~/Desktop/PhD Biostatistics/R Packages/ggmultilevel/R/extract_model.R")
source("~/Desktop/PhD Biostatistics/R Packages/ggmultilevel/R/plot_glmm.R")
source("~/Desktop/PhD Biostatistics/R Packages/ggmultilevel/R/plot_glmm_interact_contcat.R")
source("~/Desktop/PhD Biostatistics/R Packages/ggmultilevel/R/plot_glmm_interact_contcont.R")
library(sjPlot)
library(ggeffects)
df_bruneau <- read_sav("Dataset; 2025.01.31.sav")
model1 <- lmer(
  ComP_Lev1_OCS_Ave  ~ cLev2_SleepQ +
  cLev1.3_Pain +
  cLev1.3_PA +
  cLev1.3_NA +
  cLev1.3_PCS +
  cLev1.3_OWI +
  cLev1.3_Log10_Cort + 
    (cLev1.3_Log10_Cort | ID) ,
  data = df_bruneau
)
summary(model1)

plot1 <- plot_glmm(model = model1, data = df_bruneau,
          predictor = "cLev1.3_Log10_Cort", outcome = "ComP_Lev1_OCS_Ave",
          x_num_size = 25, y_num_size = 25, grouping_var = "ID", x_limit = c(-2, 2))
ggsave(plot1, dpi=1200, filename = "Figure V3; RandomSlope cLev1.3_Log10_Cort vs ComP_Lev1_OCS_Ave.tif", bg="white")




df <- ggpredict(model1, terms = c("cLev3_Log10_cortisol"))

plot1 <- ggplot(df, aes(x, predicted)) +
  geom_line(aes(color = "red")) +
  geom_ribbon(aes(ymin  = conf.low, ymax = conf.high, fill = "red"), alpha = 0.15) +
  scale_color_manual(values = "red") +
  scale_fill_manual(values = "red") +
  scale_y_continuous(limits = c(0, 5)) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(x = "cLev3_Log10_cortisol", y = "Raw_Lev1_PANAS_5") +
  theme_minimal() +
  ggtitle("cLev3_Log10_cortisol vs Raw_Lev1_PANAS_5") +
  theme(
    legend.position = "none",
    text = element_text(size = 12),
    plot.title = element_text(
      size = 12,
      hjust = 0.5,
      margin = margin(b = 20)
    ),
    axis.title.x = element_text(
      margin = margin(t = 15)
    ),
    axis.title.y = element_text(
      margin = margin(r = 15)
    ),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5)
  ) 
ggsave(plot1, dpi=1200, filename = "Figure V2; cLev3_Log10_cortisol vs Raw_Lev1_PANAS_5.tif", bg="white")

model1 <- lmer(
  ComP_Lev1_OCS_Ave   ~ 
    cLev2_SleepQ +
  cLev1.3_Pain +
  cLev1.3_PA +
  cLev1.3_NA +
  cLev1.3_PCS +
  cLev1.3_OWI +
  cLev1.3_Log10_Cort +
  cLev1.3_NA : cLev1.3_Log10_Cort +
    (1 | ID) +                       # random intercept for ID
    (1 | ID:Wave_Lev2_Day),          # Random intercept by ID*Wave_Lev2_Day
  data = df_bruneau,                  # Replace with the actual data frame
  REML = FALSE                       # Use ML to match SPSS
)
summary(model1)
df_pred <- ggpredict(
  model1,
  terms = c(
    "cLev1.3_NA  [-6:6]",  
    "cLev1.3_Log10_Cort [mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd]"
  )
)
plot1 <- ggplot(df_pred, aes(x = x, y = predicted)) +
  geom_line(aes(color = group), size = 1.3) +
  coord_cartesian(
    xlim = c(-6, 6),
    ylim = c(0, 100)
  ) +
  # Explicitly set the order of 'breaks' (top to bottom) and give the matching labels
  scale_color_discrete(
    breaks = c("5", "4", "3", "2", "1"),
    labels = c("+2SD", "+1SD", "Mean", "-1SD", "-2SD")
  ) +
  scale_fill_discrete(guide = "none") +
  labs(
    title = "Moderation Plot",
    x = "cLev1.3_NA",
    y = "ComP_Lev1_OCS_Ave",
    color = "cLev1.3_Log10_Cort"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0.5, margin = margin(b = 20)),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5)
  )

ggsave(plot1, dpi=1200, filename = "Figure V3; Moderation cLev1.3_NA vs ComP_Lev1_OCS_Ave by cLev1.3_Log10_Cort.tif", bg="white")


