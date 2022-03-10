####=============================================================####
# A.L.R.R. 11/DEC/2019
# Modified on April 6, 2021
# Modified on March 10, 2022
# This script shows the correlation plots between MS and
# ...imaging variables


#### RELEVANT PACKAGES ==========================================####
# install.packages("pacman")
# require(pacman)
pacman::p_load(dplyr, ggplot2, ggthemes, ggvis, plotly,
               rio, stringr, tidyr, ggpubr, psych, pastecs,
               Hmisc, ggExtra, corrplot, lmSupport, lmtest,
               car, gtsummary, gt, webshot, funModeling, leaps,
               RColorBrewer, FactoMineR, factoextra, gmodels)


#### SET DIRECTORY ==============================================####
# Working directory
setwd(paste('/Users/lmuresearchfellowship/Documents/Adriana/',
            'LMU_Psychology/Projects/MS/Docs', sep = ""))


#### OBTAINING DATA =============================================####
# Read the behavioral data from a file
DB_behav <- read.csv("Demographics_all.csv")

# Read the imaging data from a file (.txt)
  # This was created in a .sh script based on FSL measures
DB_imag_global <- read.table(file = "Z_average_FC_global.txt",
                             header = T) # global

# Merge the two data frames column-wise (excluding the...
# ...first, "participant" column from Z_average:
DB <- cbind(DB_behav, DB_imag_global[, -1])

# Check variable types
str(DB)
attach(DB)

# Variables that should be converted to 'factor' class
temp <- c("patnr_C", "DR_code", "Group", "sex_E", "MS_dx",
          "Curr_MS_medication_yes_no", "Curr_MS_medic_type",
          "participants")
DB[, colnames(DB) %in% temp] <-
  lapply(DB[, colnames(DB) %in% temp],
         as.factor)
rm(temp)


#### CHECK THE LINEARITY OF THE DATA ============================####
# Relationship with each of the original vbles (of interest)
pairs(DB[, c(4:5, 28:length(DB))])


#### LINEAR REGRESSION MODEL FATIGUE ============================####

# Regression model with all predictors on Fatigue
LMfullmodel_fatigue <- lm(DB$mfis_N ~
                            DB$IC24_BG +
                            DB$IC12_SMN1 +
                            DB$IC7_SMN2 +
                            DB$IC11_ADMN +
                            DB$IC6_PDMN +
                            DB$IC8_LFPN +
                            DB$IC5_RFPN +
                            DB$TotalScore +
                            DB$sumDAskala +
                            DB$TLV +
                            DB$edssges_N +
                            DB$age_N +
                            DB$sex_E +
                            DB$schule_E +
                            DB$Realign_meanFD_Jenkinson)
summary(LMfullmodel_fatigue)

# Save desired output of linear regression
sink("full_lm_on_fatigue.txt")
print(summary(LMfullmodel_fatigue))
sink()

# Confidence intervals
confint(LMfullmodel_fatigue)

# Residuals for plotting against IC7_SMN2
# MFIS
LMmodel_mfis <- lm(DB$mfis_N ~ DB$IC24_BG +
                     DB$IC12_SMN1 + DB$IC11_ADMN +
                     DB$IC6_PDMN + DB$IC8_LFPN +
                     DB$IC5_RFPN + DB$age_N + DB$TotalScore +
                     DB$sumDAskala + DB$TLV + DB$edssges_N +
                     DB$schule_E + DB$sex_E +
                     DB$Realign_meanFD_Jenkinson)
summary(LMmodel_mfis)

# FC
LMmodel_glob <- lm(DB$IC7_SMN2 ~ DB$age_N + DB$TotalScore +
                     DB$IC24_BG + DB$IC12_SMN1 +
                     DB$IC11_ADMN + DB$IC6_PDMN +
                     DB$IC8_LFPN + DB$IC5_RFPN +
                     DB$sumDAskala + DB$TLV + DB$edssges_N +
                     DB$schule_E + DB$sex_E +
                     DB$Realign_meanFD_Jenkinson)
summary(LMmodel_glob)

# Save residuals
DB$res_mfis <- residuals.lm(LMmodel_mfis)
DB$res_glob_IC7 <- residuals.lm(LMmodel_glob)

# Check whether the model suffers from multicollinearity
# ...with the "variance inflation factor"
# Multicollinearity is an issue when VIF > 10
1/(1-summary(LMmodel_mfis)$r.squared)

# Residual diagnostics - Validation of regression assumptions
# Studentized residuals
studentized <- rstudent(LMfullmodel_fatigue)
# Normality (Shapiro Wilk test)
shapiro.test(studentized)

# Homocedasticity (Breusch-Pagan Test; lmtest package)
bptest(LMfullmodel_fatigue)

# Independence (no autocorrelation) Durbin-Watson test
dwtest(LMfullmodel_fatigue)

# Residuals vs. Estimated response
plot(fitted.values(LMfullmodel_fatigue), studentized,
     ylim = c(-3.2, 3.2), pch = 16)
abline(0, 0, lty = 2)
lines(lowess(fitted.values(LMfullmodel_fatigue),
             studentized), col = "red")
text(fitted.values(LMfullmodel_fatigue),
     studentized,
     labels = row.names(DB), cex = 0.7, pos = 3)

# Influential Observations (from car package)
# Cook's D plot
# Influential Observations (from car package)
# Cook's D plot
plot(LMfullmodel_fatigue, which = 5,
     cook.levels = mean(cooks.distance(
       LMfullmodel_fatigue)) * 3)

# Influence Plot
influencePlot(LMfullmodel_fatigue,
              main = "Influence Plot",
              sub = "Circle size is proportial to Cook's Distance")


#### LINEAR REGRESSION MODEL SLEEP ==============================####

# Specify model
LMfullmodel_sleep <- lm(DB$TotalScore ~
                          DB$IC24_BG +
                          DB$IC12_SMN1 +
                          DB$IC7_SMN2 +
                          DB$IC11_ADMN +
                          DB$IC6_PDMN +
                          DB$IC8_LFPN +
                          DB$IC5_RFPN +
                          DB$mfis_N +
                          DB$sumDAskala +
                          DB$TLV +
                          DB$edssges_N +
                          DB$age_N +
                          DB$sex_E +
                          DB$schule_E +
                          DB$Realign_meanFD_Jenkinson)
summary(LMfullmodel_sleep)

# Save desired output of linear regression
sink("full_lm_on_sleep.txt")
print(summary(LMfullmodel_sleep))
sink()

# Confidence intervals
confint(LMfullmodel_sleep)

# Residuals for plotting
# PSQI
LMmodel_psqi <- lm(DB$TotalScore ~ DB$age_N + DB$mfis_N +
                     DB$IC24_BG + DB$IC12_SMN1 +
                     DB$IC7_SMN2 + DB$IC11_ADMN +
                     DB$IC6_PDMN + DB$IC5_RFPN +
                     DB$sumDAskala + DB$TLV + DB$edssges_N +
                     DB$schule_E + DB$sex_E +
                     DB$Realign_meanFD_Jenkinson)
summary(LMmodel_psqi)

# FC
LMmodel_glob <- lm(DB$IC8_LFPN ~ DB$age_N + DB$mfis_N +
                     DB$IC24_BG + DB$IC12_SMN1 +
                     DB$IC7_SMN2 + DB$IC11_ADMN +
                     DB$IC6_PDMN + DB$IC5_RFPN +
                     DB$sumDAskala + DB$TLV + DB$edssges_N +
                     DB$schule_E + DB$sex_E +
                     DB$Realign_meanFD_Jenkinson)
summary(LMmodel_glob)

# Save residuals
DB$res_psqi <- residuals.lm(LMmodel_psqi)
DB$res_glob_IC8 <- residuals.lm(LMmodel_glob)

# Check whether the model suffers from multicollinearity
# ...with the "variance inflation factor"
# Multicollinearity is an issue when VIF > 10
1/(1-summary(LMmodel_psqi)$r.squared)

# Residual diagnostics -
# Validation of regression assumptions
# Studentized residuals
studentized <- rstudent(LMfullmodel_sleep)
# Normality (Shapito Wilk test)
shapiro.test(studentized)

# Homocedasticity (Breusch-Pagan Test; lmtest package)
bptest(LMfullmodel_sleep)

# Independence (no autocorrelation) Durbin-Watson test
dwtest(LMfullmodel_sleep)

# Residuals vs. Estimated response
plot(fitted.values(LMfullmodel_sleep), studentized,
     ylim = c(-3.2, 3.2), pch = 16)
abline(0, 0, lty = 2)
lines(lowess(fitted.values(LMfullmodel_sleep),
             studentized), col = "red")
text(fitted.values(LMfullmodel_sleep),
     studentized,
     labels = row.names(DB), cex = 0.7, pos = 3)

# Influential Observations (from car package)
# Cook's D plot
plot(LMfullmodel_sleep, which = 5,
     cook.levels = mean(cooks.distance(
       LMfullmodel_sleep)) * 3)

# Influence Plot
influencePlot(LMfullmodel_sleep,
              main = "Influence Plot",
              sub = "Circle size is proportial to Cook's Distance")


#### SCATTER PLOT ====================================================####
# GLOBAL (AVERAGE) FC AND FATIGUE

# Scatter plot with the residuals of the regression (MFIS)
p <- ggscatter(DB, x = "res_glob_IC7",
               y = "res_mfis",
               alpha = 0.6,
               rug = F,
               add = "reg.line",
               shape = 20, size = 4,
               conf.int = T,
               add.params = list(fill = "gray80"),
               xlab = "Average FC SMN-II (residual)",
               ylab = "Fatigue [MFIS] (residual)"
# ) + stat_cor(label.x = -2, label.y = 35,
#              size = 5, cor.coef.name = "r")
# If R2 is instead wanted on the plot, uncomment below:
) + stat_cor(aes(label = paste(..rr.label..,
                               ..p.label.., sep = "~`,`~")),
             label.x = -2, label.y = 35, size = 5)
p <- ggpar(p, xlim = c(-2, 2), ylim= c(-35, 35),
           font.x = c(18, "plain"),
           font.y = c(18, "plain"),
           font.tickslab = 16,
           font.legend = 18)
q <- ggMarginal(p, type = "density", fill = "lightgray")
q
ggsave("./Results/mfis_SMN2_res.jpg",
       plot = q,
       dpi = 500, width = 20,
       height = 15, units = "cm")

# Scatter plot raw values (if needed/wanted)
q <- ggscatter(DB, x = "IC7_SMN2",
               y = "mfis_N",
               combine = F,
               merge = F,
               shape = 20, size = 3,
               add = "reg.line", conf.int = T, 
               cor.coef = F, cor.method = "pearson",
               xlab = "FC SMN-II average",
               ylab = "Fatigue MFIS")
q + grids(axis = "xy", linetype = "solid")


#### SCATTER PLOT ===============================================####
# GLOBAL (AVERAGE) FC AND SLEEP

# Scatter plot with the residuals of the regression
o <- ggscatter(DB, x = "res_glob_IC8",
               y = "res_psqi",
               alpha = 0.6,
               rug = F,
               add = "reg.line",
               add.params = list(fill = "gray80"),
               conf.int = T,
               shape = 20, size = 4,
               xlab = "Average FC LFPN (residual)",
               ylab = "Sleep quality [PSQI] (residual)"
# ) + stat_cor(label.x = -2, label.y = 8,
#              size = 5, cor.coef.name = "r")
# If R2 is instead wanted on the plot, uncomment below:
) + stat_cor(aes(label = paste(..rr.label..,
..p.label.., sep = "~`,`~")),
 label.x = -2, label.y = 8, size = 5)
o <- ggpar(o, xlim = c(-2, 2.2), ylim= c(-8, 8),
           font.x = c(18, "plain"),
           font.y = c(18, "plain"),
           font.tickslab = 16,
           font.legend = 18)
o <- ggMarginal(o, type = "density", fill = "lightgray")
o
ggsave("./Results/sleep_LFPN_res.jpg",
       plot = o,
       dpi = 500, width = 20,
       height = 15, units = "cm")

# Scatter plot raw values (if needed/wanted)
q <- ggscatter(DB, x = "IC8_LFPN",
               y = "TotalScore",
               combine = F,
               merge = F,
               shape = 20, size = 3,
               add = "reg.line", conf.int = T, 
               cor.coef = F, cor.method = "pearson",
               xlab = "FC LFPN average",
               ylab = "Sleep quality PSQI")
q + grids(axis = "xy", linetype = "solid")


#### EFFECT SIZES ===============================================####
# EFFECT SIZES AND INDIVIDUAL R SQUARED VALUES
# ...(Calculates unique SSRs, SSE, SST.
# ...Based on sums of squares, it calculates partial eta2
# ...and delta R2
# ...for all effects in a linear model object.)

ES_LMfullmodel_fatigue <- modelEffectSizes(LMfullmodel_fatigue)
ES_LMfullmodel_sleep <- modelEffectSizes(LMfullmodel_sleep)


#### REGRESSION TABLE FATIGUE ===================================####

LMfullmodel_fatigue %>%
  tbl_regression(label = list('DB$IC24_BG' ~ "FC BGN",
                              'DB$IC12_SMN1' ~ "FC SMN-I",
                              'DB$IC7_SMN2' ~ "FC SMN-II",
                              'DB$IC11_ADMN' ~ "FC ADMN",
                              'DB$IC6_PDMN' ~ "FC PDMN",
                              'DB$IC8_LFPN' ~ "FC LFPN",
                              'DB$IC5_RFPN' ~ "FC RFPN",
                              'DB$age_N' ~ "Age",
                              'DB$TotalScore' ~ "Sleep quality",
                              'DB$sumDAskala' ~ "Depression/Anxiety",
                              'DB$TLV' ~ "Total lesion volume",
                              'DB$edssges_N' ~ "Disability status",
                              'DB$schule_E' ~ "Education level",
                              'DB$sex_E' ~ "Sex",
                              'DB$Realign_meanFD_Jenkinson' ~ "Head motion")
                 ) %>%
  modify_header(label = "**Predictor variable**") %>%
  bold_p() %>%
  modify_column_unhide(column = std.error) %>%
  as_gt() %>% opt_table_font(font = "Arial") %>%
  tab_options(table_body.hlines.width = 0,
              column_labels.border.top.width = 2,
              column_labels.border.top.color = "black",
              column_labels.border.bottom.width = 2,
              column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table.border.bottom.color = "white",
              table.font.size = px(14)) %>%
  gt::gtsave(filename = "fatigue_model_image.html")


#### REGRESSION TABLE SLEEP =====================================####

LMfullmodel_sleep %>%
  tbl_regression(label = list('DB$IC24_BG' ~ "FC BGN",
                              'DB$IC12_SMN1' ~ "FC SMN-I",
                              'DB$IC7_SMN2' ~ "FC SMN-II",
                              'DB$IC11_ADMN' ~ "FC ADMN",
                              'DB$IC6_PDMN' ~ "FC PDMN",
                              'DB$IC8_LFPN' ~ "FC LFPN",
                              'DB$IC5_RFPN' ~ "FC RFPN",
                              'DB$age_N' ~ "Age",
                              'DB$mfis_N' ~ "Fatigue",
                              'DB$sumDAskala' ~ "Depression/Anxiety",
                              'DB$TLV' ~ "Total lesion volume",
                              'DB$edssges_N' ~ "Disability status",
                              'DB$schule_E' ~ "Education level",
                              'DB$sex_E' ~ "Sex",
                              'DB$Realign_meanFD_Jenkinson' ~ "Head motion")
  ) %>%
  modify_header(label = "**Predictor variable**") %>%
  bold_p() %>%
  modify_column_unhide(column = std.error) %>%
  as_gt() %>% opt_table_font(font = "Arial") %>%
  tab_options(table_body.hlines.width = 0,
              column_labels.border.top.width = 2,
              column_labels.border.top.color = "black",
              column_labels.border.bottom.width = 2,
              column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table.border.bottom.color = "white",
              table.font.size = px(14)) %>%
  gtsave(filename = "sleep_model_image.html")


#### SUBSCORES OF THE PSQI ======================================####

# Prepare data
PSQI_sub <- read.csv("CyCOG_PSQI.csv")

# Adjust non-numeric value
PSQI_sub$Sleep_latency_min[which(
  PSQI_sub$Sleep_latency_min
  =='keine Angaben im Fragebogen')] <- ""

# Check variable types
str(PSQI_sub)

# Adjust variable types (non-numeric)
PSQI_sub$Sleep_latency_min <-
  as.numeric(PSQI_sub$Sleep_latency_min)
PSQI_sub$patnr_C <- as.factor(PSQI_sub$patnr_C)

# Calculate regression model on FATIGUE (commented: if
# those variables are wanted instead of the corresponding
# ones (in the case of PSQI scores) or in addition to the
# ones already included (in the case of the demographic 
# variables))
LMSleepModel_fatigue <- lm(DB$mfis_N ~
                             PSQI_sub$Sleep_latency_min +
                             # PSQI_sub$Sleep_latency +
                             PSQI_sub$sleep_quality +
                             PSQI_sub$Sleep_duration +
                             PSQI_sub$sleep_efficiency_perc +
                             # PSQI_sub$sleep_efficiency +
                             PSQI_sub$sleep_disturbances +
                             PSQI_sub$sleep_Medication +
                             PSQI_sub$daytime_sleepiness) #+
                             # DB$sumDAskala +
                             # DB$TLV +
                             # DB$edssges_N +
                             # DB$schule_E +
                             # DB$sex_E)
summary(LMSleepModel_fatigue)

# Find the best sleep model to predict fatigue:
best_subset <- regsubsets(mfis_N ~ PSQI_sub$Sleep_latency_min +
                            PSQI_sub$sleep_quality +
                            PSQI_sub$Sleep_duration +
                            PSQI_sub$sleep_efficiency_perc +
                            PSQI_sub$sleep_disturbances +
                            PSQI_sub$sleep_Medication +
                            PSQI_sub$daytime_sleepiness +
                            DB$IC7_SMN2 +
                            DB$IC6_PDMN +
                            DB$IC12_SMN1 +
                            DB$IC8_LFPN,
                          DB,
                          method = "exhaustive")
results <- summary(best_subset)

# Seeing the results graphically
display.brewer.all(n = NULL, type = "all",
                   select = NULL,
                   colorblindFriendly = T)
colorscale <- brewer.pal(n = 8, name = "BrBG")
greens <- c("#01665E", "#35978F", "#80CDC1", "#C7EAE5")
par(mfrow = c(2, 2))

# Save results as a graph
# BIC
bmp("bic.bmp", width = 2200, height = 1500,
    res = 300)
plot(best_subset, scale = "bic", col = greens,
     labels = c("Intercept", "Latency", "Quality", "Duration",
                "Efficiency", "Disturbances", "Medication",
                "D. Sleepiness"))
dev.off()
# R-squared adjusted
bmp("adjr.bmp", width = 2200, height = 1500,
    res = 300)
plot(best_subset, scale = "adjr2", col = greens,
     labels = c("Intercept", "Latency", "Quality",
                "Duration", "Efficiency", "Disturbances",
                "Medication", "D. Sleepiness"))
dev.off()
# Mallows's Cp
bmp("Cp.bmp", width = 2200, height = 1500,
    res = 300)
plot(best_subset, scale = "Cp", col = greens,
     labels = c("Intercept", "Latency", "Quality", "Duration",
                "Efficiency", "Disturbances", "Medication",
                "D. Sleepiness"))
dev.off()

# Best model regression (commented: included in the original
# model)
LMSleepModel_fatigue_best <- lm(scale(DB$mfis_N) ~
                             # scale(PSQI_sub$Sleep_latency_min) +
                             # PSQI_sub$Sleep_latency +
                             # PSQI_sub$sleep_quality +
                             # PSQI_sub$Sleep_duration +
                             # PSQI_sub$sleep_efficiency_perc +
                             # PSQI_sub$sleep_efficiency +
                             scale(PSQI_sub$sleep_disturbances) +
                             # PSQI_sub$sleep_Medication +
                             scale(PSQI_sub$daytime_sleepiness) +
                             scale(DB$IC7_SMN2))
summary(LMSleepModel_fatigue_best)

# Table of best model (for report in the manuscript)
LMSleepModel_fatigue_best %>%
  tbl_regression(label =
                   list(#'scale(PSQI_sub$Sleep_latency_min)' ~
                        #  "Sleep latency (min)",
                   'scale(PSQI_sub$sleep_disturbances)' ~
                     "Sleep disturbances",
                        'scale(PSQI_sub$daytime_sleepiness)' ~
                     "Daytime sleepiness",
                   'scale(DB$IC7_SMN2)' ~ "FC SMN-II")
  ) %>%
  modify_header(label = "**Predictor variable**") %>%
  bold_p() %>%
  modify_column_unhide(column = std.error) %>%
  as_gt() %>% opt_table_font(font = "Arial") %>%
  tab_options(table_body.hlines.width = 0,
              column_labels.border.top.width = 2,
              column_labels.border.top.color = "black",
              column_labels.border.bottom.width = 2,
              column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table.border.bottom.color = "white",
              table.font.size = px(14)) %>%
  gtsave(filename = "fatigue_bestmodel_image.html")

# Calculate regression model on LFPN-FC
LMSleepModel_LFPN <- lm(DB$IC8_LFPN ~
                             PSQI_sub$Sleep_latency_min +
                             # PSQI_sub$Sleep_latency +
                             PSQI_sub$sleep_quality +
                             PSQI_sub$Sleep_duration +
                             PSQI_sub$sleep_efficiency_perc +
                             # PSQI_sub$sleep_efficiency +
                             PSQI_sub$sleep_disturbances +
                             PSQI_sub$sleep_Medication +
                             PSQI_sub$daytime_sleepiness)# +
                             # DB$sumDAskala +
                             # DB$TLV +
                             # DB$edssges_N +
                             # DB$schule_E +
                             # DB$sex_E)
summary(LMSleepModel_LFPN)

# Save desired output of linear regression
sink("lm_LFPN.txt")
print(summary(LMSleepModel_LFPN))
sink()

# Find the best sleep model to predict LFPN FC:
best_subset <- regsubsets(DB$IC8_LFPN ~ PSQI_sub$Sleep_latency_min +
                            PSQI_sub$sleep_quality +
                            PSQI_sub$Sleep_duration +
                            PSQI_sub$sleep_efficiency_perc +
                            PSQI_sub$sleep_disturbances +
                            PSQI_sub$sleep_Medication +
                            PSQI_sub$daytime_sleepiness,
                          DB,
                          method = "exhaustive")
results <- summary(best_subset)

# Calculate FC regression model on efficiency (PSQI-insomnia)
LMfullmodel_efficiency <- lm(PSQI_sub$sleep_efficiency_perc ~
                               DB$IC24_BG +
                               DB$IC12_SMN1 +
                               DB$IC7_SMN2 +
                               DB$IC11_ADMN +
                               DB$IC6_PDMN +
                               DB$IC8_LFPN +
                               DB$IC5_RFPN +
                               DB$sumDAskala +
                               DB$TLV +
                               DB$edssges_N +
                               DB$age_N +
                               DB$sex_E +
                               DB$schule_E +
                               DB$Realign_meanFD_Jenkinson)
summary(LMfullmodel_efficiency)

# Calculate FC regression model on latency (PSQI-insomnia)
LMfullmodel_latency <- lm(PSQI_sub$Sleep_latency_min ~
                               DB$IC24_BG +
                               DB$IC12_SMN1 +
                               DB$IC7_SMN2 +
                               DB$IC11_ADMN +
                               DB$IC6_PDMN +
                               DB$IC8_LFPN +
                               DB$IC5_RFPN +
                               DB$sumDAskala +
                               DB$TLV +
                               DB$edssges_N +
                               DB$age_N +
                               DB$sex_E +
                               DB$schule_E +
                               DB$Realign_meanFD_Jenkinson)
summary(LMfullmodel_latency)

# Calculate regression model on SMN2-FC
LMSleepModel_SMN2 <- lm(DB$IC7_SMN2 ~
                          PSQI_sub$Sleep_latency_min +
                          # PSQI_sub$Sleep_latency +
                          PSQI_sub$sleep_quality +
                          PSQI_sub$Sleep_duration +
                          PSQI_sub$sleep_efficiency_perc +
                          # PSQI_sub$sleep_efficiency +
                          PSQI_sub$sleep_disturbances +
                          PSQI_sub$sleep_Medication +
                          PSQI_sub$daytime_sleepiness)# +
                          # DB$sumDAskala +
                          # DB$TLV +
                          # DB$edssges_N +
                          # DB$schule_E +
                          # DB$sex_E)
summary(LMSleepModel_SMN2)

# Save desired output of linear regression
sink("lm_SMN2.txt")
print(summary(LMSleepModel_SMN2))
sink()


#### MAKE TABLE PSQI SUBSCORES ==================================####
# Make table of PSQI subscores for report

# Make dataset with variables to summarize
summ_vbles <- PSQI_sub %>%
  select("Sleep efficiency (%)" = sleep_efficiency_perc,
  "Subjective sleep quality" = sleep_quality,
  "Sleep duration" = Sleep_duration,
  "Sleep disturbances" = sleep_disturbances,
  "Sleep medication" = sleep_Medication,
  "Daytime sleepiness" = daytime_sleepiness,
  "Sleep latency (min)" = Sleep_latency_min
) %>%
  tbl_summary(missing = "no",#"ifany",
              type = all_categorical() ~ "continuous",
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_categorical() ~ c(0, 1),
                            all_continuous() ~ 1)) %>% # add_p() %>%
  modify_header(label = "**Variable**") %>%
  #add_overall() %>% #bold_labels() %>%
  as_gt() %>% opt_table_font(font = "Arial") %>%
  tab_options(table_body.hlines.width = 0,
              column_labels.border.top.width = 2,
              column_labels.border.top.color = "black",
              column_labels.border.bottom.width = 2,
              column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table.border.bottom.color = "white",
              table.font.size = px(14))
gtsave(summ_vbles, "PSQI_subscores.html")


#### MAKE FREQUENCY TABLE =======================================####
# Make table of dichotomous fatigue and SQ frequencies for report

# Create dichotomous variables
DB$fatigue <- ""
DB$sleep <- ""
DB$fatigue <- ifelse(DB$mfis_N >= 38, "high", "low")
DB$sleep <- ifelse(DB$TotalScore >= 5, "poor", "better")

# Make cross-table
table(DB$fatigue)
prop.table(table(DB$fatigue))
table(DB$sleep)
prop.table(table(DB$sleep))
crosstable <- CrossTable(DB$fatigue, DB$sleep)


#### ADDITIONAL EXPLORATORY ANALYSES ============================####
# Other exploratory/interesting analyses

# Create quantitative dataframe
table <- cbind(PSQI_sub[,c(2:3,5,7:9,11)], mfis_N = DB[, 4])
table <- cbind(table, DB[, c(28:34)])

# Impute missing value for models to work
table$Sleep_latency_min[is.na(table$Sleep_latency_min)] <-
  mean(table$Sleep_latency_min, na.rm = T)

# Alternatively (better results because of same scale)
table$sleep_efficiency_perc <- PSQI_sub$sleep_efficiency
table$Sleep_latency_min <- PSQI_sub$Sleep_latency

# PCA (leaving out MFIS and FC variables)
pcamodel <- PCA(table, quanti.sup = 8:10)
pcamodel <- PCA(table)

# Check optimum cluster number
fviz_nbclust(table, kmeans, method = "wss")

# Run K-means (k = 2) on individuals
set.seed(123)
kmeansmodel <- kmeans(table, 3, nstart = 25)

# Run K-means on variables
set.seed(123)
res.km <- kmeans(pcamodel$var$coord,
                 centers = 3, nstart = 25)

# Plot results taking into account PCA and K-means
fviz_pca_biplot(pcamodel,
                col.ind = as.factor(kmeansmodel$cluster),
                geom = "point",
                palette = "jco",
                addEllipses = T, label = "var",
                col.var = as.factor(res.km$cluster), repel = T,
                legend.title = "K-means clusters")
ggsave("biplot_pca_kmeans.jpg")


#### CORRELATION BETWEEN TWO CLUSTERS ===========================####
# Correlation between FC values of two clusters (for review)

# Read the information
DB_imag_local <- read.table(file = "Z_average_FC_local_IC8.txt",
                             header = T)

# Bivariate correlation
cor.test(DB_imag_local$IC8_LFPN_fatigue,
         DB_imag_local$IC8_LFPN_sleep)

# To be fair, we need to recreate the two clusters, e.g., with
# partial correlation:
#install.packages("ppcor")
library("ppcor")

# Partial correlation test
# DB_behav was taken instead of DB because some variables were
# converted to factors for the Demographic table in DB.
pcor.test(DB_imag_local$IC8_LFPN_fatigue,
          DB_imag_local$IC8_LFPN_sleep,
          DB_behav[, c("TotalScore", "mfis_N",
                 "sumDAskala", "TLV",
                 "edssges_N", "age_N",
                 "sex_E", "schule_E",
                 "Realign_meanFD_Jenkinson")])
