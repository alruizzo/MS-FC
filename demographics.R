####=========================================================####
# A.L.R.R. March 2021
# Script to create demographic table for MS manuscript


# Relevant packages =============================================####
# Load relevant packages
# install.packages("pacman") # one time step
# require(pacman) # one time step
pacman::p_load(dplyr, gtsummary, gt, webshot, funModeling)


# Set directory =================================================####
setwd(paste('/Users/lmuresearchfellowship/Documents/Adriana/',
            'LMU_Psychology/Projects/MS/Docs', sep = ""))


# Obtain/Read data ==============================================####

# Create data frame
DB <- read.csv("Demographics_all.csv")

# Check data frame structure
str(DB)

# Adjust variable types (mainly to factor)
DB$patnr_C <- factor(DB$patnr_C)
DB$DR_code <- factor(DB$DR_code)
DB$Group <- factor(DB$Group)

# Adjust factor levels for MS DX
DB$MS_dx[which(DB$MS_dx==1)] <- "RRMS"
DB$MS_dx[which(DB$MS_dx==2)] <- "SP-MS"
DB$MS_dx <- factor(DB$MS_dx, levels = c("RRMS", "SP-MS"))

# Adjust levels for sex
DB$sex_E[which(DB$sex_E==1)] <- "Male"
DB$sex_E[which(DB$sex_E==2)] <- "Female"
DB$sex_E <- factor(DB$sex_E, levels = c("Male", "Female"))

# Adjust levels for education (Hauptschule, Realschule, and
# Gymnasium)
DB$schule_E[which(DB$schule_E==2)] <- "Lower secondary level"
DB$schule_E[which(DB$schule_E==3)] <- "Upper secondary level A"
DB$schule_E[which(DB$schule_E==4)] <- "Upper secondary level B"
DB$schule_E <- factor(DB$schule_E,
                      levels = c("Lower secondary level",
                                 "Upper secondary level A",
                                 "Upper secondary level B"))

# Adjust levels for number of relapses last year
DB$Nr_relapses_last_yr[which(
  DB$Nr_relapses_last_yr>=2)] <- "≥2"
DB$Nr_relapses_last_yr[which(is.na(DB$Nr_relapses_last_yr)==T &
                               DB$MS_dx=="SP-MS")] <- "0"
DB$Nr_relapses_last_yr <- factor(DB$Nr_relapses_last_yr,
                                 levels = c("0", "1", "≥2"))

# Adjust current medication info and type
DB$Curr_MS_medication_yes_no[which(
  DB$Curr_MS_medication_yes_no==0)] <- "No"
DB$Curr_MS_medication_yes_no[which(
  DB$Curr_MS_medication_yes_no==1)] <- "Yes"
DB$Curr_MS_medication_yes_no <-
  factor(DB$Curr_MS_medication_yes_no,
         levels = c("Yes", "No"))
  # Frequency table
Medic_y_n <- table(DB$Curr_MS_medication_yes_no)
cbind(freq = Medic_y_n,
      percentage = round((prop.table(Medic_y_n)*100), 2))
prop.table(Medic_y_n)*100
  # Read medication names
medic_type <- read.csv("Medication_codes.csv")
  # Update medication names in data frame
for (i in medic_type$Code){
  DB$Curr_MS_medic_type[
    DB$Curr_MS_medic_type %in% medic_type$Code[i]] <-
    medic_type$Medication[i]
}
DB$Curr_MS_medic_type[which(
  DB$Curr_MS_medic_type==999)] <- "None" 
DB$Curr_MS_medic_type <- factor(DB$Curr_MS_medic_type)
  # Frequency table
Medic_type_table <- table(DB$Curr_MS_medic_type[which(
  # DB$MS_dx=="SP-MS" &
    DB$Curr_MS_medication_yes_no=="Yes")])
freq_table <- data.frame(cbind(freq = Medic_type_table,
      percentage = round((prop.table(Medic_type_table)*100), 2)))

# Adjust EDSS
  # Check how to break into bins (given it's discrete categories)
bins <- equal_freq(DB$edssges_N, 2) # also tried with 3
summary(bins)
DB$edssges_N[which(DB$edssges_N<3)] <- "< 3"
DB$edssges_N[which(DB$edssges_N>=3)] <- "3 - 7"


# Make table ====================================================####
# Make and save the demographics table for the manuscript

# Make dataset with variables to summarize
summ_vbles <- DB %>% select("Age [years]" = age_N,
                            "Sex" = sex_E,
                            "Education levels" = schule_E,
                            "Disease duration [years]" =
                              Disease_duration,
                            "Current MS medication [yes]" =
                              Curr_MS_medication_yes_no,
                            "Number of relapses [last year]" =
                              Nr_relapses_last_yr,
                            "Disability Status [EDSS]" = edssges_N,
                            "Total lesion volume [ml]" = TLV,
                            "Functional impairment [MSFC]" = MSFC,
                            "Global cognitive status [MoCA]" =
                              moca_N,
                            "Depression/Anxiety [HADS-D]" =
                              sumDAskala,
                            "Subjective fatigue [MFIS]" = mfis_N,
                            "Sleep quality [PSQI]" = TotalScore,
                            #MS_dx) %>%
                            ) %>%
  tbl_summary(missing = "ifany", #by = MS_dx
              type = "Global cognitive status [MoCA]" ~ "continuous",
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
gtsave(summ_vbles, "demographic_table.html")


# Comorbidities =================================================####
# Get a list of comorbidities
DB$Comorbidities[which(DB$Comorbidities!="")]


# PASAT =========================================================####
# Compute the z-score of PASAT (for manuscript revision)
# Mean and SD of the entire baseline cohort

DB$pasatZ <- (DB$pasatGesamt - 45.215789) / 11.4767


# DEPRESSION/ANXIETY ============================================####
# Obtain the separate depression/anxiety scores

# Read file 
anx_dep <- read.csv("depression_anxiety_all_FC.csv")

# Delete blank columns & restrict to "DB" rows
anx_dep <- anx_dep[anx_dep$patnr_C %in% DB$patnr_C, c(1:3)] # Excuse
# the hard-coding for the columns here!
rownames(anx_dep) <- NULL

cor.test(anx_dep$dskala_N, anx_dep$askala_N)
