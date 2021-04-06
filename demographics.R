####=========================================================####
# A.L.R.R. March 2021
# Script to create demographic table for manuscript


####=========================================================####
# RELEVANT PACKAGES
# Loading relevant packages
# install.packages("pacman") # one time step
# require(pacman)
# pacman::p_load(dplyr, gtsummary, gt, webshot, funModeling)


####=========================================================####
# SET DIRECTORY
setwd(paste('/Users/lmuresearchfellowship/Documents/Adriana/',
            'LMU_Psychology/Projects/MS/Docs', sep = ""))


####=========================================================####
# OBTAIN DATA

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
DB$MS_dx[which(DB$MS_dx==2)] <- "P/SP-MS"
DB$MS_dx <- factor(DB$MS_dx, levels = c("RRMS", "P/SP-MS"))

# Adjust levels for sex
DB$sex_E[which(DB$sex_E==1)] <- "x"
DB$sex_E[which(DB$sex_E==2)] <- "y"
DB$sex_E <- factor(DB$sex_E, levels = c("x", "y"))

# Adjust levels for education
DB$schule_E <- factor(DB$schule_E)

# Adjust levels for number of relapses last year
DB$Nr_relapses_last_yr[which(
  DB$Nr_relapses_last_yr>=2)] <- "≥2"
DB$Nr_relapses_last_yr <- factor(DB$Nr_relapses_last_yr,
                                 levels = c("0", "1", "≥ 2"))

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
  DB$MS_dx=="P/SP-MS" & DB$Curr_MS_medication_yes_no=="Yes")])
freq_table <- data.frame(cbind(freq = Medic_type_table,
      percentage = round((prop.table(Medic_type_table)*100), 2)))

# Adjust EDSS
  # Check how to break into bins (given it's discrete categories)
bins <- equal_freq(DB$edssges_N, 2) # also tried with 3
summary(bins)
DB$edssges_N[which(DB$edssges_N<3)] <- "< 3"
DB$edssges_N[which(DB$edssges_N>=3)] <- "3 - 7"


####=========================================================####
# Make table

# Make dataset with variables to summarize
summ_vbles <- DB %>% select("Age [years]" = age_N,
                            "Sex" = sex_E,
                            "Education level" = schule_E,
                            "Subjective fatigue [MFIS]" = mfis_N,
                            "Sleep quality [PSQI]" = psqi_N,
                            "Depression/Anxiety [HADS-D]" = sumDAskala,
                            "Functional impairment [MSFC]" = MSFC,
                            "Global cognitive status [MoCA]" = moca_N,
                            "Disease duration [years]" = Disease_duration,
                            "Current MS medication [yes]" = Curr_MS_medication_yes_no,
                            "Total lesion volume [ml]" = TLV,
                            "Disability Status [EDSS]" = edssges_N,
                            "Number of relapses [last year]" = Nr_relapses_last_yr,
                            MS_dx) %>%
  tbl_summary(by = MS_dx, missing = "no",
              type = "Global cognitive status [MoCA]" ~ "continuous",
              digits = all_continuous() ~ 1,) %>%
  add_p() %>% modify_header(label = "**Variable**") %>%
  add_overall() %>% #bold_labels() %>%
  as_gt() %>% opt_table_font(font = "Arial") %>%
  tab_options(table_body.hlines.width = 0,
              column_labels.border.top.width = 2,
              column_labels.border.top.color = "black",
              column_labels.border.bottom.width = 2,
              column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table.border.bottom.color = "white",
              table.font.size = px(14))
gtsave(summ_vbles, "demographic_table.rtf")
gtsave(summ_vbles, "demographic_table.html")
webshot(url =
"file:///Users/lmuresearchfellowship/Documents/Adriana/LMU_Psychology/Projects/MS/Docs/demographic_table.html",
        "table.jpeg", zoom = 6)#, cliprect = c(0, 0, 850, 400),
        #expand = c(0, 0, 0, -100))


####=========================================================####
# Reporting comorbidities (more narratively, I guess)
DB$Comorbidities[which(DB$Comorbidities!="" &
                         DB$MS_dx=="P/SP-MS")]
