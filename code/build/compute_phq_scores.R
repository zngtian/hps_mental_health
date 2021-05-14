source("initiate.R")

## read the input data from the hps PUF
## Change the path to your data file
f <- "/Users/ztian/OneDrive - The Pennsylvania State University/research/database/pulse_household/output_data/hps_individual_week1_27.rds"
df.puf <- readRDS(f)

vars.meta <- c("scram", "week", "est_st", "pweight")
vars.phq <- c("anxious", "interest", "down", "worry")

## Filter out NA, -88, and -99 for all variables -----------------------------------------------------------------
df <- df.puf %>%
    select(all_of(c(vars.meta, vars.phq))) %>%
    remove_missing(vars.use = vars.phq)

df.miss.phq <- df$missing

## the following work is based on this cleaned data
df.phq <- df$clean

## Compute PHQ scores ----------------------------------------------------------------------------------------
compute_phq_scores <- function(data, vars.phq) {

    stopifnot(all(vars.phq %in% names(data)))

    data <- data %>%
        ## (1) Recode the four mental variables
        mutate(across(all_of(vars.phq), ~ (.x - 1))) %>%
        ##  (2) Compute valid phq2, gad2, and phq4
        ## The Household Pulse Survey coded values for mental health scale
        ## items with numbers that do not match usual clinical applications.
        ## This recoding places responses into a value set more commonly used
        ## by clinicians and researchers in Psychology.
        mutate(
            valid_phq2 = ifelse(interest >= 0 & down >= 0, 1, ifelse(is.na(interest) | is.na(down), 0, NA)),
            valid_gad2 = ifelse(anxious >= 0 & worry >= 0, 1, ifelse(is.na(anxious) | is.na(worry), 0, NA)),
            valid_phq4 = ifelse(valid_phq2 == 1 & valid_gad2 == 1, 1, ifelse(valid_phq2 == 0 | valid_gad2 == 0, 0, NA))
        ) %>%
        ## (3) Compute composite scores for each scale
        ## If respondent has a valid responses for all items in a given
        ## diagnostic scale, they receive a composite score which is the
        ## sum of their responses for all of the scale's components,
        ## otherwise not and a missing value is entered.
        mutate(
            composite_phq2 = ifelse(valid_phq2 == 1, interest + down, NA),
            composite_gad2 = ifelse(valid_gad2 == 1, anxious + worry, NA),
            composite_phq4 = ifelse(valid_phq4 == 1, interest + down + anxious + worry, NA)
        ) %>%
        ## (4) Create indicators of the need for clinical intervention based
        ## on composite scores for diagnostic scales.
        ## If respondent has a composite score above the threshold, they should
        ## be referred for clinical intervention and they receive a value of 1 to
        ## indicate clinical status, otherwise not and value of 0 is entered.
        mutate(
            clinical_phq2 = ifelse(composite_phq2 >= 3, 1, ifelse(is.na(composite_phq2), NA, 0)),
            clinical_gad2 = ifelse(composite_gad2 >= 3, 1, ifelse(is.na(composite_gad2), NA, 0)),
            clinical_phq4 = ifelse(composite_phq4 >= 6, 1, ifelse(is.na(composite_phq4), NA, 0))
        ) %>%
        ## Create categorical classifications of anxiety-depression using PHQ-4 clinical
        ## diagnostic scale composite scores. If respondent has a composite
        ## score less than 2 they are Normal (value:0), between 3 and 5 Mild (value: 1),
        ## between 6 and 8 Moderate (value: 2), and between 9 and 12 Severe (value: 3)
        ## Respondents who did not complete all four items in the scale should receive a
        ## missing value. Those with values outside of the range 0 to 12 are invalid and
        ## indicate and error has occurred. ##### N. B. Respondents in the Moderate and
        ## Severe categories would be referred for additional mental health resources in
        ## a clinical setting. #####
        mutate(
            category_phq4_normal = ifelse(composite_phq4 <=2, 1, ifelse(is.na(composite_phq4), NA, 0)),
            category_phq4_mild = ifelse(composite_phq4 >=3 & composite_phq4<=5, 1, ifelse(is.na(composite_phq4), NA, 0)),
            category_phq4_moderate = ifelse(composite_phq4 >=6 & composite_phq4<=8, 1, ifelse(is.na(composite_phq4), NA, 0)),
            category_phq4_severe = ifelse(composite_phq4 >=9 & composite_phq4<=12, 1, ifelse(is.na(composite_phq4), NA, 0)),
        ) %>%
        mutate(category_phq4 =
                   case_when(
                       category_phq4_normal == 1 ~ "Normal",
                       category_phq4_mild == 1 ~ "Mild",
                       category_phq4_moderate == 1 ~ "Moderate",
                       category_phq4_severe == 1 ~ "Severe",
                       TRUE ~ NA_character_)
        ) %>%
        mutate(
            catnum_phq4 =
                case_when(
                    category_phq4 == "Normal" ~ 0,
                    category_phq4 == "Mild" ~ 1,
                    category_phq4 == "Moderate" ~ 2,
                    category_phq4 == "Severe" ~ 3,
                    TRUE ~ NA_real_)
        )
}


df.phq <- compute_phq_scores(df.phq, vars.phq)

write_csv(df.phq, "output_data/phq_individual_level.csv")
