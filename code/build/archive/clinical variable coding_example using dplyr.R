######### Create categorical classifications of anxiety-depression using PHQ-4 clinical diagnostic scale composite scores. #########
############### If respondent has a composite score less than 2 they are Normal (value:0), between 3 and 5 Mild (value: 1), between 6 and 8 Moderate (value: 2), and between 9 and 12 Severe (value: 3)
############### Respondents who did not complete all four items in the scale should receive a missing value. Those with values outside of the range 0 to 12 are invalid and indicate and error has occurred. #####
############### N. B. Respondents in the Moderate and Severe categories would be referred for additional mental health resources in a clinical setting.  #####
df$category_phq4 <- with(df, ifelse(is.na(composite_phq4), NA, 
                                    ifelse(composite_phq4 <=2, "Normal", 
                                           ifelse(composite_phq4 >=3 && composite_phq4 <=5, "Mild", 
                                                  ifelse(composite_phq4 >=6 && composite_phq4 <=8, "Moderate", 
                                                         ifelse(composite_phq4 >=9 && composite_phq4 <=12, "Severe","ERROR"))))))
df$catnum_phq4 <- ifelse(is.na(df$category_phq4), NA, 
                         ifelse(df$category_phq4 == "Normal", 0, 
                                ifelse(df$category_phq4 == "Mild", 1, 
                                       ifelse(df$category_phq4 == "Moderate", 2, 
                                              ifelse(df$category_phq4 == "Severe", 3, NA)))))



############### Doing the same thing, but with mutate and case_when from the dplyr package.#####
install.packages("dplyr")
library(dplyr)
df %>% mutate(category_phq4 = 
                case_when(
                  is.na(composite_phq4) ~ NA,
                  composite_phq4 <=2 ~ "Normal",
                  composite_phq4 >=3 && composite_phq4 <=5 ~ "Mild",
                  composite_phq4 >=6 && composite_phq4 <=8 ~ "Moderate",
                  composite_phq4 >=9 && composite_phq4 <=12 ~ "Severe",
                  TRUE ~ NA))

df %>% mutate(catnum_phq4 = 
                case_when(
                  is.na(category_phq4)~ NA, 
                  category_phq4 == "Normal" ~ 0, 
                  category_phq4 == "Mild" ~ 1, 
                  category_phq4 == "Moderate" ~ 2, 
                  category_phq4 == "Severe" ~ 3, 
                  TRUE ~ NA))