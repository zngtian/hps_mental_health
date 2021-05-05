##### AFTER IMPORTING DATA #####

########## Recode user-identified missing values as system missing values. #########
df[df == -99] <- NA
df[df == -88] <- NA

########## Recode mental health scale items into standard clinical values. #########
############### The Household Pulse Survey coded values for mental health scale items with numbers that do not match usual clinical applications.
############### This recoding places responses into a value set more commonly used by clinicians and researchers in Psychology.#####
ifelse(interest ==1, 0, ifelse(interest ==2, 1, ifelse(interest ==3, 2, ifelse(interest ==4, 3,NA))))
ifelse(down ==1, 0, ifelse(down ==2, 1, ifelse(down ==3, 2, ifelse(down ==4, 3,NA))))
ifelse(anxious ==1, 0, ifelse(anxious ==2, 1, ifelse(anxious ==3, 2, ifelse(anxious ==4, 3,NA))))
ifelse(worry ==1, 0, ifelse(worry ==2, 1, ifelse(worry ==3, 2, ifelse(worry ==4, 3,NA))))

########## Validate Respondents for Mental Health - Ensure respondents have answered all items in a scale. ########## 
############### Scales are: PHQ-2 (depression diagnostic element), GAD-2 (anxiety component diagnostic element), and PHQ-4 (combined anxiety-depression diagnostic)#####
############### Determine if respondent has a valid responses for a given diagnostic scale. If respondent has answered for all items in a scale they have valid responses for a composite score 
############### and receive a value of 1 to indicate validity, if not a value of 0 is entered and if indeterminate a value of NA. #####
############### N.B.  THESE HAVE NO INTERPRETABLE MEANING. THEY ARE FOR VALIDATION PURPOSES ONLY.#####
df$valid_phq2 <- with(df, ifelse(interest >= 0 && down >= 1, 1, ifelse( is.na(interest) || is.na(down), 0, NA)))
df$valid_gad2 <- with(df, ifelse(anxious >= 0 && worry >= 0, 1, ifelse(is.na(anxious) || is.na(worry), 0, NA)))
df$valid_phq4 <- with(df, ifelse(valid_phq2 == 1 && valid_gad2 == 1, 1, ifelse(valid_phq2 == 0 || valid_gad2 == 0, 0, NA)))

########## Create composite scores for each scale ##########
############### If respondent has a valid responses for all items in a given diagnostic scale, they receive a composite score 
############### which is the sum of their responses for all of the scale's components, otherwise not and a missing value is entered. #####
df$composite_phq2 <- with(df, ifelse(valid_phq2 ==1, interest + down, NA)) 
df$composite_gad2 <- with(df, ifelse(valid_gad2 ==1, anxious + worry, NA)) 
df$composite_phq4 <- with(df, ifelse(valid_phq4 ==1, interest + down + anxious + worry, NA))

######### Create indicators of the need for clinical intervention based on composite scores for diagnostic scales. #########
############### If respondent has a composite score above the threshold, they should be referred for clinical intervention 
############### and they receive a value of 1 to indicate clinical status, otherwise not and value of 0 is entered. #####
df$clinical_phq2 <- with(df, ifelse(composite_phq2 >=3, 1, ifelse(is.na(composite_phq2), NA, 0)))
df$clinical_gad2 <- with(df, ifelse(composite_gad2 >=3, 1, ifelse(is.na(composite_gad2), NA, 0)))
df$clinical_phq4 <- with(df, ifelse(composite_phq4 <=6, 1, ifelse(is.na(composite_phq4), NA, 0)))

######### Create categorical classifications of anxiety-depression using PHQ-4 clinical diagnostic scale composite scores. #########
############### If respondent has a composite score less than 2 they are Normal (value:0), between 3 and 5 Mild (value: 1), between 6 and 8 Moderate (value: 2), and between 9 and 12 Severe (value: 3)
############### Respondents who did not complete all four items in the scale should receive a missing value. Those with values outside of the range 0 to 12 are invalid and indicate and error has occurred. #####
############### N. B. Respondents in the Moderate and Severe categories would be referred for additional mental health resources in a clinical setting.  #####
df$category_phq4 <- with(df, ifelse(is.na(composite_phq4), NA, 
                                    ifelse(composite_phq4 <=2, "Normal", 
                                                                      ifelse(composite_phq4 >=3 && composite_phq4 <=5, "Mild", 
                                                                                                           ifelse(composite_phq4 >=6 && composite_phq4 <=8, "Moderate", 
                                                                                                                                                                    ifelse(composite_phq4 >=9 && composite_phq4 <=12, "Severe","ERROR"))))))
df$catnum_phq4 <- ifelse(is.na(category_phq4), NA, 
                         ifelse(category_phq4 == "Normal", 0, 
                                ifelse(category_phq4 == "Mild", 1, 
                                       ifelse(category_phq4 == "Moderate", 2, 
                                              ifelse(category_phq4 == "Severe", 3, NA)))))
