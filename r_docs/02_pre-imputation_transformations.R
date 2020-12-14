# PRE-IMPUTATION TRANSFORMATIONS
#----
library(tidyverse)
library(lubridate)

# Smoking variables----

# Match correct smoking intensity vars from correct series

matchSmokingIntensityIntervals <- function(dataframe){
  return(
    dataframe %>%
      mutate(
        smoking1519 = case_when(
          SERIENR >= 14 & SERIENR <= 26 ~ ROYKANT1,
          TRUE ~ ROYKANT1519),
        smoking2029 = case_when(
          SERIENR >= 14 & SERIENR <= 26 ~ ROYKANT2,
          TRUE ~ ROYKANT2029),
        smoking3039 = case_when(
          SERIENR >= 14 & SERIENR <= 26 ~ ROYKANT3,
          TRUE ~ ROYKANT3039),
        smoking4049 = case_when(
          SERIENR >= 14 & SERIENR <= 26 ~ ROYKANT4,
          TRUE ~ ROYKANT4049),
        smoking5059 = case_when(
          SERIENR >= 14 & SERIENR <= 26 ~ ROYKANT5,
          TRUE ~ ROYKANT50MM),
        smoking60pl = case_when(
          SERIENR >= 14 & SERIENR <= 26 ~ ROYKANT6,
          TRUE ~ ROYKANT50MM))
  )
}

casesIncluded <- matchSmokingIntensityIntervals(casesIncluded)


# Find current intensity by matching age to corresponding interval


findCurrentSmokingIntensity <- function(dataframe){
  return (dataframe %>% 
            mutate(
              currentSmokingIntensity = case_when(
                smokingStatus == 3 ~ case_when(
                  SERIENR == 28 | SERIENR == 29 ~ ROYKAR2,
                  SERIENR == 32 | SERIENR == 33 ~ ROYKAR1,
                  ageEntry >= 65 ~ smoking60pl,
                  ageEntry >= 55 ~ smoking5059,
                  ageEntry >= 45 ~ smoking4049,
                  ageEntry >= 35 ~ smoking3039,
                  ageEntry >= 25 ~ smoking2029,
                  ageEntry >= 15 ~ smoking1519), 
                smokingStatus == 1 | smokingStatus == 2 ~ as.integer(0),
              )))
}  

casesIncluded <- findCurrentSmokingIntensity(casesIncluded)


# Check smokingIntensityCurrent works
#smokers <- casesIncluded[casesIncluded$smokingStatus==3 & !is.na(casesIncluded$smokingStatus),] #be careful!
#without !is.na statement, smokingStatus =na will be included 
#table(smokers$smokingIntensityCurrent, exclude=NULL)



# Duration since quitting smoking ----


timeQuit <- function(dataframe){
  return(dataframe %>%
           mutate(timeSinceQuitSmoking = case_when(
             smokingStatus == 2 ~ ageEntry - SISTALD,
             smokingStatus == 1 | smokingStatus == 3 ~ 0)
           ))
}

casesIncluded <- timeQuit(casesIncluded)


#--------------------------------------------------


#BMI----
# 
calculateBMI <- function(weightInKg, heightInCm){
  a <- weightInKg/((heightInCm/100)^2)
  return(a)
}

casesIncluded$BMI <- calculateBMI(casesIncluded$weight, casesIncluded$height)



# Alcohol


calculateGrAlcohol <- function(dataframe){
  return(
    dataframe %>%
    mutate(grAlcohol = case_when(
      AVHOLD == 1 & #Missing on alcohol if declared not sober and have not filled in frequency questions
      is.na(casesIncluded$OLGLASS) &
      is.na(casesIncluded$VINGLASS) &
      is.na(casesIncluded$DRINKER) ~ NA_real_,
      TRUE ~ ALKOGR
    )))
}  


casesIncluded <- calculateGrAlcohol(casesIncluded)

# physical activity (dichotomize PA scale for descriptive purposes)

findActive <- function(dataframe) {
  dataframe %>%
    mutate(active = case_when(dataframe$physicalActivity < 6 ~ 0,
                              dataframe$physicalActivity >=6 ~ 1
                              ))
}

casesIncluded <- findActive(casesIncluded) 

# Whole grain bread----

calculateGrWholeGrainBread <- function(dataframe){
  return(
    dataframe %>%
      mutate(grWholeGrainBread = case_when(
             is.na(fqgrbrod) &
               is.na(fqknbrod) &
               is.na(BRODFIN) ~ NA_real_,
             TRUE ~ grgrbrod)
  ))
}

casesIncluded <- calculateGrWholeGrainBread(casesIncluded)

# Fruit----

calculateGrFruit <- function(dataframe){
  return(
    dataframe %>% 
      mutate(grFruit = case_when(
        is.na(casesIncluded$fqeplepa) &
        is.na(casesIncluded$fqappels) &
        is.na(casesIncluded$fqbanan) &
        is.na(casesIncluded$fqanfruk) ~ NA_real_,
        TRUE ~ grfrukt
      ))
  )
}

casesIncluded <- calculateGrFruit(casesIncluded)



# Vegetables----

calculateGrVegetable <- function(dataframe){
  return(
    dataframe %>%
    mutate(grVegetable = case_when(
      is.na(casesIncluded$fqgulrot) &
      is.na(casesIncluded$fqkaal) &
      is.na(casesIncluded$fqbrokko)&
      is.na(casesIncluded$fqsalat) &
      is.na(casesIncluded$fqgrblan) &
      is.na(casesIncluded$fqkalrot)&
      is.na(casesIncluded$fqgrsak) ~ NA_real_,
      TRUE ~ grgrsak
    )))
}

casesIncluded <- calculateGrVegetable(casesIncluded)


# Red meat----

calculateGrRedMeat <- function(dataframe){
  return(
    dataframe %>%
      mutate(grRedMeat = case_when(
        is.na(casesIncluded$fqsteik) &
        is.na(casesIncluded$fqkotele) &
        is.na(casesIncluded$fqbiff) &
        is.na(casesIncluded$fqkjkake) &
        is.na(casesIncluded$fqpolse) &
        is.na(casesIncluded$fqlapska) &
        is.na(casesIncluded$fqpizza) &
        is.na(casesIncluded$fqkyllin) &
        is.na(casesIncluded$fqkjot) ~ NA_real_,
        TRUE ~ grrenkjo
      ))
  )
}
casesIncluded <- calculateGrRedMeat(casesIncluded)



# Processed Meat----

#calculateGrProcessedMeat <- function(dataframe){
# return(
#    dataframe %>%
#      mutate(grProcessedMeat = case_when(
#        is.na(casesIncluded$fqsteik) &
#          is.na(casesIncluded$fqkotele) &
#          is.na(casesIncluded$fqbiff) &
#          is.na(casesIncluded$fqkjkake) &
#          is.na(casesIncluded$fqpolse) &
#          is.na(casesIncluded$fqlapska) &
#          is.na(casesIncluded$fqpizza) &
#          is.na(casesIncluded$fqkyllin) &
#          is.na(casesIncluded$fqkjot) ~ NA_real_,
#        TRUE ~ grmalkjo
#      ))
#  )
#}


calculateGrProcessedMeat <- function(dataframe){
  return(
    dataframe %>%
      mutate(grProcessedMeat = case_when(
        is.na(casesIncluded$fqsteik) &
          is.na(casesIncluded$fqkotele) &
          is.na(casesIncluded$fqbiff) &
          is.na(casesIncluded$fqkjkake) &
          is.na(casesIncluded$fqpolse) &
          is.na(casesIncluded$fqlapska) &
          is.na(casesIncluded$fqpizza) &
          is.na(casesIncluded$fqkyllin) &
          is.na(casesIncluded$fqkjot) ~ NA_real_,
        TRUE ~ (grkjkake + # as per Parr, 2013
                ifelse(
                       is.na(grpolse), 
                      0, 
                      grpolse) + 
                grkjotpa)
      ))
  )
}



casesIncluded <- calculateGrProcessedMeat(casesIncluded)


# Milk----

calculateGrMilk <- function(dataframe){
  return(
    dataframe %>% 
      mutate(grMilk = case_when(
        is.na(casesIncluded$fqhemelk) &
        is.na(casesIncluded$fqlemelk) &
        is.na(casesIncluded$fqskmelk) &
        is.na(casesIncluded$fqdmelk) ~ NA_real_,
        TRUE ~ grmelk
      ))
  )
}

casesIncluded <- calculateGrMilk(casesIncluded)

# Cheese----

calculateGrCheese <- function(dataframe){
  return(
    dataframe %>%
      mutate(grCheese = case_when(
        is.na(casesIncluded$fqsyltet) & 
        is.na(casesIncluded$fqbrostf) &
        is.na(casesIncluded$fqbrostm) &
        is.na(casesIncluded$fqkvostf) &
        is.na(casesIncluded$fqkvostm) &
        is.na(casesIncluded$fqkjotpa) ~ NA_real_,
        TRUE ~ grbrostf + grbrostm + grkvostf + grkvostm
      ))
  )
}

casesIncluded <- calculateGrCheese(casesIncluded)

# Ratio polyunsaturated to saturated fat

calculateFatRatio <- function(dataframe){
  return(
    dataframe %>%
      mutate(fatRatio = totflfet / totmfett
      ))
}

casesIncluded <- calculateFatRatio(casesIncluded)

#--------------------------------------------------------------
#Covariates-------------------------

# Family history of breast cancer in 1st degree (0 = no, 1 = yes)


findFamilyHistBC <- function(dataframe){
  dataframe %>%
    mutate(familyHistBC = case_when((MOR==0 |
                                    MOR == 3 |
                                    DATTER == 0 |
                                    SOSTER == 0 |
                                    SOSTER ==3) ~ 1,
                                      (is.na(MOR) &
                                      is.na(DATTER) &
                                      is.na(SOSTER)) ~ 0,
                                    TRUE ~ 0))
  
}

casesIncluded <- findFamilyHistBC(casesIncluded)



# HRT status

findHRTStatus <- function(dataframe){
  dataframe %>%
    mutate(HRTStatus = case_when(HTGROUP == 1 ~ 0,
                                 HTGROUP == 2 ~ 1,
                                 HTGROUP == 3 ~ 2,
                                 TRUE ~ 0))
}

casesIncluded <- findHRTStatus(casesIncluded)



# Oral contraceptive ever use

findOCEverUse <- function(dataframe) {
  dataframe %>% 
    mutate(OCEverUse = case_when(PPILLE == 1 ~ 0,
                                 PPILLE == 0 ~ 1,
                                 is.na(PPILLE) ~ 0))
}

casesIncluded <- findOCEverUse(casesIncluded)

# parity categories
categorizeParity <- function(dataframe){
  dataframe %>%
    mutate(parityCat = case_when(parity == 0 ~ 0,
                                 parity == 1 ~ 1,
                                 parity == 2 ~ 1,
                                 parity > 2 ~ 2))
}

casesIncluded <- categorizeParity(casesIncluded)


# breastfeeding categories
categorizeBreastfeeding <- function(dataframe) {
  dataframe %>%
    mutate(breastfeedingCat = case_when(dataframe$breastfeedMths == 0 ~ 0,
                                        dataframe$breastfeedMths > 0 & dataframe$breastfeedMths <= 12 ~ 1,
                                        dataframe$breastfeedMths>12 ~ 2))
}

casesIncluded <- categorizeBreastfeeding(casesIncluded)


# Cancer incident? (0 = no, 1 = yes) Prevalent cancer will be filtered out anyways in exclusion.R

findCancerIncidence <- function(dataframe){
  dataframe %>%
    mutate(cancerIncident = ifelse(
      is.na(DIAGDAT), 
      0,
      1
    ))
}

casesIncluded <- findCancerIncidence(casesIncluded)


# Hypertension

findHypertensive <- function(dataframe){
  dataframe %>%
    mutate(hypertensive = ifelse(
      HYPERTE == 1 |
        is.na(HYPERTE),
      0,
      1
    ))
}

casesIncluded <- findHypertensive(casesIncluded)


# Myocardial infarction

findMyoInfarction <- function(dataframe){
  dataframe %>%
    mutate(infarction = ifelse(
      INF == 1 |
        is.na(INF),
      0,
      1
    ))
}

casesIncluded <- findMyoInfarction(casesIncluded)



# Diabetes

findDiabetic <- function(dataframe){
  dataframe %>%
    mutate(diabetic = ifelse(
      DIAB == 1 |
        is.na(DIAB),
      0,
      1
    ))
}

casesIncluded <- findDiabetic(casesIncluded)



# Heart failure

findHeartFailure <- function(dataframe){
  dataframe %>%
    mutate(heartFailure = ifelse(
      HJERTE == 1 |
        is.na(HJERTE),
      0,
      1
    ))
}

casesIncluded <- findHeartFailure(casesIncluded)




# Stroke

findStroke <- function(dataframe){
  dataframe %>%
    mutate(stroke = ifelse(
      SLAG == 1 |
        is.na(SLAG),
      0,
      1
    ))
}

casesIncluded <- findStroke(casesIncluded)


# survival object variables ----
# follow up time ----
# will be same for all models

findFollowUpTime <- function(dataframe){
  endOfFollowUp <-  pmin(as.Date("2018-12-31"), 
                         dataframe$deathDate,
                         dataframe$emigrationDate,
                         dataframe$diagnosisDate,
                         na.rm = TRUE)
  
  mutate(dataframe, followUpTime = endOfFollowUp - entryDate)
}

casesIncluded <- findFollowUpTime(casesIncluded)

#status ----
# Lung cancer (LC) status 


findLungCancerStatus <- function(dataframe){
  dataframe%>%
    mutate(statusLung = ifelse(dataframe$ICD10_GR=="C340" |
                                 dataframe$ICD10_GR=="C341" |
                                 dataframe$ICD10_GR=="C342" |
                                 dataframe$ICD10_GR=="C343" |
                                 dataframe$ICD10_GR=="C348" |
                                 dataframe$ICD10_GR=="C349" ,
                               TRUE, #true if event occurs
                               FALSE #false if censored
    ))
}


casesIncluded <- findLungCancerStatus(casesIncluded)


# small cell LC (SCLC)
findSCLCStatus <- function(dataframe){
  dataframe %>% 
    mutate(statusSCLC = ifelse(dataframe$HIST == 804133 |
                                  dataframe$HIST == 804134 |
                                  dataframe$HIST == 804139 |
                                  dataframe$HIST == 804539,
           TRUE,
           FALSE))
}

casesIncluded <- findSCLCStatus(casesIncluded)

# non-small cell LC (NSCLC)
findNSCLCStatus <- function(dataframe){
  dataframe %>%
    mutate(statusNSCLC = case_when((dataframe$ICD10_GR=="C340" | 
                                      dataframe$ICD10_GR=="C341" |
                                      dataframe$ICD10_GR=="C342" |
                                      dataframe$ICD10_GR=="C343" |
                                      dataframe$ICD10_GR=="C348" |
                                      dataframe$ICD10_GR=="C349") &
                                     (dataframe$HIST == 814031 | #adenocarcinoma
                                        dataframe$HIST == 814032 |
                                        dataframe$HIST == 814033 |
                                        dataframe$HIST == 814034 |
                                        dataframe$HIST == 814039 |
                                        dataframe$HIST == 820032 |
                                        dataframe$HIST == 820039 |
                                        dataframe$HIST == 823033 |
                                        dataframe$HIST == 824939  |
                                        dataframe$HIST == 825031  |
                                        dataframe$HIST == 825032  |
                                        dataframe$HIST == 825039  |
                                        dataframe$HIST == 825339  |
                                        dataframe$HIST == 825531  |
                                        dataframe$HIST == 825532  |
                                        dataframe$HIST == 825539  |
                                        dataframe$HIST == 826031  |
                                        dataframe$HIST == 826032  |
                                        dataframe$HIST == 826039  |
                                        dataframe$HIST == 831033  |
                                        dataframe$HIST == 843039  |
                                        dataframe$HIST == 848031  |
                                        dataframe$HIST == 848039  |
                                        dataframe$HIST == 848132  |
                                        dataframe$HIST == 848133  |
                                        dataframe$HIST == 848139  |
                                        dataframe$HIST == 849033  |
                                        dataframe$HIST == 849039  |
                                        dataframe$HIST == 855131  |
                                        dataframe$HIST == 855132  |
                                        dataframe$HIST == 855133  |
                                        dataframe$HIST == 855139  |
                                        dataframe$HIST == 856032  |
                                        dataframe$HIST == 856033   |
                                        dataframe$HIST == 856039   |
                                        dataframe$HIST == 857439  | 
                                        dataframe$HIST == 805232   |#squamous
                                        dataframe$HIST == 807031   |
                                        dataframe$HIST == 807032   |
                                        dataframe$HIST == 807033   |
                                        dataframe$HIST == 807034   |
                                        dataframe$HIST == 807036   |
                                        dataframe$HIST == 807039   |
                                        dataframe$HIST == 807131   |
                                        dataframe$HIST == 807132   |
                                        dataframe$HIST == 807134   |
                                        dataframe$HIST == 807139   |
                                        dataframe$HIST == 807233   |
                                        dataframe$HIST == 808333  |
                                        dataframe$HIST == 801233   |# large cell
                                        dataframe$HIST == 801234   |
                                        dataframe$HIST == 801239   |
                                        dataframe$HIST == 801339  |
                                        dataframe$HIST == 801033    |# what are other NSCL? |
                                        dataframe$HIST == 801034    |        
                                        dataframe$HIST == 801039    |
                                        dataframe$HIST == 802034    |   
                                        dataframe$HIST == 804633    |
                                        dataframe$HIST == 804634    |
                                        dataframe$HIST == 804639    |
                                        dataframe$HIST == 824031    |
                                        dataframe$HIST == 824033    |
                                        dataframe$HIST == 824039     |
                                        dataframe$HIST == 824633     |
                                        dataframe$HIST == 824639     |
                                        dataframe$HIST == 824932) ~ TRUE,
                                   is.na(dataframe$HIST) ~ FALSE,
                                   TRUE ~ FALSE
    ))
}

casesIncluded <- findNSCLCStatus(casesIncluded)


# Colorectal(CRC) status


findColonCancerStatus <- function(dataframe){
  dataframe%>%
    mutate(statusColon = ifelse(dataframe$ICD10_GR =="C18"|
                                  dataframe$ICD10_GR == "C181" |
                                  dataframe$ICD10_GR == "C183" |
                                  dataframe$ICD10_GR == "C184" |
                                  dataframe$ICD10_GR == "C185" |
                                  dataframe$ICD10_GR == "C186" |
                                  dataframe$ICD10_GR == "C187" |
                                  dataframe$ICD10_GR == "C188" |
                                  dataframe$ICD10_GR == "C189",
                                TRUE,
                                FALSE)
    )
}

casesIncluded <- findColonCancerStatus(casesIncluded)

findRectalCancerStatus <- function(dataframe){
  dataframe%>%
    mutate(statusRectal = ifelse(dataframe$ICD10_GR =="C19"|
                                   dataframe$ICD10_GR == "C199" |
                                   dataframe$ICD10_GR == "C20" |
                                   dataframe$ICD10_GR == "C209",
                                 TRUE,
                                 FALSE)
    )
}


casesIncluded <- findRectalCancerStatus(casesIncluded)

findColorectalCancerStatus <- function(dataframe){
  dataframe%>%
    mutate(statusColorectal = ifelse(dataframe$statusColon == TRUE |
                                       dataframe$statusRectal == TRUE,
                                     TRUE,
                                     FALSE
    ))
}


casesIncluded <- findColorectalCancerStatus(casesIncluded)




# post menopausal follow up time 
# Create DOB variable from birth year as 2 digit integer----



findPostMenoFollowUpTime <- function(dataframe){
  dateBirth <- as.Date(paste("19", as.character(dataframe$FAAR),
                             "07",
                             "02",
                             sep = ""),
                       "%Y%m%d")
  menopauseAge <- ifelse(is.na(dataframe$FKLIMALD), 
                         53,
                         dataframe$FKLIMALD)
  menopauseDate <- dateBirth + menopauseAge*365.25
  endOfFollowUpDate <-  pmin(as.Date("2018-12-31"), 
                             dataframe$deathDate,
                             dataframe$emigrationDate,
                             dataframe$diagnosisDate,
                             na.rm = TRUE)
  fuTime <- as.numeric(case_when(dataframe$MENOSTATUS == "POST" ~ endOfFollowUpDate - dataframe$entryDate,
                                 endOfFollowUpDate - menopauseDate >= 0 ~ endOfFollowUpDate - menopauseDate,
                                 endOfFollowUpDate - menopauseDate < 0 ~ NA_real_
  ))
  return(fuTime)
}

casesIncluded$postMenoFollowUpTime <- findPostMenoFollowUpTime(casesIncluded)

# age at menopause
findMenopausalAge <- function(dataframe){
  dataframe %>%
    mutate(ageMenopause = ifelse(is.na(dataframe$FKLIMALD),
                                 53,
                                 dataframe$FKLIMALD))
}
casesIncluded <- findMenopausalAge(casesIncluded)

# age at diagnosis
findAgeDiagnosis <- function(dataframe){
  birthDate <- as.Date(paste("19", as.character(dataframe$FAAR),
                             "07",
                             "02",
                             sep = ""),
                       "%Y%m%d")
  ageDiagnosis <- as.numeric((dataframe$diagnosisDate - birthDate)/365.25)
  
  return(ageDiagnosis)
}

casesIncluded$ageDiagnosis <- findAgeDiagnosis(casesIncluded)

# Breast cancer (BC) ----

findBreastCancerStatus <- function(dataframe){
  dataframe %>%
    mutate(statusBreast = ifelse(dataframe$ICD10_GR == "C50",
                                 TRUE,
                                 FALSE))
}


casesIncluded <- findBreastCancerStatus((casesIncluded))

findPostMenoBreastStatus <- function(dataframe){
  statusBreast <- ifelse(dataframe$ICS10_GR == "C50",
                              TRUE,
                              FALSE)
  dataframe %>%
    mutate(statusPostMenoBreast = ifelse(statusBreast == TRUE &
                                                ageMenopause - ageDiagnosis < 0, 
                                              TRUE,
                                              FALSE))
}

casesIncluded <- findPostMenoBreastStatus(casesIncluded)

# ovarian cancer status

findOvarianCancerStatus <- function(dataframe){
  dataframe %>%
    mutate(statusOvarian = ifelse(dataframe$ICD10_GR == "C56",
                                 TRUE,
                                 FALSE))
}
casesIncluded <- findOvarianCancerStatus(casesIncluded)

# kidney cancer ----

findKidneyCancerStatus <- function(dataframe){
  dataframe %>%
    mutate(statusKidney = ifelse(dataframe$ICD10_GR == "C64",
                                 TRUE,
                                 FALSE))
}

casesIncluded <- findKidneyCancerStatus(casesIncluded)


# pancreatic cancer ----
findPancreaticCancerStatus <- function(dataframe){
  dataframe %>%
    mutate(statusPancreatic = ifelse(dataframe$ICD10_GR == "C25",
                                     TRUE,
                                     FALSE))
}

casesIncluded <- findPancreaticCancerStatus(casesIncluded)



# thyroid cancer ----
findThyroidCancerStatus <- function(dataframe){
  dataframe %>%
    mutate(statusThyroid = ifelse(dataframe$ICD10_GR == "C73",
                                  TRUE,
                                  FALSE))
}

casesIncluded <- findThyroidCancerStatus(casesIncluded)


# endometrial cancer ----
findEndometrialCancerStatus <- function(dataframe){
  dataframe %>%
    mutate(statusEndometrial = ifelse(dataframe$ICD10_GR == "C54",
                                      TRUE,
                                      FALSE))
}

casesIncluded <- findEndometrialCancerStatus(casesIncluded)

findPreMenoEndometrialStatus <- function(dataframe){
  statusEndometrial <- ifelse(dataframe$ICS10_GR == "C54",
                              TRUE,
                              FALSE)
  dataframe %>%
    mutate(statusPreMenoEndometrial = ifelse(statusEndometrial == TRUE &
                                               ageMenopause - ageDiagnosis > 0, 
                                             TRUE,
                                             FALSE))
}

casesIncluded <- findPreMenoEndometrialStatus(casesIncluded)

findPostMenoEndometrialStatus <- function(dataframe){
  statusEndometrial <- ifelse(dataframe$ICS10_GR == "C54",
                              TRUE,
                              FALSE)
  dataframe %>%
    mutate(statusPostMenoEndometrial = ifelse(statusEndometrial == TRUE &
                                               ageMenopause - ageDiagnosis < 0, 
                                             TRUE,
                                             FALSE))
}

casesIncluded <- findPostMenoEndometrialStatus(casesIncluded)
# age at exit variable ----

findAgeExit <- function(dataframe){
  dataframe %>%
    mutate(ageExit = as.numeric(ageEntry + followUpTime/365.25))
  
}

casesIncluded <- findAgeExit(casesIncluded)
