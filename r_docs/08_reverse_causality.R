# ===========================================

# script name: 8. Reverse causality
# author: Sairah Lai Fa Chen

# data: NOWAC diet cohort
# description: testing for reverse causality by excluding first 2 yrs followup 

# packages
library(tidyverse)
library(survival)

# ===========================================






# exclude those diagnosed with cancer within 2 years fu time
# run analysis on observed dataset

excludeCancerWithin2Years <- function(dataframe){
  
  cancerWithin2Years <- case_when(
    dataframe$diagnosisDate - dataframe$entryDate < 730 ~ 1,
    is.na(dataframe$diagnosisDate) ~ 0,
    TRUE ~ 0
  )
  return(
    filter(dataframe,
           cancerWithin2Years == 0))
}
  
dfReverseCausality <- excludeCancerWithin2Years(casesIncluded)


coxBreastReverseCausality <- coxph(Surv(ageMenopause, ageExit, statusBreast) ~ HLIScore + 
                                     education +
                                     HRTStatus +
                                     OCEverUse +
                                     MENSALD + 
                                     parity +
                                     familyHistBC,
                                 data=dfReverseCausality)

coxColorectalReverseCausality <- coxph(Surv(ageEntry, ageExit, statusColorectal) ~ HLIScore + education,
                                   data=dfReverseCausality)

coxLungReverseCausality <- coxph(Surv(ageEntry, ageExit, statusLung) ~ HLIScore + education,
                                 data=dfReverseCausality)

coxEndometrialReverseCausality <- coxph(Surv(ageMenopause, ageExit, statusEndometrial) ~ HLIScore + 
                                     education +
                                     HRTStatus +
                                     OCEverUse +
                                     MENSALD + 
                                     parity,
                                   data=dfReverseCausality)

coxPancreaticReverseCausality <- coxph(Surv(ageEntry, ageExit, statusPancreatic) ~ HLIScore + education,
                                 data=dfReverseCausality)

coxKidneyReverseCausality <- coxph(Surv(ageEntry, ageExit, statusKidney) ~ HLIScore + education,
                                 data=dfReverseCausality)

