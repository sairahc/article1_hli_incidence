# ===========================================

# script name: 3. Imputation
# author: Sairah Lai Fa Chen

# data: NOWAC diet cohort
# description: impute 

# packages

install.packages("missForest")
install.packages("Hmisc")
install.packages("mice")
install.packages("VIM")
install.packages("rms")
install.packages("tidyverse")
install.packages("survival")
library(missForest)
library(Hmisc)
library(mice)
library(VIM)
library(rms)
library(tidyverse)
library(survival)
# ==========================================



# Nelson-Aalen estimator ----
#cumulative hazard rate 
#need for imputation 

findNelsonAalen <- function(dataframe){
  dataframe %>%
    mutate(nelsonAalen = nelsonaalen(dataframe,
                                     ageExit,
                                     cancerIncident))
}
casesIncluded <- findNelsonAalen(casesIncluded)


#plot(x=casesIncluded$followUpTime, 
#     y=casesIncluded$nelsonAalen,
#     ylab="Cumulative hazard",
#     xlab="Time")

# transform smoking status into categorical 
casesIncluded$smokingStatus <- as.factor(casesIncluded$smokingStatus)
# Create dataframe with only variables in analysis / predictors of those variables ----

selectImputationVars <- function(casesIncludedDataframe){
  return(
    casesIncludedDataframe %>%
    select(myID, #complete, but taken out from predictor matrix, below
           ageEntry, # complete
           education, #
           physicalActivity,
           height,
           weight,
           grAlcohol, 
           currentSmokingIntensity,
           timeSinceQuitSmoking,
           smokingStatus, 
           grWholeGrainBread,
           grMusli, #complete
           grFruit,
           grVegetable,
           grRedMeat,
           grProcessedMeat,
           grMilk,
           grCheese,
           grYogurt, #complete 
           energyIntake, #complete
           breastfeedMths,#complete
           ageMenarche, 
           HRTStatus, #complete
           OCEverUse, #complete
           familyHistBC, #complete
           hypertensive, #complete
           heartFailure, #complete
           diabetic, #complete
           stroke, #complete
           infarction, #complete
           parityCat, #complete
           cancerIncident, #complete
           nelsonAalen 
           
  ))
}

imputationReady <- selectImputationVars(casesIncluded)   
p_missing <- unlist(lapply(imputationReady, function(x) sum(is.na(x))))/nrow(imputationReady)
sort(p_missing[p_missing > 0], decreasing = TRUE)
sapply(casesIncluded, function(x) sum(is.na(x)))

#imputationDataframe = df containing predictor variables, exclusions already made
#creates mids object from MICE 
# assign output name of desired mids object
#TODO check methods pmm and log regression 
# TODO Nelson Aalen function should be in the imputation model 
imputeToMids <- function(imputationDataframe){
  initialise <- mice(imputationDataframe, maxit = 0)
  predM <- initialise$predictorMatrix
    predM[, c("myID")] = 0
    predM["timeSinceQuitSmoking", "currentSmokingIntensity"] = 0
    predM["currentSmokingIntensity", "timeSinceQuitSmoking"] = 0 
  meth <- initialise$method
    ordinal <- c("smokingStatus")
#    meth[binary] = "logreg" / all binary vars are complete
    meth[ordinal] = "polr"
  
  imputedMids <- mice(imputationDataframe, method=meth, predictorMatrix = predM, m=20, print=FALSE)
  #TODO make a plot for imputedMids to check convergence
  return(imputedMids)
}

imputedMids <- imputeToMids(imputationReady) 



#check there are no missing left with this----
#sapply(dataframe, function(x) sum(is.na(x)))
sapply(complete(imputedMids), function(x) sum(is.na(x)))

# save
saveRDS(imputedMids, "C:/Users/sch044/OneDrive - UiT Office 365/R/Paper1_new/data/imputedMids21082020.RDS")






# ----------------------------------------------------------------
# do not run
# check if survival is different in BMI missing compared to BMI complete
#casesIncluded$isMissingBMI <- ifelse(is.na(casesIncluded$BMI),
#                                     TRUE,
#                                     FALSE)
#
#KMbmi <- survfit(Surv(followUpTime, statusColorectal) ~ isMissingBMI, 
#                 data= casesIncluded)
#ggsurvplot(
#  fit= KMbmi,
#  xlab="Days",
#  ylab= "Probability of no CRC"
#)
#survdiff(Surv(followUpTime,statusColorectal) ~ isMissingBMI, data=casesIncluded)
#------------------------------------------------------------------------

