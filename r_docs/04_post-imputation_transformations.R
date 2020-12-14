# ================================================

# script name: 4. Post-imputation transformations
# author: Sairah Lai Fa Chen

# data: NOWAC diet cohort
# description: transformations to vars that need to be completed after imputation

# packages
library(lubridate)
library(tidyverse)
library(missForest)
library(Hmisc)
library(mice)
library(VIM)
library(rms)
# ================================================

# merge with pre imputation dataset to get the rest of the variables unincluded in imputation
convertMidsToLongMerged <- function(imputedMids, casesIncludedDataframe){
  long <- complete(imputedMids, "long", include=TRUE)
  imputedMergedLong <- left_join(long, casesIncludedDataframe[, c("myID", 
                                                                  setdiff(
                                                                    colnames(casesIncludedDataframe),
                                                                    colnames(long)
                                                                  ))], by = "myID")
  return(imputedMergedLong)
}

imputedMergedLong <- convertMidsToLongMerged(imputedMids, casesIncluded)

# diet ----

# whole grains

calculateGrWholeGrain <- function(dataframe){
  dataframe %>%
    mutate(grWholeGrain = grWholeGrainBread + grMusli)
}

casesIncluded <- calculateGrWholeGrain(casesIncluded)
imputedMergedLong <- calculateGrWholeGrain(imputedMergedLong)

# dairy (milk, cheese, yogurt)

calculateGrDairy <- function(dataframe){
  dataframe %>%
    mutate(grDairy = grMilk + grCheese + grYogurt)
}

casesIncluded <- calculateGrDairy(casesIncluded)
imputedMergedLong <- calculateGrDairy(imputedMergedLong)

# energy adjustment of dietary variables by nutrient density 
findNutrientDensities <- function(dataframe){
  a <- dataframe$totkjoul/1000
  dataframe %>% 
  mutate(
         ndWholeGrain = grWholeGrain/a,
         ndVegetable = grVegetable/a,
         ndFruit = grFruit/a,
         ndDairy = grDairy/a,
         ndRedMeat = grRedMeat/a,
         ndProcessedMeat = grProcessedMeat/a)

}

casesIncluded <- findNutrientDensities(casesIncluded)
imputedMergedLong <- findNutrientDensities(imputedMergedLong)

# diet score 

scoreDietFull <- function(wholeGrainVariable,
                      vegetableVariable,
                      fruitVariable,
                      dairyVariable,
                      redMeatVariable,
                      processedMeatVariable){ 
  a <- cut(wholeGrainVariable, breaks = c(
    quantile(wholeGrainVariable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  a <- as.numeric(as.character(a))
  b <- cut(vegetableVariable, breaks = c(
    quantile(vegetableVariable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  b <- as.numeric(as.character(b))
  c <- cut(fruitVariable, breaks = c(
    quantile(fruitVariable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  c <- as.numeric(as.character(c))
  d <- cut(dairyVariable, breaks = c(
    quantile(dairyVariable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  d <- as.numeric(as.character(d))
  e <- cut(redMeatVariable, breaks = c(
    quantile(redMeatVariable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  e <- as.numeric(as.character(e))
  f <- cut(processedMeatVariable, breaks = c(
    quantile(processedMeatVariable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  f <- as.numeric(as.character(f))
  g <- a+b+c+d+e+f
  return(g)}

casesIncluded$dietScoreFull <- scoreDietFull(casesIncluded$ndWholeGrain,
                                         casesIncluded$ndVegetable,
                                         casesIncluded$ndFruit,
                                         casesIncluded$ndDairy,
                                         casesIncluded$ndRedMeat,
                                         casesIncluded$ndProcessedMeat)

scoreDiet <- function(wholeGrainVariable,
                      vegetableVariable,
                      fruitVariable,
                      dairyVariable,
                      redMeatVariable,
                      processedMeatVariable){ 
  a <- cut(wholeGrainVariable, breaks = c(
    quantile(wholeGrainVariable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  a <- as.numeric(as.character(a))
  b <- cut(vegetableVariable, breaks = c(
    quantile(vegetableVariable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  b <- as.numeric(as.character(b))
  c <- cut(fruitVariable, breaks = c(
    quantile(fruitVariable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  c <- as.numeric(as.character(c))
  d <- cut(dairyVariable, breaks = c(
    quantile(dairyVariable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  d <- as.numeric(as.character(d))
  e <- cut(redMeatVariable, breaks = c(
    quantile(redMeatVariable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  e <- as.numeric(as.character(e))
  f <- cut(processedMeatVariable, breaks = c(
    quantile(processedMeatVariable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  f <- as.numeric(as.character(f))
  g <- a+b+c+d+e+f
  g <- cut(g, breaks = c(
  quantile(g, probs = seq(0, 1, 0.2), na.rm=TRUE)), 
  labels = c("0", "1", "2", "3", "4"), include.lowest = TRUE, right = FALSE)
  g <- as.numeric(as.character(g))
return(g)
}
casesIncluded$dietScore <- scoreDiet(casesIncluded$ndWholeGrain,
                           casesIncluded$ndVegetable,
                           casesIncluded$ndFruit,
                           casesIncluded$ndDairy,
                           casesIncluded$ndRedMeat,
                           casesIncluded$ndProcessedMeat)

imputedMergedLong$dietScore <- scoreDiet(imputedMergedLong$ndWholeGrain,
                                         imputedMergedLong$ndVegetable,
                                         imputedMergedLong$ndFruit,
                                         imputedMergedLong$ndDairy,
                                         imputedMergedLong$ndRedMeat,
                                         imputedMergedLong$ndProcessedMeat)


# Each food group if want to look at score in components ----
# 0-3points, scored based on quartile

# whole grain score
scoreWholeGrain <- function(wholegrainVariable){ 
  a <- cut(wholegrainVariable, breaks = c(
    quantile(wholegrainVariable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("0","1", "2", "3"))
  as.numeric(as.character(a))
  return(a)
}

# vegetable score
scoreVegetable <- function(vegetableVariable){ 
  a <- cut(vegetableVariable, breaks = c(
    quantile(vegetableVariable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("0","1", "2", "3"))
  as.numeric(as.character(a))
  return(a)
}

# fruit score
scoreFruit <- function(fruitVariable){ 
  a <- cut(fruitVariable, breaks = c(
    quantile(fruitVariable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("0","1", "2", "3"))
  as.numeric(as.character(a))
  return(a)
}

# dairy score
scoreDairy <- function(dairyVariable){ 
  a <- cut(dairyVariable, breaks = c(
    quantile(dairyVariable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("0","1", "2", "3"))
  as.numeric(as.character(a))
  return(a)
}


# red meat score
scoreRedMeat <- function(redMeatVariable){ 
  a <- cut(redMeatVariable, breaks = c(
    quantile(redMeatVariable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"))
  as.numeric(as.character(a))
  return(a)
}

# processed meat score
scoreProcessedMeat <- function(processedMeatVariable){ 
  a <- cut(processedMeatVariable, breaks = c(
    quantile(processedMeatVariable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"))
  as.numeric(as.character(a))
  return(a)
}



# smoking  ----

scoreSmoking <- function(smokingStatus, 
                         timeSinceQuitSmoking,
                         currentSmokingIntensity){
  a <- case_when(smokingStatus == 1 ~ 4,
                 smokingStatus == 2 & timeSinceQuitSmoking > 10 ~ 3,
                 smokingStatus == 2 & timeSinceQuitSmoking <= 10 ~ 2,
                 smokingStatus == 3 & currentSmokingIntensity <= 3 ~ 1,
                 smokingStatus == 3 & currentSmokingIntensity > 3 ~ 0,
                 TRUE ~ NA_real_)
  return(a)
}
casesIncluded$smokingScore <- scoreSmoking(casesIncluded$ROYKSTAT, 
                                 casesIncluded$timeSinceQuitSmoking, 
                                 casesIncluded$currentSmokingIntensity)

imputedMergedLong$smokingScore <- scoreSmoking(imputedMergedLong$ROYKSTAT, 
                                               imputedMergedLong$timeSinceQuitSmoking, 
                                               imputedMergedLong$currentSmokingIntensity)


# BMI----


scoreBMI <- function(weight, height){
  a <- case_when(calculateBMI(weight, height) > 30 ~ 0,
                 calculateBMI(weight, height) > 27 ~ 1,
                 calculateBMI(weight, height) > 25 ~ 2,
                 calculateBMI(weight, height) > 23 ~ 3,
                 TRUE ~ 4
  )
}

casesIncluded$BMIScore <- scoreBMI(casesIncluded$weight, casesIncluded$height)
imputedMergedLong$BMIScore <- scoreBMI(imputedMergedLong$weight, imputedMergedLong$height)

scoreBMIExcludeLowest <- function(weight, height) {
  a <- case_when(calculateBMI(weight, height) > 30 ~ 0,
                 calculateBMI(weight, height) > 27 ~ 1,
                 calculateBMI(weight, height) > 25 ~ 2,
                 calculateBMI(weight, height) > 23 ~ 3,
                 calculateBMI(weight, height) >= 18.5 ~ 4,
                 TRUE ~ NA_real_
  )
}

casesIncluded$BMI18.5Score <- scoreBMIExcludeLowest(casesIncluded$weight, casesIncluded$height)

#alcohol----

scoreAlcohol <- function(gramsAlcohol){
  a <- case_when(gramsAlcohol > 20 ~ 0,
                 gramsAlcohol > 10 ~ 1,
                 gramsAlcohol > 5 ~ 2,
                 gramsAlcohol > 0 ~ 3,
                 TRUE ~ 4)
  return(a)
}
  
casesIncluded$alcoholScore <- scoreAlcohol(casesIncluded$grAlcohol)
imputedMergedLong$alcoholScore <- scoreAlcohol(imputedMergedLong$grAlcohol)

# physical activity----

scorePA <- function(physicalActivity){
  a <- cut(physicalActivity, breaks = (
    quantile(physicalActivity, probs= seq(0,1,0.2), na.rm=TRUE)),
    labels = c("0", "1", "2", "3", "4"), include.lowest = TRUE, right = FALSE)
  a <- as.numeric(as.character(a))
  return(a)
}
 
casesIncluded$physicalActivityScore <- scorePA(as.numeric(casesIncluded$physicalActivity))
imputedMergedLong$physicalActivityScore <- scorePA(as.numeric(imputedMergedLong$physicalActivity))


# reproductive related
breastfeeding3 <- function(dataframe, cumulativeBreastfeedingMonths){
  dataframe %>%
    mutate(breastfeedingCat = case_when(cumulativeBreastfeedingMonths == 0 ~ 0,
                                        cumulativeBreastfeedingMonths > 0 & cumulativeBreastfeedingMonths <= 12 ~ 1,
                                        cumulativeBreastfeedingMonths>12 ~ 2))
}

casesIncluded <- breastfeeding3(casesIncluded, casesIncluded$breastfeedMths)
imputedMergedLong <- breastfeeding3(imputedMergedLong, imputedMergedLong$breastfeedMths)



# HLI score----
# HLI continuous
scoreHLI <- function(dietScore, smokingScore, BMIScore, alcoholScore, physicalActivityScore){
  return(dietScore +
           smokingScore +
           BMIScore +
           alcoholScore +
           physicalActivityScore)
}

casesIncluded$HLIScore <- scoreHLI(casesIncluded$dietScore, 
                                       casesIncluded$smokingScore, 
                                       casesIncluded$BMIScore, 
                                       casesIncluded$alcoholScore, 
                                       casesIncluded$physicalActivityScore)

imputedMergedLong$HLIScore <- scoreHLI(imputedMergedLong$dietScore, 
                                       imputedMergedLong$smokingScore, 
                                       imputedMergedLong$BMIScore, 
                                       imputedMergedLong$alcoholScore, 
                                       imputedMergedLong$physicalActivityScore)

casesIncluded$HLIScoreExcludeBMI18.5 <- scoreHLI(casesIncluded$dietScore, 
                                   casesIncluded$smokingScore, 
                                   casesIncluded$BMI18.5Score, 
                                   casesIncluded$alcoholScore, 
                                   casesIncluded$physicalActivityScore)

# HLI in 4 groups
quantile(imputedMergedLong$HLIScore, probs = seq(0,1, 0.2), na.rm=TRUE)
quantile(casesIncluded$HLIScore, probs = seq(0,1, 0.2), na.rm=TRUE)

casesIncluded$HLI4 <- cut(casesIncluded$HLIScore,
                          breaks = c(0,5,10,15,20),
                          right = TRUE,
                          include.lowest = TRUE)
imputedMergedLong$HLI4 <- cut(imputedMergedLong$HLIScore,
                              breaks = c(0,5,10,15,20),
                              right = TRUE,
                              include.lowest = TRUE)
imputedMergedLong$HLI4psfmi <- as.numeric(# needs to be num for psfmi_coxr
                               relevel(
                               cut(imputedMergedLong$HLIScore,
                               breaks = c(0,5,10,15,20),
                               right = TRUE,
                               include.lowest = TRUE), ref = "(10,15]"))

# HLI in 5 groups (with the most populated grp split 11-13, 14-15)
casesIncluded$HLI11_13 <- cut(casesIncluded$HLIScore,
                          breaks = c(0,5,10, 13,15,20),
                          right = TRUE,
                          include.lowest = TRUE)
imputedMergedLong$HLI11_13 <- cut(imputedMergedLong$HLIScore,
                              breaks = c(0,5,10, 13,15,20),
                              right = TRUE,
                              include.lowest = TRUE)
# HLI without diet
imputedMergedLong$HLInoDiet <- imputedMergedLong$smokingScore +
                               imputedMergedLong$BMIScore +
                               imputedMergedLong$alcoholScore +
                               imputedMergedLong$physicalActivityScore


# HLI without smoking
imputedMergedLong$HLInoSmoking <- imputedMergedLong$dietScore +
                                  imputedMergedLong$BMIScore +
                                  imputedMergedLong$alcoholScore +
                                  imputedMergedLong$physicalActivityScore


# HLI without BMI
imputedMergedLong$HLInoBMI <- imputedMergedLong$dietScore +
                              imputedMergedLong$smokingScore +
                              imputedMergedLong$alcoholScore +
                              imputedMergedLong$physicalActivityScore

# HLI without alcohol
imputedMergedLong$HLInoAlcohol <- imputedMergedLong$dietScore +
                                  imputedMergedLong$BMIScore +
                                  imputedMergedLong$smokingScore +
                                  imputedMergedLong$physicalActivityScore

# HLI without physical activity
imputedMergedLong$HLInoPA <- imputedMergedLong$dietScore +
                                  imputedMergedLong$BMIScore +
                                  imputedMergedLong$alcoholScore +
                                  imputedMergedLong$smokingScore




# convert long imputed dataset back to mids object for analysis----
imputedMergedMids <- as.mids(imputedMergedLong)
