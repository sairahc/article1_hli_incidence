library(tidyverse)
library(MASS)

# variable generation for descriptive table purposes
casesIncluded$active <- case_when(casesIncluded$physicalActivity < 6 ~ 0,
                                  casesIncluded$physicalActivity >=6 ~ 1)

casesIncluded$breastfeedingCat <- case_when(casesIncluded$breastfeedMths == 0 ~ 0,
                                            casesIncluded$breastfeedMths > 0 & casesIncluded$breastfeedMths <= 12 ~ 1,
                                            casesIncluded$breastfeedMths>12 ~ 2)

#create complete cases dataset
casesIncludedComplete <- filter(casesIncluded, !is.na(HLIScore))


casesIncludedCompleteBreast <- filter(casesIncluded, !is.na(HLIScore) & 
                                        !is.na(education) &
                                        !is.na(HRTStatus) &
                                        !is.na(OCEverUse) &
                                        !is.na(ageMenarche) &
                                        !is.na(parity) &
                                        !is.na(familyHistBC))

casesIncludedCompleteEndometrial <- filter(casesIncluded, !is.na(HLIScore) & 
                                        !is.na(education) &
                                        !is.na(HRTStatus) &
                                        !is.na(OCEverUse) &
                                        !is.na(ageMenarche) &
                                        !is.na(parity))




# ----

descriptivesByGroup <- function(dataframe, variable) {
  
  descGroups <- dataframe %>%
  group_by(HLI4) %>%
    summarise(
      count = n(),
      mean = mean({{variable}}, na.rm=TRUE),
      SD = sd({{variable}}, na.rm = TRUE),
      median = median({{variable}}, na.rm=TRUE),
      IQ1 = quantile({{variable}}, probs = 0.25, na.rm=TRUE),
      IQ3 = quantile({{variable}}, probs = 0.75, na.rm=TRUE)
    ) 

  descOverall <- dataframe %>%
    summarise(
      count = n(),
      mean = mean({{variable}}, na.rm=TRUE),
      SD = sd({{variable}}, na.rm = TRUE),
      median = median({{variable}}, na.rm=TRUE),
      IQ1 = quantile({{variable}}, probs = 0.25, na.rm=TRUE),
      IQ3 = quantile({{variable}}, probs = 0.75, na.rm=TRUE)
    )
  
  overall <- data.frame("Overall")
  colnames(overall) <- c("HLI4")
                       
  descOverallLabeled <- cbind(overall, descOverall)
  
  desc <- rbind(descOverallLabeled, descGroups)
  
  return(desc)
}


desc <- descriptivesByGroup(casesIncludedComplete, height)
View(desc)

# couldnt manage to make function for proportions
# manually switch out *variable*
prop <- casesIncludedComplete %>%
  group_by(HLI4, active) %>%
  summarise(
    count = n()) %>%
  mutate(freq = count / sum(count))
View(prop)


# remove pre-menopausal cancer to get correct number of post-meno breast and endometrial cases
casesPostMeno <- filter(casesIncluded, ageDiagnosis - ageMenopause > 0 |
                          is.na(ageDiagnosis))

# get counts of postmeno cancer cases across HLI cats
prop <- casesPostMeno %>%
  group_by(HLI4, statusOvarian) %>%
  summarise(
    count = n()) %>%
  mutate(freq = count / sum(count))
View(prop)



# testing education difference across HLI4 groups
anova <- aov(education~HLI4, data=casesIncluded)
summary(anova)
tukey <- TukeyHSD(aov) # tukey measures mean difference between individual groups
tukey

# testing breastfeeding difference across HLI4 groups
# consider ordinal logistic regression from ::MASS
olrBreastfeeding <- polr(as.factor(breastfeedingCat)~HLI4,
                          data=casesIncluded)
cbind(exp(coef(olrBreastfeeding)),exp(confint(olrBreastfeeding)))

# testing breastfeeding continuous over HLI score
lrBreastfeeding <- glm(breastfeedMths~ HLIScore,
                       data=casesIncluded,
                       family = "gaussian")

# parity
olrParity <- polr(as.factor(parityCat)~HLI4,
                         data=casesIncluded)
cbind(exp(coef(olrParity)),exp(confint(olrParity)))

lrParity <- glm(parity~ HLIScore,
                       data=casesIncluded,
                       family = "gaussian")

# HRT

olrHRT <- polr(as.factor(HRTStatus)~HLI4,
                  data=casesIncluded)
cbind(exp(coef(olrHRT)),exp(confint(olrHRT)))



