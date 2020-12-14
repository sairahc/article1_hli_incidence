# -----------------------------------------------

# script name: Cox regression
# author: Sairah Lai Fa Chen

# data: NOWAC diet cohort
# description: linear survival analysis of complete case and imputed data

# packages
library(survival)
library(survminer)
library(lubridate)
library(tidyverse)
library(foreign)
library(psfmi)
library(mice)

# -------------------------------------------------

# median follow-up time----

# using reverse KM method
library(prodlim)
library(Publish)

quantile(prodlim(Hist(followUpTime, statusColorectal)~1,data=df,reverse=TRUE))

quantile(prodlim(Hist(followUpTimeMock, statusColorectal)~1,data=df,reverse=TRUE)) #mock

quantile(prodlim(Hist(postMenoFollowUpTime, statusBreast)~1,data=df,reverse=TRUE))

quantile(prodlim(Hist(followUpTime, statusLung)~1,data=df,reverse=TRUE))

quantile(prodlim(Hist(postMenoFollowUpTime, statusEndometrial)~1,data=df,reverse=TRUE))

quantile(prodlim(Hist(postMenoFollowUpTime, statusOvarian)~1,data=df,reverse=TRUE))

quantile(prodlim(Hist(followUpTime, statusPancreatic)~1,data=df,reverse=TRUE))

quantile(prodlim(Hist(followUpTime, statusKidney)~1,data=df,reverse=TRUE))




# breast cancer ----

completeCoxCrudeBreast <- coxph(Surv(ageMenopause, ageExit, statusBreast)~ HLIScore, data=casesIncluded)

multivariateBreastCox <- function(dataframe,score) {
  coxObject <- coxph(Surv(ageMenopause, ageExit, statusBreast) ~
                       score + 
                       education +
                       HRTStatus +
                       OCEverUse +
                       ageMenarche + 
                       parityCat +
                       familyHistBC +
                       breastfeedingCat,
                     data = dataframe)
}



completeCoxBreast <- multivariateBreastCox(casesIncluded,
                                           casesIncluded$HLIScore)

# model with HLI as categorical
casesIncluded$HLI4relevel <- relevel(factor(casesIncluded$HLI4), ref = "(10,15]")
completeCoxBreastCat <- multivariateBreastCox(casesIncluded,
                                              casesIncluded$HLI4relevel)


# Colorectal cancer ----

completeCoxCrudeColorectal <- coxph(Surv(ageEntry, ageExit, statusColorectal) ~ HLIScore, data = casesIncluded)

completeCoxColorectal<- coxph(Surv(ageEntry, ageExit, statusColorectal)~ HLIScore + education, data = casesIncluded)



# model with HLI as categorical
completeCoxColorectalCat <- coxph(Surv(ageEntry, ageExit, statusColorectal)~
                              relevel(factor(HLI4), ref = "(10,15]") +
                              education,
                              data = casesIncluded)


#fit <- survfit(Surv(followUpTime, statusColorectal) ~ HLI4, data=casesIncluded)
#ggsurvplot(fit, 
#           fun="cumhaz",
#           conf.int = TRUE,
#           legend.title = "HLI score groups")






# Lung cancer ----

# HLI continuous
completeCoxCrudeLung <- coxph(Surv(ageEntry, ageExit, statusLung) ~ HLIScore, data = casesIncluded)

completeCoxLung<- coxph(Surv(ageEntry, ageExit, statusLung)~ HLIScore + education, data = casesIncluded)


# model with HLI as categorical
completeCoxLungCat <- coxph(Surv(ageEntry, ageExit, statusLung)~
                              relevel(factor(HLI4), ref = "(10,15]") +
                              education,
                            data=casesIncluded)


# endometrial cancer ----

# HLI continuous
completeCoxCrudeEndometrial <- coxph(Surv(ageMenopause, ageExit, statusEndometrial)~ HLIScore, data=casesIncluded)


completeCoxEndometrial <- coxph(Surv(ageMenopause, ageExit, statusEndometrial) ~
                          HLIscore + 
                          education +
                          HRTStatus +
                          OCEverUse +
                          ageMenarche + 
                          parityCat +
                          breastfeedingCat,
                        data = casesIncluded)

#model with HLI as categorical
completeCoxEndometrialCat <- coxph(Surv(ageMenopause, ageExit, statusEndometrial) ~ 
                                 relevel(factor(HLI4), ref = "(10,15]") +
                                 education +
                                 HRTStatus +
                                 OCEverUse +
                                 ageMenarche + 
                                 parityCat +
                                 breastfeedingCat, 
                               data= casesIncluded)

# ovarian cancer ----
completeCoxOvarian <- coxph(Surv(ageMenopause, ageExit, statusOvarian) ~ 
                              HLIScore +
                              education +
                              HRTStatus +
                              OCEverUse +
                              ageMenarche + 
                              parityCat +
                              breastfeedingCat, 
                            data= casesIncluded)

#model with HLI as categorical
completeCoxOvarianCat <- coxph(Surv(ageMenopause, ageExit, statusOvarian) ~ 
                              relevel(factor(HLI4), ref = "(10,15]") +
                              education +
                              HRTStatus +
                              OCEverUse +
                              ageMenarche + 
                              parityCat +
                              breastfeedingCat, 
                            data= casesIncluded)

# pancreatic cancer ----

completeCoxCrudePancreatic <- coxph(Surv(ageEntry, ageExit, statusPancreatic) ~ HLIScore, data = casesIncluded)

completeCoxPancreatic<- coxph(Surv(ageEntry, ageExit, statusPancreatic)~ HLIScore + 
                                education, data = casesIncluded)


# model with HLI as categorical
completeCoxPancreaticCat <- coxph(Surv(ageEntry, ageExit, statusPancreatic)~
                              relevel(factor(HLI4), ref = "(10,15]") +
                              education,
                              data = casesIncluded)

# kidney cancer ----

completeCoxCrudeKidney <- coxph(Surv(ageEntry, ageExit, statusKidney) ~ HLIScore, data = casesIncluded)

completeCoxKidney<- coxph(Surv(ageEntry, ageExit, statusKidney)~ HLIScore + education, data = casesIncluded)


# model with HLI as categorical
completeCoxKidneyCat <- coxph(Surv(ageEntry, ageExit, statusKidney)~
                                    relevel(factor(HLI4), ref = "(10,15]") +
                                    education,
                                  data = casesIncluded)


# testing PH assumption ----
test.ph.breast <- cox.zph(completeCoxBreast)
test.ph.crc <- cox.zph(completeCoxColorectal)
test.ph.lung <- cox.zph(completeCoxLung)
test.ph.endometrial <- cox.zph(completeCoxEndometrial)
test.ph.ovarian <- cox.zph(completeCoxOvarian)
test.ph.pancreatic <- cox.zph(completeCoxPancreatic)
test.ph.kidney <- cox.zph(completeCoxKidney)



# ---------------------------------------------------------------
# testing individual HLI components with breast cancer incidence

smokingBreast <- coxph(Surv(ageMenopause, ageExit, statusBreast)~
                       smokingScore +
                         physicalActivityScore +
                         BMIScore +
                         dietScore +
                         alcoholScore +
                         ageMenarche +
                         breastfeedingCat +
                         HRTStatus +
                         OCEverUse +
                         parityCat +
                         familyHistBC,
                       data = casesIncluded)

PABreast <- coxph(Surv(ageMenopause, ageExit, statusBreast)~
                         physicalActivityScore +
                         smokingScore +
                         BMIScore +
                         dietScore +
                         alcoholScore +
                         ageMenarche +
                         breastfeedingCat +
                         HRTStatus +
                         OCEverUse +
                         parityCat +
                         familyHistBC,
                       data = casesIncluded)

BMIBreast <- coxph(Surv(ageMenopause, ageExit, statusBreast)~
                      BMIScore +   
                         smokingScore +
                         physicalActivityScore +
                         dietScore +
                         alcoholScore +
                         ageMenarche +
                         breastfeedingCat +
                         HRTStatus +
                         OCEverUse +
                         parityCat +
                         familyHistBC,
                       data = casesIncluded)

dietBreast <- coxph(Surv(ageMenopause, ageExit, statusBreast)~
                      dietScore +  
                         smokingScore +
                         physicalActivityScore +
                         BMIScore +
                         alcoholScore +
                         ageMenarche +
                         breastfeedingCat +
                         HRTStatus +
                         OCEverUse +
                         parityCat +
                         familyHistBC,
                       data = casesIncluded)

alcoholBreast <- coxph(Surv(ageMenopause, ageExit, statusBreast)~
                      alcoholScore +
                         physicalActivityScore +
                         BMIScore +
                         dietScore +
                         smokingScore +
                         ageMenarche +
                         breastfeedingCat +
                         HRTStatus +
                         OCEverUse +
                         parityCat +
                         familyHistBC,
                       data = casesIncluded)



#----------------------------------------------------------------
# imputed ----

# function for extracting HR and CI from a pooled object of class mipo
extractHRwithCIs <- function(pooledObject){  
  a <- cbind(as.character(pooledObject[,1]),
             exp(pooledObject[,2]),
             exp((pooledObject[,2]-1.96 * pooledObject[,3])),
             exp((pooledObject[,2] + 1.96*pooledObject[,3])))
  colnames(a) <- c("Predictor", "HR", "Lower CI", "Upper CI")
  return(a)
}

# HLI continuous
breast.pool <- with(data=imputedMergedMids, 
                    exp=coxph(Surv(ageMenopause, ageExit, statusBreast)~ 
                                HLIScore + 
                                education +
                                HRTStatus +
                                OCEverUse +
                                parityCat +
                                familyHistBC +
                                ageMenarche +
                                breastfeedingCat)) %>%
  pool()%>%
  summary()

breast.pool.HR <- extractHRwithCIs(breast.pool)




crc.pool <- with(data=imputedMergedMids, 
             exp=coxph(Surv(ageEntry, ageExit, statusColorectal)~ HLIScore + education)) %>%
  pool() %>%
  summary()

crc.pool.HR <- extractHRwithCIs(crc.pool)

lung.pool <- with(data=imputedMergedMids, 
                  exp=coxph(Surv(ageEntry, ageExit, statusLung)~ HLIScore + education)) %>%
  pool() %>%
  summary()


lung.pool.HR <- extractHRwithCIs(lung.pool)



endometrial.pool <- with(data=imputedMergedMids, 
                    exp=coxph(Surv(ageMenopause, ageExit, statusEndometrial)~ 
                                HLIScore + 
                                education +
                                HRTStatus +
                                OCEverUse +
                                parityCat +
                                ageMenarche +
                                breastfeedingCat)) %>%
  pool()%>%
  summary()

endometrial.pool.HR <- extractHRwithCIs(endometrial.pool)


ovarian.pool <- with(data=imputedMergedMids, 
                         exp=coxph(Surv(ageMenopause, ageExit, statusOvarian)~ 
                                     HLIScore + 
                                     education +
                                     HRTStatus +
                                     OCEverUse +
                                     parityCat +
                                     ageMenarche +
                                     breastfeedingCat)) %>%
  pool()%>%
  summary()

ovarian.pool.HR <- extractHRwithCIs(ovarian.pool) 



kidney.pool <- with(data=imputedMergedMids, 
                 exp=coxph(Surv(ageEntry, ageExit, statusKidney)~ HLIScore + education)) %>%
  pool() %>%
  summary()

kidney.pool.HR <- extractHRwithCIs(kidney.pool)

pancreatic.pool <- with(data=imputedMergedMids, 
                    exp=coxph(Surv(ageEntry, ageExit, statusPancreatic)~ 
                                HLIScore + 
                                education)) %>%
  pool() %>%
  summary()

pancreatic.pool.HR <- extractHRwithCIs(pancreatic.pool)



# HLI categorised with 10-15 set as reference group
lungCatHLI.pool <- with(data=imputedMergedMids,
                   exp = coxph(Surv(ageEntry, ageExit, statusLung) ~ 
                               relevel(factor(HLI4), ref= "(10,15]") +   
                               education)) %>%
  pool() %>%
  summary()

lungCatHLI.pool.HR <- extractHRwithCIs(lungCatHLI.pool)          


crcCatHLI.pool <- with(data=imputedMergedMids,
                        exp = coxph(Surv(ageEntry, ageExit, statusColorectal) ~ 
                                      relevel(factor(HLI4), ref= "(10,15]") +   
                                      education)) %>%
  pool() %>%
  summary()

crcCatHLI.pool.HR <- extractHRwithCIs(crcCatHLI.pool)     

breastCatHLI.pool <- with(data=imputedMergedMids,
                        exp = coxph(Surv(ageMenopause, ageExit, statusBreast) ~ 
                                      relevel(factor(HLI4), ref= "(10,15]") +   
                                      education+
                                      HRTStatus +
                                      OCEverUse +
                                      parityCat +
                                      familyHistBC +
                                      ageMenarche +
                                      breastfeedingCat)) %>%
  pool() %>%
  summary()

breastCatHLI.pool.HR <- extractHRwithCIs(breastCatHLI.pool)  

endometrialCatHLI.pool <- with(data=imputedMergedMids, 
                         exp=coxph(Surv(ageMenopause, ageExit, statusEndometrial)~ 
                                     relevel(factor(HLI4), ref= "(10,15]") + 
                                     education +
                                     HRTStatus +
                                     OCEverUse +
                                     parityCat +
                                     ageMenarche +
                                     breastfeedingCat)) %>%
  pool()%>%
  summary()

endometrialCatHLI.pool.HR <- extractHRwithCIs(endometrialCatHLI.pool)

ovarianCatHLI.pool <- with(data=imputedMergedMids, 
                               exp=coxph(Surv(ageMenopause, ageExit, statusOvarian)~ 
                                           relevel(factor(HLI4), ref= "(10,15]") + 
                                           education +
                                           HRTStatus +
                                           OCEverUse +
                                           parityCat +
                                           ageMenarche +
                                           breastfeedingCat)) %>%
  pool()%>%
  summary()

ovarianCatHLI.pool.HR <- extractHRwithCIs(ovarianCatHLI.pool)


pancreaticCatHLI.pool <- with(data=imputedMergedMids, 
                               exp=coxph(Surv(ageEntry, ageExit, statusPancreatic)~ 
                                           relevel(factor(HLI4), ref= "(10,15]") + 
                                           education)) %>%
  pool() %>%
  summary()

pancreaticCatHLI.pool.HR <- extractHRwithCIs(pancreaticCatHLI.pool)


kidneyCatHLI.pool <- with(data=imputedMergedMids, 
                    exp=coxph(Surv(ageEntry, ageExit, statusKidney)~ 
                                relevel(factor(HLI4), ref= "(10,15]") + 
                                education)) %>%
  pool() %>%
  summary()

kidneyCatHLI.pool.HR <- extractHRwithCIs(kidneyCatHLI.pool)






# HLI without diet


breastNoDiet.pool <- with(data=imputedMergedMids, 
                          exp=coxph(Surv(ageMenopause, ageExit, statusBreast)~ 
                                      HLInoDiet + 
                                      education +
                                      HRTStatus +
                                      OCEverUse +
                                      parityCat +
                                      familyHistBC +
                                      ageMenarche +
                                      breastfeedingCat)) %>%
  pool()%>%
  summary()
breastNoDiet.pool.HR <- extractHRwithCIs(breastNoDiet.pool)


crcNoDiet.pool <- with(data=imputedMergedMids, 
                       exp=coxph(Surv(ageEntry, ageExit, statusColorectal)~ HLInoDiet + education)) %>%
  pool() %>%
  summary()

crcNoDiet.pool.HR <- extractHRwithCIs(crcNoDiet.pool)

lungNoDiet.pool <- with(data=imputedMergedMids, 
                        exp=coxph(Surv(ageEntry, ageExit, statusLung)~ HLInoDiet + education)) %>%
  pool() %>%
  summary()

lungNoDiet.pool.HR <- extractHRwithCIs(lungNoDiet.pool)


endometrialNoDiet.pool <- with(data=imputedMergedMids, 
                          exp=coxph(Surv(ageMenopause, ageExit, statusEndometrial)~ 
                                      HLInoDiet + 
                                      education +
                                      HRTStatus +
                                      OCEverUse +
                                      parityCat +
                                      ageMenarche +
                                      breastfeedingCat)) %>%
  pool()%>%
  summary()
endometrialNoDiet.pool.HR <- extractHRwithCIs(endometrialNoDiet.pool)

ovarianNoDiet.pool <- with(data=imputedMergedMids, 
                               exp=coxph(Surv(ageMenopause, ageExit, statusOvarian)~ 
                                           HLInoDiet + 
                                           education +
                                           HRTStatus +
                                           OCEverUse +
                                           parityCat +
                                           ageMenarche +
                                           breastfeedingCat)) %>%
  pool()%>%
  summary()
ovarianNoDiet.pool.HR <- extractHRwithCIs(ovarianNoDiet.pool)


pancreaticNoDiet.pool <- with(data=imputedMergedMids, 
                               exp=coxph(Surv(ageEntry, ageExit, statusPancreatic)~ 
                                           HLInoDiet + 
                                           education)) %>%
  pool()%>%
  summary()
pancreaticNoDiet.pool.HR <- extractHRwithCIs(pancreaticNoDiet.pool)

kidneyNoDiet.pool <- with(data=imputedMergedMids, 
                              exp=coxph(Surv(ageEntry, ageExit, statusKidney)~ 
                                          HLInoDiet + 
                                          education)) %>%
  pool()%>%
  summary()
kidneyNoDiet.pool.HR <- extractHRwithCIs(kidneyNoDiet.pool)




# HLI without smoking


breastNoSmoking.pool <- with(data=imputedMergedMids, 
                             exp=coxph(Surv(ageMenopause, ageExit, statusBreast)~ 
                                         HLInoSmoking + 
                                         education +
                                         HRTStatus +
                                         OCEverUse +
                                         parityCat +
                                         familyHistBC +
                                         ageMenarche +
                                         breastfeedingCat)) %>%
  pool()%>%
  summary()
breastNoSmoking.pool.HR <- extractHRwithCIs(breastNoSmoking.pool)


crcNoSmoking.pool <- with(data=imputedMergedMids, 
                          exp=coxph(Surv(ageEntry, ageExit, statusColorectal)~ HLInoSmoking + education)) %>%
  pool() %>%
  summary()

crcNoSmoking.pool.HR <- extractHRwithCIs(crcNoSmoking.pool)



lungNoSmoking.pool <- with(data=imputedMergedMids, 
                           exp=coxph(Surv(ageEntry, ageExit, statusLung)~ HLInoSmoking + education)) %>%
  pool() %>%
  summary()

lungNoSmoking.pool.HR <- extractHRwithCIs(lungNoSmoking.pool)

endometrialNoSmoking.pool <- with(data=imputedMergedMids, 
                               exp=coxph(Surv(ageMenopause, ageExit, statusEndometrial)~ 
                                           HLInoSmoking + 
                                           education +
                                           HRTStatus +
                                           OCEverUse +
                                           parityCat +
                                           ageMenarche +
                                           breastfeedingCat)) %>%
  pool()%>%
  summary()
endometrialNoSmoking.pool.HR <- extractHRwithCIs(endometrialNoSmoking.pool)


ovarianNoSmoking.pool <- with(data=imputedMergedMids, 
                                  exp=coxph(Surv(ageMenopause, ageExit, statusOvarian)~ 
                                              HLInoSmoking + 
                                              education +
                                              HRTStatus +
                                              OCEverUse +
                                              parityCat +
                                              ageMenarche +
                                              breastfeedingCat)) %>%
  pool()%>%
  summary()
ovarianNoSmoking.pool.HR <- extractHRwithCIs(ovarianNoSmoking.pool)

pancreaticNoSmoking.pool <- with(data=imputedMergedMids, 
                           exp=coxph(Surv(ageEntry, ageExit, statusPancreatic)~ HLInoSmoking + education)) %>%
  pool() %>%
  summary()

pancreaticNoSmoking.pool.HR <- extractHRwithCIs(pancreaticNoSmoking.pool)

kidneyNoSmoking.pool <- with(data=imputedMergedMids, 
                                 exp=coxph(Surv(ageEntry, ageExit, statusKidney)~ HLInoSmoking + education)) %>%
  pool() %>%
  summary()

kidneyNoSmoking.pool.HR <- extractHRwithCIs(kidneyNoSmoking.pool)

# HLI without BMI


breastNoBMI.pool <- with(data=imputedMergedMids, 
                         exp=coxph(Surv(ageMenopause, ageExit, statusBreast)~ 
                                     HLInoBMI + 
                                     education +
                                     HRTStatus +
                                     OCEverUse +
                                     parityCat +
                                     familyHistBC +
                                     ageMenarche +
                                     breastfeedingCat)) %>%
  pool()%>%
  summary()
breastNoBMI.pool.HR <- extractHRwithCIs(breastNoBMI.pool)


crcNoBMI.pool <- with(data=imputedMergedMids, 
                          exp=coxph(Surv(ageEntry, ageExit, statusColorectal)~ HLInoBMI + education)) %>%
  pool() %>%
  summary()

crcNoBMI.pool.HR <- extractHRwithCIs(crcNoBMI.pool)



lungNoBMI.pool <- with(data=imputedMergedMids, 
                       exp=coxph(Surv(ageEntry, ageExit, statusLung)~ HLInoBMI + education)) %>%
  pool() %>%
  summary()

lungNoBMI.pool.HR <- extractHRwithCIs(lungNoBMI.pool)


endometrialNoBMI.pool <- with(data=imputedMergedMids, 
                                  exp=coxph(Surv(ageMenopause, ageExit, statusEndometrial)~ 
                                              HLInoBMI + 
                                              education +
                                              HRTStatus +
                                              OCEverUse +
                                              parityCat +
                                              ageMenarche +
                                              breastfeedingCat)) %>%
  pool()%>%
  summary()
endometrialNoBMI.pool.HR <- extractHRwithCIs(endometrialNoBMI.pool)


ovarianNoBMI.pool <- with(data=imputedMergedMids, 
                              exp=coxph(Surv(ageMenopause, ageExit, statusOvarian)~ 
                                          HLInoBMI + 
                                          education +
                                          HRTStatus +
                                          OCEverUse +
                                          parityCat +
                                          ageMenarche +
                                          breastfeedingCat)) %>%
  pool()%>%
  summary()
ovarianNoBMI.pool.HR <- extractHRwithCIs(ovarianNoBMI.pool)

pancreaticNoBMI.pool <- with(data=imputedMergedMids, 
                       exp=coxph(Surv(ageEntry, ageExit, statusPancreatic)~ HLInoBMI + education)) %>%
  pool() %>%
  summary()

pancreaticNoBMI.pool.HR <- extractHRwithCIs(pancreaticNoBMI.pool)

kidneyNoBMI.pool <- with(data=imputedMergedMids, 
                             exp=coxph(Surv(ageEntry, ageExit, statusKidney)~ HLInoBMI + education)) %>%
  pool() %>%
  summary()

kidneyNoBMI.pool.HR <- extractHRwithCIs(kidneyNoBMI.pool)

# HLI without physical activity 

breastNoPA.pool <- with(data=imputedMergedMids, 
                        exp=coxph(Surv(ageMenopause, ageExit, statusBreast)~ 
                                    HLInoPA + 
                                    education +
                                    HRTStatus +
                                    OCEverUse +
                                    parityCat +
                                    familyHistBC +
                                    ageMenarche +
                                    breastfeedingCat)) %>%
  pool()%>%
  summary()
breastNoPA.pool.HR <- extractHRwithCIs(breastNoPA.pool)

crcNoPA.pool <- with(data=imputedMergedMids, 
                      exp=coxph(Surv(ageEntry, ageExit, statusColorectal)~ HLInoPA + education)) %>%
  pool() %>%
  summary()

crcNoPA.pool.HR <- extractHRwithCIs(crcNoPA.pool)


lungNoPA.pool <- with(data=imputedMergedMids, 
                      exp=coxph(Surv(ageEntry, ageExit, statusLung)~ HLInoPA + education)) %>%
  pool() %>%
  summary()
lungNoPA.pool.HR <- extractHRwithCIs(lungNoPA.pool)


endometrialNoPA.pool <- with(data=imputedMergedMids, 
                              exp=coxph(Surv(ageMenopause, ageExit, statusEndometrial)~ 
                                          HLInoPA + 
                                          education +
                                          HRTStatus +
                                          OCEverUse +
                                          parityCat +
                                          ageMenarche +
                                          breastfeedingCat)) %>%
  pool()%>%
  summary()
endometrialNoPA.pool.HR <- extractHRwithCIs(endometrialNoPA.pool)

ovarianNoPA.pool <- with(data=imputedMergedMids, 
                             exp=coxph(Surv(ageMenopause, ageExit, statusOvarian)~ 
                                         HLInoPA + 
                                         education +
                                         HRTStatus +
                                         OCEverUse +
                                         parityCat +
                                         ageMenarche +
                                         breastfeedingCat)) %>%
  pool()%>%
  summary()
ovarianNoPA.pool.HR <- extractHRwithCIs(ovarianNoPA.pool)

pancreaticNoPA.pool <- with(data=imputedMergedMids, 
                      exp=coxph(Surv(ageEntry, ageExit, statusPancreatic)~ HLInoPA + education)) %>%
  pool() %>%
  summary()
pancreaticNoPA.pool.HR <- extractHRwithCIs(pancreaticNoPA.pool)


kidneyNoPA.pool <- with(data=imputedMergedMids, 
                      exp=coxph(Surv(ageEntry, ageExit, statusKidney)~ HLInoPA + education)) %>%
  pool() %>%
  summary()
kidneyNoPA.pool.HR <- extractHRwithCIs(kidneyNoPA.pool)


# HLI without alcohol

breastNoAlcohol.pool <- with(data=imputedMergedMids, 
                             exp=coxph(Surv(ageMenopause, ageExit, statusBreast)~ 
                                         HLInoAlcohol + 
                                         education +
                                         HRTStatus +
                                         OCEverUse +
                                         parityCat +
                                         familyHistBC +
                                         ageMenarche +
                                         breastfeedingCat)) %>%
  pool()%>%
  summary()
breastNoAlcohol.pool.HR <- extractHRwithCIs(breastNoAlcohol.pool)

crcNoAlcohol.pool <- with(data=imputedMergedMids, 
                     exp=coxph(Surv(ageEntry, ageExit, statusColorectal)~ HLInoAlcohol + education)) %>%
  pool() %>%
  summary()

crcNoAlcohol.pool.HR <- extractHRwithCIs(crcNoAlcohol.pool)

lungNoAlcohol.pool <- with(data=imputedMergedMids, 
                                 exp=coxph(Surv(ageEntry, ageExit, statusLung)~ HLInoAlcohol + education)) %>%
  pool() %>%
  summary()
lungNoAlcohol.pool.HR <- extractHRwithCIs(lungNoAlcohol.pool)


endometrialNoAlcohol.pool <- with(data=imputedMergedMids, 
                             exp=coxph(Surv(ageMenopause, ageExit, statusEndometrial)~ 
                                         HLInoAlcohol + 
                                         education +
                                         HRTStatus +
                                         OCEverUse +
                                         parityCat +
                                         ageMenarche +
                                         breastfeedingCat)) %>%
  pool()%>%
  summary()
endometrialNoAlcohol.pool.HR <- extractHRwithCIs(endometrialNoAlcohol.pool)


ovarianNoAlcohol.pool <- with(data=imputedMergedMids, 
                                  exp=coxph(Surv(ageMenopause, ageExit, statusOvarian)~ 
                                              HLInoAlcohol + 
                                              education +
                                              HRTStatus +
                                              OCEverUse +
                                              parityCat +
                                              ageMenarche +
                                              breastfeedingCat)) %>%
  pool()%>%
  summary()
ovarianNoAlcohol.pool.HR <- extractHRwithCIs(ovarianNoAlcohol.pool)

pancreaticNoAlcohol.pool <- with(data=imputedMergedMids, 
                           exp=coxph(Surv(ageEntry, ageExit, statusPancreatic)~ HLInoAlcohol + education)) %>%
  pool() %>%
  summary()
pancreaticNoAlcohol.pool.HR <- extractHRwithCIs(pancreaticNoAlcohol.pool)

kidneyNoAlcohol.pool <- with(data=imputedMergedMids, 
                           exp=coxph(Surv(ageEntry, ageExit, statusKidney)~ HLInoAlcohol + education)) %>%
  pool() %>%
  summary()
kidneyNoAlcohol.pool.HR <- extractHRwithCIs(kidneyNoAlcohol.pool)


# standard method below requires manual rubin's rule for CIs
#fit <- with(data=imputedMergedMids, 
#            exp=coxph(Surv(followUpTime, statusLung) ~ HLIScore))


# sensitivity analysis BMI > 18.5 ----

BMI18.5 <- filter(casesIncluded, BMI >= 18.5)

breastBMI18.5 <- multivariateBreastCox(BMI18.5, BMI18.5$HLIScore)

lungBMI18.5 <- coxph(Surv(ageEntry, ageExit, statusLung)~ HLIScore + education, data= BMI18.5)

colorectalBMI18.5 <- coxph(Surv(ageEntry, ageExit, statusColorectal)~ HLIScore + education, data= BMI18.5)

endometrialBMI18.5 <- multivariateEndometrialCox(BMI18.5, BMI18.5$HLIScore)

pancreaticBMI18.5 <- coxph(Surv(ageEntry, ageExit, statusPancreatic) ~ HLIScore + education, data = BMI18.5)

kidneyBMI18.5 <- coxph(Surv(ageEntry, ageExit, statusKidney) ~ HLIScore + education, data = BMI18.5)

