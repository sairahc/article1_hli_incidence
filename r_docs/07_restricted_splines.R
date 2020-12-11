#==============================================================
# Title: 07_restricted_splines
# Author: Sairah L. F. Chen



# Description: see rcs.Rmd for final restricted spline analysis
#       This file is just playing around w RCS

#---------------------------------
# these rms functions work 

library(rms)
library(survival)
library(tidyverse)
library(ggpubr)
library(gridExtra)
# lung, 5 knots
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline5Lung <- cph(Surv(ageEntry, ageExit, statusLung) ~ rcs(HLIScore, 5) + education, data=casesIncluded, x=TRUE, y=TRUE)

plotLung5 <- ggplot(rms:::Predict(coxspline5Lung, HLIScore=seq(1, 20),
                                  fun=exp),
                    xlab= "Healthy Lifestyle Index score",
                    ylab="Hazard Ratio") + 
  ggtitle("5 knots")

# can also plot Predict with the following
#rms:::plot.Predict(p, ~ HLIScore,
#                   col="black",
#                   col.fill=gray(seq(.8, .75, length=5)))



# lung, 4 knots
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline4Lung <- cph(Surv(ageEntry, ageExit, statusLung) ~ rcs(HLIScore, 4) + education, data=casesIncluded, x=TRUE, y=TRUE)

plotLung4 <- ggplot(rms:::Predict(coxspline4Lung, HLIScore=seq(1, 20),
                                  fun=exp),
                    xlab= "Healthy Lifestyle Index score",
                    ylab="Hazard Ratio") + 
  ggtitle("4 knots")


# lung, 3 knots
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline3Lung <- cph(Surv(ageEntry, ageExit, statusLung) ~ rcs(HLIScore, 3) + education, data=casesIncluded, x=TRUE, y=TRUE)

plotLung3 <- ggplot(rms:::Predict(coxspline3Lung, HLIScore=seq(1, 20),
                                  fun=exp),
                    xlab= "Healthy Lifestyle Index score",
                    ylab="Hazard Ratio") + 
  ggtitle("3 knots")

#lung, continuous
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxLung <- cph(Surv(ageEntry, ageExit, statusLung) ~ HLIScore + education, data=casesIncluded, x=TRUE, y=TRUE)

plotLung <- ggplot(rms:::Predict(coxLung, HLIScore=seq(1, 20),
                                 fun=exp),
                   xlab= "Healthy Lifestyle Index score",
                   ylab="Hazard Ratio") + 
  ggtitle("Linear")

# combine lung plots

pdf(file=".plots/lungSplines.pdf")
# width = 8,
# height = 8)
lungSplineOptions <- ggarrange(plotLung5,
                               plotLung4,
                               plotLung3,
                               plotLung,
                               ncol=2, nrow=2)
annotate_figure(lungSplineOptions,
                top = text_grob("Lung cancer HRs modeled by various HLI-score spline options",
                                size = 16))
dev.off()

# make a matrix for lung AIC values

lungAIC <- cbind(c("5 knots", "4 knots", "3 knots", "linear"), 
                 c(AIC(coxspline5Lung), 
                   AIC(coxspline4Lung), 
                   AIC(coxspline3Lung), 
                   AIC(coxLung)))

#this does the same thing as ggarrange
grid.arrange(plotLung5,
             plotLung4,
             plotLung3,
             plotLung,
             lungAIC,
             ncol=3, nrow=2)

#crc ----

# crc, 5 knots
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline5Colorectal <- cph(Surv(ageEntry, ageExit, statusColorectal) ~ rcs(HLIScore, 5) + education, data=casesIncluded, x=TRUE, y=TRUE)

plotColorectal5 <- ggplot(rms:::Predict(coxspline5Colorectal, HLIScore=seq(1, 20),
                                        fun=exp),
                          xlab= "Healthy Lifestyle Index score",
                          ylab="Hazard Ratio") + 
  ggtitle("5 knots")

# crc, 4 knots
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline4Colorectal <- cph(Surv(ageEntry, ageExit, statusColorectal) ~ rcs(HLIScore, 4) + education, data=casesIncluded, x=TRUE, y=TRUE)

plotColorectal4 <- ggplot(rms:::Predict(coxspline4Colorectal, HLIScore=seq(1, 20),
                                        fun=exp),
                          xlab= "Healthy Lifestyle Index score",
                          ylab="Hazard Ratio") + 
  ggtitle("4 knots")

#crc, 3 knots
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline3Colorectal <- cph(Surv(ageEntry, ageExit, statusColorectal) ~ rcs(HLIScore, 3) + education, data=casesIncluded, x=TRUE, y=TRUE)

plotColorectal3 <- ggplot(rms:::Predict(coxspline3Colorectal, HLIScore=seq(1, 20),
                                        fun=exp),
                          xlab= "Healthy Lifestyle Index score",
                          ylab="Hazard Ratio") + 
  ggtitle("3 knots")


# crc, linear
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxColorectal <- cph(Surv(ageEntry, ageExit, statusColorectal) ~ HLIScore + education, data=casesIncluded, x=TRUE, y=TRUE)

plotColorectal <- ggplot(rms:::Predict(coxColorectal, HLIScore=seq(1, 20),
                                       fun=exp),
                         xlab= "Healthy Lifestyle Index score",
                         ylab="Hazard Ratio") + 
  ggtitle("Linear")


pdf(file="./plots/colorectalSplines.pdf")



colorectalSplineOptions <- ggarrange(plotColorectal5,
                                     plotColorectal4,
                                     plotColorectal3,
                                     plotColorectal,
                                     ncol=2, nrow=2)
annotate_figure(colorectalSplineOptions,
                top = text_grob("Colorectal cancer HRs modeled by various HLI-score spline options",
                                size = 16))
dev.off()

# make a matrix for crc AIC values

colorectalAIC <- cbind(c("5 knots", "4 knots", "3 knots", "linear"), 
                       c(AIC(coxspline5Colorectal), 
                         AIC(coxspline4Colorectal), 
                         AIC(coxspline3Colorectal), 
                         AIC(coxColorectal)))

# breast ----

# breast, 5 knots
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline5Breast <- cph(Surv(ageMenopause, ageExit, statusBreast) ~ rcs(HLIScore, 5) + 
                          education +
                          HRTStatus +
                          OCEverUse +
                          parityCat +
                          familyHistBC +
                          ageMenarche, data=casesIncluded, x=TRUE, y=TRUE)

plotBreast5 <- ggplot(rms:::Predict(coxspline5Breast, HLIScore=seq(1, 20),
                                    fun=exp),
                      xlab= "Healthy Lifestyle Index score",
                      ylab="Hazard Ratio") + 
  ggtitle("5 knots")


# breast, 4 knots
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline4Breast <- cph(Surv(ageMenopause, ageExit, statusBreast) ~ rcs(HLIScore, 4) + 
                          education +
                          HRTStatus +
                          OCEverUse +
                          parityCat +
                          familyHistBC +
                          ageMenarche, data=casesIncluded, x=TRUE, y=TRUE)

plotBreast4 <- ggplot(rms:::Predict(coxspline4Breast, HLIScore=seq(1, 20),
                                    fun=exp),
                      xlab= "Healthy Lifestyle Index score",
                      ylab="Hazard Ratio") + 
  ggtitle("4 knots")

# breast, 3 knots
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline3Breast <- cph(Surv(ageMenopause, ageExit, statusBreast) ~ rcs(HLIScore, 3) + 
                          education +
                          HRTStatus +
                          OCEverUse +
                          parityCat +
                          familyHistBC +
                          ageMenarche, data=casesIncluded, x=TRUE, y=TRUE)

plotBreast3 <- ggplot(rms:::Predict(coxspline3Breast, HLIScore=seq(1, 20),
                                    fun=exp),
                      xlab= "Healthy Lifestyle Index score",
                      ylab="Hazard Ratio") + 
  ggtitle("3 knots")

# breast, linear
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxBreast <- cph(Surv(ageMenopause, ageExit, statusBreast) ~ HLIScore + 
                   education +
                   HRTStatus +
                   OCEverUse +
                   parityCat +
                   familyHistBC +
                   ageMenarche, data=casesIncluded, x=TRUE, y=TRUE)

plotBreast <- ggplot(rms:::Predict(coxBreast, HLIScore=seq(1, 20),
                                   fun=exp),
                     xlab= "Healthy Lifestyle Index score",
                     ylab="Hazard Ratio") + 
  ggtitle("Linear")            

# arrange breast plots 
pdf(file="./plots/breastSplines.pdf")

breastSplineOptions <- ggarrange(plotBreast5,
                                 plotBreast4,
                                 plotBreast3,
                                 plotBreast,
                                 ncol=2, nrow=2)

annotate_figure(breastSplineOptions,
                top = text_grob("Breast cancer HRs modeled by various HLI-score spline options",
                                size = 16))
dev.off()

breastAIC <- cbind(c("5 knots", "4 knots", "3 knots", "linear"), 
                   c(AIC(coxspline5Breast), 
                     AIC(coxspline4Breast), 
                     AIC(coxspline3Breast), 
                     AIC(coxBreast)))

# endometrial ----
# endometrial, 5 knots
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline5Endometrial <- cph(Surv(ageMenopause, ageExit, statusEndometrial) ~ rcs(HLIScore, 5) + 
                               education +
                               HRTStatus +
                               OCEverUse +
                               parityCat +
                               ageMenarche, data=casesIncluded, x=TRUE, y=TRUE)

plotEndometrial5 <- ggplot(rms:::Predict(coxspline5Endometrial, HLIScore=seq(1, 20),
                                         fun=exp),
                           xlab= "Healthy Lifestyle Index score",
                           ylab="Hazard Ratio") + 
  ggtitle("5 knots")


# Endometrial, 4 knots
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline4Endometrial <- cph(Surv(ageMenopause, ageExit, statusEndometrial) ~ rcs(HLIScore, 4) + 
                               education +
                               HRTStatus +
                               OCEverUse +
                               parityCat +
                               ageMenarche, data=casesIncluded, x=TRUE, y=TRUE)

plotEndometrial4 <- ggplot(rms:::Predict(coxspline4Endometrial, HLIScore=seq(1, 20),
                                         fun=exp),
                           xlab= "Healthy Lifestyle Index score",
                           ylab="Hazard Ratio") + 
  ggtitle("4 knots")

# Endometrial, 3 knots
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline3Endometrial <- cph(Surv(ageMenopause, ageExit, statusEndometrial) ~ rcs(HLIScore, 3) + 
                               education +
                               HRTStatus +
                               OCEverUse +
                               parityCat +
                               ageMenarche, data=casesIncluded, x=TRUE, y=TRUE)

plotEndometrial3 <- ggplot(rms:::Predict(coxspline3Endometrial, HLIScore=seq(1, 20),
                                         fun=exp),
                           xlab= "Healthy Lifestyle Index score",
                           ylab="Hazard Ratio") + 
  ggtitle("3 knots")

# Endometrial, continous
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxEndometrial <- cph(Surv(ageMenopause, ageExit, statusEndometrial) ~ HLIScore + 
                        education +
                        HRTStatus +
                        OCEverUse +
                        parityCat +
                        ageMenarche, data=casesIncluded, x=TRUE, y=TRUE)

plotEndometrial <- ggplot(rms:::Predict(coxEndometrial, HLIScore=seq(1, 20),
                                        fun=exp),
                          xlab= "Healthy Lifestyle Index score",
                          ylab="Hazard Ratio") + 
  ggtitle("Continuous")            

# arrange Endometrial plots 
pdf(file="./plots/endometrialSplines.pdf")

endometrialSplineOptions <- ggarrange(plotEndometrial5,
                                      plotEndometrial4,
                                      plotEndometrial3,
                                      plotEndometrial,
                                      ncol=2, nrow=2)

annotate_figure(endometrialSplineOptions,
                top = text_grob("Endometrial cancer HRs modeled by various HLI-score spline options",
                                size = 16))
dev.off()

endometrialAIC <- cbind(c("5 knots", "4 knots", "3 knots", "linear"), 
                        c(AIC(coxspline5Endometrial), 
                          AIC(coxspline4Endometrial), 
                          AIC(coxspline3Endometrial), 
                          AIC(coxEndometrial)))


#kidney ----

# Kidney, 5 knots
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline5Kidney <- cph(Surv(ageEntry, ageExit, statusKidney) ~ rcs(HLIScore, 5) + education, data=casesIncluded, x=TRUE, y=TRUE)

plotKidney5 <- ggplot(rms:::Predict(coxspline5Kidney, HLIScore=seq(1, 20),
                                    fun=exp),
                      xlab= "Healthy Lifestyle Index score",
                      ylab="Hazard Ratio") + 
  ggtitle("5 knots")

# kidney, 4 knots
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline4Kidney <- cph(Surv(ageEntry, ageExit, statusKidney) ~ rcs(HLIScore, 4) + education, data=casesIncluded, x=TRUE, y=TRUE)

plotKidney4 <- ggplot(rms:::Predict(coxspline4Kidney, HLIScore=seq(1, 20),
                                    fun=exp),
                      xlab= "Healthy Lifestyle Index score",
                      ylab="Hazard Ratio") + 
  ggtitle("4 knots")

#kidney, 3 knots
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline3Kidney <- cph(Surv(ageEntry, ageExit, statusKidney) ~ rcs(HLIScore, 3) + education, data=casesIncluded, x=TRUE, y=TRUE)

plotKidney3 <- ggplot(rms:::Predict(coxspline3Kidney, HLIScore=seq(1, 20),
                                    fun=exp),
                      xlab= "Healthy Lifestyle Index score",
                      ylab="Hazard Ratio") + 
  ggtitle("3 knots")


# kidney, continuous
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxKidney <- cph(Surv(ageEntry, ageExit, statusKidney) ~ HLIScore + education, data=casesIncluded, x=TRUE, y=TRUE)

plotKidney <- ggplot(rms:::Predict(coxKidney, HLIScore=seq(1, 20),
                                   fun=exp),
                     xlab= "Healthy Lifestyle Index score",
                     ylab="Hazard Ratio") + 
  ggtitle("Continuous")


pdf(file="./plots/kidneySplines.pdf")

kidneySplineOptions <- ggarrange(plotKidney5,
                                 plotKidney4,
                                 plotKidney3,
                                 plotKidney,
                                 ncol=2, nrow=2)
annotate_figure(kidneySplineOptions,
                top = text_grob("Kidney cancer HRs modeled by various HLI-score spline options",
                                size = 16))
dev.off()

# make a matrix for endometrial AIC values

kidneyAIC <- cbind(c("5 knots", "4 knots", "3 knots", "linear"), 
                   c(AIC(coxspline5Kidney), 
                     AIC(coxspline4Kidney), 
                     AIC(coxspline3Kidney), 
                     AIC(coxKidney)))

# pancreatic ----
# Pancreatic, 5 knots
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline5Pancreatic <- cph(Surv(ageEntry, ageExit, statusPancreatic) ~ rcs(HLIScore, 5) + education, data=casesIncluded, x=TRUE, y=TRUE)

plotPancreatic5 <- ggplot(rms:::Predict(coxspline5Pancreatic, HLIScore=seq(1, 20),
                                        fun=exp),
                          xlab= "Healthy Lifestyle Index score",
                          ylab="Hazard Ratio") + 
  ggtitle("5 knots")

# crc, 4 knots
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline4Pancreatic <- cph(Surv(ageEntry, ageExit, statusPancreatic) ~ rcs(HLIScore, 4) + education, data=casesIncluded, x=TRUE, y=TRUE)

plotPancreatic4 <- ggplot(rms:::Predict(coxspline4Pancreatic, HLIScore=seq(1, 20),
                                        fun=exp),
                          xlab= "Healthy Lifestyle Index score",
                          ylab="Hazard Ratio") + 
  ggtitle("4 knots")

#crc, 3 knots
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline3Pancreatic <- cph(Surv(ageEntry, ageExit, statusPancreatic) ~ rcs(HLIScore, 3) + education, data=casesIncluded, x=TRUE, y=TRUE)

plotPancreatic3 <- ggplot(rms:::Predict(coxspline3Pancreatic, HLIScore=seq(1, 20),
                                        fun=exp),
                          xlab= "Healthy Lifestyle Index score",
                          ylab="Hazard Ratio") + 
  ggtitle("3 knots")


# crc, continuous
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxPancreatic <- cph(Surv(ageEntry, ageExit, statusPancreatic) ~ HLIScore + education, data=casesIncluded, x=TRUE, y=TRUE)

plotPancreatic <- ggplot(rms:::Predict(coxPancreatic, HLIScore=seq(1, 20),
                                       fun=exp),
                         xlab= "Healthy Lifestyle Index score",
                         ylab="Hazard Ratio") + 
  ggtitle("Continuous")


pdf(file="./plots/pancreaticSplines.pdf")

pancreaticSplineOptions <- ggarrange(plotPancreatic5,
                                     plotPancreatic4,
                                     plotPancreatic3,
                                     plotPancreatic,
                                     ncol=2, nrow=2)
annotate_figure(pancreaticSplineOptions,
                top = text_grob("Pancreatic cancer HRs modeled by various HLI-score spline options",
                                size = 16))
dev.off()



# make a matrix for endometrial AIC values

pancreaticAIC <- cbind(c("5 knots", "4 knots", "3 knots", "linear"), 
                       c(AIC(coxspline5Pancreatic), 
                         AIC(coxspline4Pancreatic), 
                         AIC(coxspline3Pancreatic), 
                         AIC(coxPancreatic)))

# set knots to 5 HLI score intervals ----
ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline5LungSetKnotLocation <- cph(Surv(ageEntry, ageExit, statusLung) ~ rcs(HLIScore, c(1,5,10,15,20)) + education, data=casesIncluded, x=TRUE, y=TRUE)

plotLung5SetKnotLocation <- ggplot(rms:::Predict(coxspline5LungSetKnotLocation, HLIScore=seq(1, 20),
                                                 fun=exp),
                                   xlab= "Healthy Lifestyle Index score",
                                   ylab="Hazard Ratio") + 
  ggtitle("Lung 5 knots set at 5 HLI score unit intervals")


ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline5ColorectalSetKnotLocation <- cph(Surv(ageEntry, ageExit, statusColorectal) ~ rcs(HLIScore, c(1,5,10,15,20)) + education, data=casesIncluded, x=TRUE, y=TRUE)

plotColorectal5SetKnotLocation <- ggplot(rms:::Predict(coxspline5ColorectalSetKnotLocation, HLIScore=seq(1, 20),
                                                       fun=exp),
                                         xlab= "Healthy Lifestyle Index score",
                                         ylab="Hazard Ratio") + 
  ggtitle("CRC 5 knots set at 5 HLI score unit intervals")


ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline5BreastSetKnotLocation <- cph(Surv(ageMenopause, ageExit, statusBreast) ~ rcs(HLIScore, c(1,5,10,15,20)) + 
                                         education +
                                         HRTStatus +
                                         OCEverUse +
                                         parityCat +
                                         familyHistBC +
                                         ageMenarche, data=casesIncluded, x=TRUE, y=TRUE)

plotBreast5SetKnotLocation <- ggplot(rms:::Predict(coxspline5BreastSetKnotLocation, HLIScore=seq(1, 20),
                                                   fun=exp),
                                     xlab= "Healthy Lifestyle Index score",
                                     ylab="Hazard Ratio") + 
  ggtitle("Breast 5 knots set at 5 HLI score unit intervals")


pdf(file="./plots/splineSetKnotLocations.pdf")
splineSetKnotLocations <- ggarrange(plotLung5SetKnotLocation,
                                    plotColorectal5SetKnotLocation,
                                    plotBreast5SetKnotLocation,
                                    ncol=2, nrow=2)
dev.off()

# testing for linearity ----
# test model comparison of log-likelihood (known as deviance)
# of two models

# --------------------------------------------------------------------------
# fooling around with other spline packages----
library(rstpm2)
library(splines)
library(survival)
library(tidyverse)
# do not run ----
# only creates splines for hazard-time model

# lung cancer 
rcs_k3_lung <- stpm2(Surv(ageEntry, ageExit, statusLung) ~ HLIScore,
                              data = casesIncluded,
                              df=3)
summary(rcs_k3_lung)


# compare fit of rcs model with regular cox
AIC(rcs_k3_lung, completeCoxCrudeLung)


head(predict(rcs_k3_lung,se.fit=TRUE,type="surv"))
head(predict(rcs_k3_lung,se.fit=TRUE,type="hazard"))


plot(rcs_k3_lung,newdata=data.frame(HLIScore=0),type="hazard") #plotting hazard against time"
plot(rcs_k3_lung,newdata=data.frame(HLIScore=0),type="cumhaz") #against time

# how to plot hazard against HLI score ----
#       seems we need to extract hazard
# take HR from survfit
cumhaz <- survfit(Surv(ageEntry, ageExit, statusLung) ~ 1, data=casesIncluded)
# regress HLI on cumhaz


#### USE nsx() to generate B-spline basis matrix for a natural cubic spline of nsxD() for derivative of natural cubic splines 
ns <- nsx(casesIncluded$HLIScore, 
          knots = 3)
rcs <- stpm2(Surv(ageEntry, ageExit, statusLung) ~ HLIScore, # this errors bc variable lengths differ
             smooth.formula= ~ nsx(casesIncluded$HLIScore, df=2), 
             data=casesIncluded)
# try creating dataframe with only complete HLI score obs
casesIncludedComplete <- casesIncluded[!is.na(casesIncluded$HLIScore), ]
nsComplete <- nsx(casesIncludedComplete$HLIScore, df = 2)
rcs <- stpm2(Surv(ageEntry, ageExit, statusLung) ~ HLIScore, # this also errors :(
             smooth.formula= ~ nsx(casesIncludedComplete$HLIScore, df=2), 
             data=casesIncludedComplete)

# place into cox, this works!!
knots_x <- quantile(x, probs=c(0, 1, 0.2))
ns(x, knots=knots_x)
ns(casesIncluded$HLIScore)
coxspline5 <- coxph(Surv(ageEntry, ageExit, statusLung) ~ ns(HLIScore, df=4) + education, data=casesIncluded)
coxspline4 <- coxph(Surv(ageEntry, ageExit, statusLung) ~ ns(casesIncluded$HLIScore, df=3) + education, data=casesIncluded)
coxspline3 <- coxph(Surv(ageEntry, ageExit, statusLung) ~ ns(casesIncluded$HLIScore, df=2) + education, data=casesIncluded)

library(Hmisc)
coxspline5plot <- rcspline.plot() # this doesn't make sense, can't use coxmodel in ?
coxspline5plot <- ggsurvplot(Survfit(coxspline5), data=casesIncluded) # this makes something

# gam route, can be inserted into smooth.formula of stpm2 
library(mgcv)
coxGam <- cox.ph()

# termplot route - this plots one covariate against another(others?)
termplot <- termplot(coxspline5, se=TRUE, term=2, col.term=1, col.se=1, xlab="HLI Score" ) #not sure what this produced

# smoothHR
library(smoothHR)
coxspline5HR <- smoothHR(data=casesIncluded, 
                         time="ageEntry", 
                         time2="ageExit",
                         status= "statusLung",
                         formula=~ ns(casesIncluded$HLIScore, df=4)+education)
coxspline5HR <- smoothHR::smoothHR(data=casesIncluded, 
                                   coxfit=coxspline5)
smoothHR <- plot.HR(survfit(coxspline5HR), 
                 predictor="HLIScore",
                 prob=0) 

#rms for cph
library(rms)
survObject <- Surv(casesIncluded$ageEntry, casesIncluded$ageExit, casesIncluded$statusLung)
spline <- rcs(casesIncluded$HLIScore, 5)
edu <- casesIncluded$education
coxspline5rms <- cph(survObject ~ spline + edu, x=TRUE, y=TRUE)



datadist <- datadist(casesIncluded$HLIScore
) #requirement for summary to work 
options(datadist="datadist") 
coxspline5rms <- cph(Surv(ageEntry, ageExit, statusLung) ~ rcs(HLIScore, 5) + education, 
                     data=casesIncluded,
                     x=TRUE,
                     y=TRUE)

#requirement for summary to work, probably why Predict is not working 
summary(coxspline5rms)
plot(Predict(coxspline5rms, casesIncluded$HLIScore)) #https://stackoverflow.com/questions/28385509/how-to-plot-a-cox-hazard-model-with-splines
Predict(coxspline5rms, casesIncluded$HLIScore=seq(0,20,by=0.1)) # https://stackoverflow.com/questions/54416953/why-does-ggplot-predict-not-plot-in-r-rms-package
# see error in Predict source code


# Greg::plotHR
library(Greg)

# boundary knots appear to be off
coxspline5boundaries <- coxph(Surv(ageEntry, ageExit, statusLung) ~ 
                      ns(HLIScore, 
                         df=4,
                         Boundary.knots = c(1,20)) + 
                      education, 
                    data=casesIncludedComplete)

coxspline5 <- coxph(Surv(ageEntry, ageExit, statusLung) ~ 
                      ns(HLIScore, df=4) + 
                      education, 
                    data=casesIncludedComplete)


plot3Interior <- plotHR(
  coxspline5, # the key is using a dataframe with complete values of x
  term="HLIScore", plot.bty="o",
  xlab = "Healthy lifestyle index score",
  xlim =c(0,20) 
)

plot1Interior <- plotHR(
  coxspline3, # the key is using a dataframe with complete values of x
  term="HLIScore", plot.bty="o",
  xlab = "Healthy lifestyle index score",
  xlim =c(0,20) 
)




coxspline4 <- coxph(Surv(ageEntry, ageExit, statusLung) ~ 
                      ns(HLIScore, df=3) + 
                      education, 
                    data=casesIncludedComplete)

plot2Interior <- plotHR(
  coxspline4, # the key is using a dataframe with complete values of x
  term="HLIScore", plot.bty="o",
  xlab = "Healthy lifestyle index score",
  xlim = c(0,20)
)

