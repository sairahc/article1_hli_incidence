# START
#----

library(tidyverse)
library(lubridate)

df <- read.csv2("C:/Users/sch044/OneDrive - UiT Office 365/R/Data/Data150420.csv")




# Create ID variable
df$myID <- 1:nrow(df)

# Changing variable names 
df$physicalActivity <- df$AKTIDAG
df$height <- df$HOYDE
df$weight <- df$VEKTANA
df$smokingStatus <- df$ROYKSTAT
df$grMusli <- df$grfrubla
df$grYogurt <- df$gryoghur
df$energyIntake <- df$totkjoul
df$ageEntry <- df$STARTALD
df$education <- df$SKOLE
df$breastfeedMths <- df$AMME
df$ageMenarche <- df$MENSALD
df$parity <- df$ANTBARN


#EXCLUSIONS
#----


# Format dates 
# Dates are originally 6 digit integers


# Create function to parse 1900 and 2000 at year "19"
parseYear19 <- function(dateAsInteger){
  sixDigitInteger <- sprintf("%06d", dateAsInteger) #left-pad with zeros if less than 6 digits
  dateAsDate <- as.Date(as.character(sixDigitInteger), "%d%m%y")
  a <- year(dateAsDate) %% 100
  b <- ifelse(
    a > 19, 
    1900 + a, 
    2000 + a)
  monthFromDate <- sprintf("%02d", month(dateAsDate))
  dayFromDate <- day(dateAsDate)
  return(as.Date(
    paste(b, 
          monthFromDate, 
          dayFromDate,
          sep = ""),
    "%Y%m%d")
  )
}

df$deathDate <- parseYear19(df$DODDT)
df$emigrationDate <- parseYear19(df$EMIGDT)
df$diagnosisDate <- parseYear19(df$DIAGDAT)
df$entryDate <- parseYear19(df$STARTDAT)


#df$dead <- ifelse(df$deathDate - df$entryDate <= 0, 
#                   1,
#                   0) 
#df$emigrated <- ifelse(df$emigrationDate - df$entryDate <= 0, 
#                   1,
#                   0) 
#df$prevalentCancer <- ifelse(df$diagnosisDate - df$entryDate <= 0, 
#                              1,
#                              0) 

#dead <- df[df$dead==1 & !is.na(df$dead), ]
#dead$DeathMinusEntry <- dead$deathDate - dead$entryDate

#df1 <- filter(df, dead==0 | is.na(dead))
#emigrated <- filter(df1, emigrated == 1)
#df2 <- filter(df1, emigrated==0 | is.na(emigrated))
#df3 <- filter(df2, prevalentCancer==0 | is.na(prevalentCancer))


# Returns dataframe with only inclusions----
makeExclusions <- function(dataframe){
    
      prevalentCancer <- ifelse(dataframe$diagnosisDate - 
                                  dataframe$entryDate <= 0,
                                      1,
                                      0)
      prevalentDeath <-  ifelse(dataframe$deathDate - 
                                  dataframe$entryDate <= 0,
                              1,
                              0)
      prevalentEmigration <-  ifelse(dataframe$emigrationDate - 
                                       dataframe$entryDate <= 0,
                                   1,
                                   0)
      
    return(
        filter(dataframe, ((prevalentCancer == 0 | is.na(prevalentCancer)) &
                           (prevalentDeath == 0 | is.na(prevalentDeath)) &
                           (prevalentEmigration == 0 | is.na(prevalentEmigration)) &
                           (dataframe$energyIntake < 15000 |
                            dataframe$energyIntake > 2100)
  )))
}

casesIncluded <- makeExclusions(df)
















