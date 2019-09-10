#SETTING WORKING DIRECTORY AND LOADING NECESSARY LIBRARIES

setwd("F:\\work\\Data Wrangling In R\\Water Quality")
library(tidyverse)
library(lubridate)
library(stringr)


#IMPORTING DATASET

water <- read_csv("austinwater.csv")
glimpse(water)


#OPTIMIZING DATASET [EXCLUDED SOME VARIABLES AND RENAMED SOME VARIABLES]

water <- tibble('sitename' = water$SITE_NAME,
                'siteType' = water$SITE_TYPE,
                'sampleTime' = water$SAMPLE_DATE,
                'parameterType' = water$PARAM_TYPE,
                'parameter' = water$PARAMETER,
                'results' = water$RESULT,
                'units' = water$UNIT)
glimpse(water)


#FILTERING THE DATASET ON THE BASIS OF PH AND WATER TEMPERATURE

unique(water$parameterType)
filtered_water <- subset(water, (parameterType == 'Alkalinity/Hardness/pH') | 
                           (parameterType == 'Conventionals'))

unique(filtered_water$parameter)
filtered_water <- subset(water, (parameter == 'PH') | (parameter
                                     == 'WATER TEMPERATURE'))

glimpse(filtered_water)
summary(filtered_water)

#ASSIGINING APPROPIATE DATA TYPES

filtered_water$siteType <- as.factor(filtered_water$siteType)
filtered_water$parameterType <- as.factor(filtered_water$parameterType)
filtered_water$parameter <- as.factor(filtered_water$parameter)
filtered_water$units <- as.factor(filtered_water$units)
filtered_water$sampleTime <- mdy_hms(filtered_water$sampleTime)

#FIXING SOME ISSUES WITH UNITS VARIABLE

#1
subset(filtered_water, units == 'Feet')
which(filtered_water$units == 'Feet')
filtered_water$units[10520] <- 'Deg. Fahrenheit'

#2
subset(filtered_water, units == 'MG/L')
subset(filtered_water, units == 'MG/L' & parameter == 'PH')
convert <- which(filtered_water$units == 'MG/L' & filtered_water$parameter == 'PH')
filtered_water$units[convert] <- "Standard units"

#3
glimpse(subset(filtered_water, units == 'MG/L'))
glimpse(subset(filtered_water, units == 'MG/L' & results > 70))
convert <- which(filtered_water$units == 'MG/L' & filtered_water$results > 70)
filtered_water$units[convert] <- "Deg. Fahrenheit"

#4
convert <- which(filtered_water$units == 'MG/L')
filtered_water$units[convert] <- "Deg. Celsius"

summary(filtered_water)

#VISUALIZING DATA AND DETECTING OUTLIERS

#SCATTERPLOT
ggplot(filtered_water , aes(x = sampleTime, y= results))+
  geom_point()

#FINDING OUTLIER AND REMOVING IT FROM DATASET

#1
glimpse(subset(filtered_water, results > 1000000))
remove <- which(filtered_water$results > 1000000 | is.na(filtered_water$results))
filtered_water <- filtered_water[-remove,]

#2
glimpse(subset(filtered_water, results > 1000))
remove <- which(filtered_water$results > 1000)
filtered_water <- filtered_water[-remove,]

summary(filtered_water)

#BOXPLOT
ggplot(filtered_water , aes(x = units , y= results))+
  geom_boxplot()

#CONVERTING ALL FAHRENHEIT TEMP. TO DEGREE CELSIUS TEMP.

convert <- which(filtered_water$results > 60 &
                   filtered_water$units == 'Deg. Celsius')

filtered_water$units[convert] <- 'Deg. Fahrenheit' 
fahrenheit <- which(filtered_water$units == 'Deg. Fahrenheit')

filtered_water$results[fahrenheit] <- (filtered_water$results[fahrenheit] -32) *
  (5.0/9.0)

filtered_water$units[fahrenheit] <- 'Deg. Celsius'

summary(filtered_water)

filtered_water$units <- droplevels(filtered_water$units)

#BOXPLOT
ggplot(filtered_water , aes(x = units , y= results))+
  geom_boxplot()


#MAKING DATASET WIDE USING SPREAD FUNCTION

filtered_water <- filtered_water[,-4]
filtered_water_wide <- spread(filtered_water, parameter, results)

dupes_check <- filtered_water[,-5]
dupes <- which(duplicated(dupes_check))

filtered_water <- filtered_water[-dupes,]
filtered_water_wide <- spread(filtered_water, parameter, results)

glimpse(filtered_water_wide)

colnames(filtered_water_wide)[4] <- 'pH'
colnames(filtered_water_wide)[5] <- 'temp'
