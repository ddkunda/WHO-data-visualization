library(plyr)
library(shiny)
library(tidyverse)
library(png)
library(grid)
library(gridExtra)
library(rsconnect)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(reshape2)
library(knitr)
library(sendmailR)
library(shinyAce)
library(mailR)
library(googlesheets)
library(DT)
#library(shinydashboard)
#library(shinydashboardPlus)


############################################################################################################################################                 
############################################################################################################################################                 

## load data
file.list <- list.files("../WHO_burden_and_mortality_visualization/raw datasets/",
                        recursive=T,
                        pattern='*.RData',
                        full.names = TRUE)

for (j in 1:length(file.list)){ 
  load(file.list[j])
}

############################################################################################################################################                 
############################################################################################################################################                 

## import translation bin
translationContent <- read.delim("../WHO_burden_and_mortality_visualization/translation and dictionary/dictionary.csv", header = TRUE, sep = ",", as.is = TRUE) 
translation <- dlply(translationContent ,.(key), function(s) key = as.list(s))
save(translation, file = "../WHO_burden_and_mortality_visualization/translation and dictionary/translation.bin")
load("../WHO_burden_and_mortality_visualization/translation and dictionary/translation.bin") 

## import country and DiseaseGroup translation
country_translation <- read.csv("../WHO_burden_and_mortality_visualization/translation and dictionary/country_translation.csv", header = TRUE)
DiseaseGroup_translation <- read.csv("../WHO_burden_and_mortality_visualization/translation and dictionary/DiseaseGroup_translation.csv", header = TRUE)


############################
## dataset for 1st tab plot
############################
country_geo_code <- country_geo_code[!country_geo_code$Year %in% "2016", ]
country_geo_code <- country_geo_code[!country_geo_code$Latitude %in% NA, ]
country_geo_code <- merge(country_geo_code, country_translation, by="Country", all.x = TRUE)

## french
country_geo_code$AVAI <- country_geo_code$DALY
country_geo_code$APVP <- country_geo_code$YLL
country_geo_code$AVI <- country_geo_code$YLD
country_geo_code$Mortalité <- country_geo_code$Mortality

## spanish
country_geo_code$AVAD <- country_geo_code$DALY
country_geo_code$AVP <- country_geo_code$YLL
country_geo_code$APD <- country_geo_code$YLD
country_geo_code$Mortalidad <- country_geo_code$Mortality



############################
## dataset for 2nd tab plot
############################
country_dis_categ <- merge(country_dis_categ, country_translation, by="Country", all.x = TRUE)

## french
country_dis_categ$AVAI <- country_dis_categ$DALY
country_dis_categ$APVP <- country_dis_categ$YLL
country_dis_categ$AVI <- country_dis_categ$YLD
country_dis_categ$Mortalité <- country_dis_categ$Mortality
country_dis_categ$Année <- country_dis_categ$Year
country_dis_categ$Sexe <- country_dis_categ$Sex
country_dis_categ$Sexe[country_dis_categ$Sexe %in% "Male"] <- "Masculin"
country_dis_categ$Sexe[country_dis_categ$Sexe %in% "Female"] <- "Féminin"
country_dis_categ$Causes <- country_dis_categ$Cause
country_dis_categ$Causes[country_dis_categ$Causes %in% "All Causes"] <- "Toutes les causes"
country_dis_categ$Causes[country_dis_categ$Causes %in% "Group I"] <- "Groupe I"
country_dis_categ$Causes[country_dis_categ$Causes %in% "Group II"] <- "Groupe II"
country_dis_categ$Causes[country_dis_categ$Causes %in% "Group III"] <- "Groupe III"
country_dis_categ$Causes <- factor(country_dis_categ$Causes,levels = c("Toutes les causes", "Groupe I", "Groupe II", "Groupe III"))

## spanish
country_dis_categ$AVAD <- country_dis_categ$DALY
country_dis_categ$AVP <- country_dis_categ$YLL
country_dis_categ$APD <- country_dis_categ$YLD
country_dis_categ$Mortalidad <- country_dis_categ$Mortality
country_dis_categ$Año <- country_dis_categ$Year
country_dis_categ$Sexo <- country_dis_categ$Sex
country_dis_categ$Sexo[country_dis_categ$Sexo %in% "Male"] <- "Masculino"
country_dis_categ$Sexo[country_dis_categ$Sexo %in% "Female"] <- "Femenino"
country_dis_categ$Causa <- country_dis_categ$Cause
country_dis_categ$Causa[country_dis_categ$Causa %in% "All Causes"] <- "Todas las causas"
country_dis_categ$Causa[country_dis_categ$Causa %in% "Group I"] <- "Groupo I"
country_dis_categ$Causa[country_dis_categ$Causa %in% "Group II"] <- "Groupo II"
country_dis_categ$Causa[country_dis_categ$Causa %in% "Group III"] <- "Groupo III"
country_dis_categ$Causa <- factor(country_dis_categ$Causa,levels = c("Todas las causas", "Groupo I", "Groupo II", "Groupo III"))


country_dis_categ <- country_dis_categ[order(country_dis_categ$Country), ]
country_dis_categ1 <- country_dis_categ[country_dis_categ$Country %in% "Global", ]
country_dis_categ2 <- country_dis_categ[!country_dis_categ$Country %in% "Global", ]
country_dis_categ <- rbind(country_dis_categ1, country_dis_categ2)


############################
## dataset for 3rd tab plot
############################
# country_dis_categ Same as for 2nd TAB


############################
## dataset for 4th tab plot
############################
country_age_categ <- merge(country_age_categ, country_translation, by="Country", all.x = TRUE)

## french
country_age_categ$AVAI <- country_age_categ$DALY
country_age_categ$APVP <- country_age_categ$YLL
country_age_categ$AVI <- country_age_categ$YLD
country_age_categ$Mortalité <- country_age_categ$Mortality
country_age_categ$Année <- country_age_categ$Year
country_age_categ$Âge <- country_age_categ$Age
country_age_categ$Âge <- gsub("years", "ans",country_age_categ$Âge)
country_age_categ$Âge <- factor(country_age_categ$Âge, levels =  c("0-4 ans","5-14 ans","15-29 ans", "30-49 ans","50-59 ans","60-69 ans", "70+ ans")) 

## spanish
country_age_categ$AVAD <- country_age_categ$DALY
country_age_categ$AVP <- country_age_categ$YLL
country_age_categ$APD <- country_age_categ$YLD
country_age_categ$Mortalidad <- country_age_categ$Mortality
country_age_categ$Año <- country_age_categ$Year
country_age_categ$Edad <- country_age_categ$Age
country_age_categ$Edad <- gsub("years", "años",country_age_categ$Edad)
country_age_categ$Edad <- factor(country_age_categ$Edad, levels =  c("0-4 años","5-14 años","15-29 años", "30-49 años","50-59 años","60-69 años", "70+ años")) 


country_age_categ <- country_age_categ[order(country_age_categ$Country), ]
country_age_categ1 <- country_age_categ[country_age_categ$Country %in% "Global", ]
country_age_categ2 <- country_age_categ[!country_age_categ$Country %in% "Global", ]
country_age_categ <- rbind(country_age_categ1, country_age_categ2)


############################
## dataset for 5th tab plot
############################
region_dis_categ <- region_dis_categ[!region_dis_categ$Region %in% "Global", ]
names(region_dis_categ)[2] <- "Cause"

## french
region_dis_categ$Région <- region_dis_categ$Region
region_dis_categ$AVAI <- region_dis_categ$DALY
region_dis_categ$APVP <- region_dis_categ$YLL
region_dis_categ$AVI <- region_dis_categ$YLD
region_dis_categ$Mortalité <- region_dis_categ$Mortality
region_dis_categ$Année <- region_dis_categ$Year
region_dis_categ$Groupe <- region_dis_categ$Group
region_dis_categ$Groupe[region_dis_categ$Groupe %in% "Male"] <- "Masculin"
region_dis_categ$Groupe[region_dis_categ$Groupe %in% "Female"] <- "Féminin"
region_dis_categ$Causes <- region_dis_categ$Cause
region_dis_categ$Causes[region_dis_categ$Causes %in% "All Causes"] <- "Toutes les causes"
region_dis_categ$Causes[region_dis_categ$Causes %in% "Group I"] <- "Groupe I"
region_dis_categ$Causes[region_dis_categ$Causes %in% "Group II"] <- "Groupe II"
region_dis_categ$Causes[region_dis_categ$Causes %in% "Group III"] <- "Groupe III"
region_dis_categ$Causes <- factor(region_dis_categ$Causes,levels = c("Toutes les causes", "Groupe I", "Groupe II", "Groupe III"))

## spanish
region_dis_categ$Región <- region_dis_categ$Region
region_dis_categ$AVAD <- region_dis_categ$DALY
region_dis_categ$AVP <- region_dis_categ$YLL
region_dis_categ$APD <- region_dis_categ$YLD
region_dis_categ$Mortalidad <- region_dis_categ$Mortality
region_dis_categ$Año <- region_dis_categ$Year
region_dis_categ$Groupo <- region_dis_categ$Group
region_dis_categ$Groupo[region_dis_categ$Groupo %in% "Male"] <- "Masculino"
region_dis_categ$Groupo[region_dis_categ$Groupo %in% "Female"] <- "Femenino"
region_dis_categ$Causa <- region_dis_categ$Cause
region_dis_categ$Causa[region_dis_categ$Causa %in% "All Causes"] <- "Todas las causas"
region_dis_categ$Causa[region_dis_categ$Causa %in% "Group I"] <- "Groupo I"
region_dis_categ$Causa[region_dis_categ$Causa %in% "Group II"] <- "Groupo II"
region_dis_categ$Causa[region_dis_categ$Causa %in% "Group III"] <- "Groupo III"
region_dis_categ$Causa <- factor(region_dis_categ$Causa,levels = c("Todas las causas", "Groupo I", "Groupo II", "Groupo III"))


############################
## dataset for 6th tab plot
############################
income_dis_categ <- income_dis_categ[!income_dis_categ$Income %in% "Global", ]
names(income_dis_categ)[2] <- "Cause"

## french
income_dis_categ$Revenu <- income_dis_categ$Income
income_dis_categ$AVAI <- income_dis_categ$DALY
income_dis_categ$APVP <- income_dis_categ$YLL
income_dis_categ$AVI <- income_dis_categ$YLD
income_dis_categ$Mortalité <- income_dis_categ$Mortality
income_dis_categ$Année <- income_dis_categ$Year
income_dis_categ$Groupe <- income_dis_categ$Group
income_dis_categ$Groupe[income_dis_categ$Groupe %in% "Male"] <- "Masculin"
income_dis_categ$Groupe[income_dis_categ$Groupe %in% "Female"] <- "Féminin"
income_dis_categ$Causes <- income_dis_categ$Cause
income_dis_categ$Causes[income_dis_categ$Causes %in% "All Causes"] <- "Toutes les causes"
income_dis_categ$Causes[income_dis_categ$Causes %in% "Group I"] <- "Groupe I"
income_dis_categ$Causes[income_dis_categ$Causes %in% "Group II"] <- "Groupe II"
income_dis_categ$Causes[income_dis_categ$Causes %in% "Group III"] <- "Groupe III"
income_dis_categ$Causes <- factor(income_dis_categ$Causes,levels = c("Toutes les causes", "Groupe I", "Groupe II", "Groupe III"))

## spanish
income_dis_categ$Ingresos <- income_dis_categ$Income
income_dis_categ$AVAD <- income_dis_categ$DALY
income_dis_categ$AVP <- income_dis_categ$YLL
income_dis_categ$APD <- income_dis_categ$YLD
income_dis_categ$Mortalidad <- income_dis_categ$Mortality
income_dis_categ$Año <- income_dis_categ$Year
income_dis_categ$Groupo <- income_dis_categ$Group
income_dis_categ$Groupo[income_dis_categ$Groupo %in% "Male"] <- "Masculino"
income_dis_categ$Groupo[income_dis_categ$Groupo %in% "Female"] <- "Femenino"
income_dis_categ$Causa <- income_dis_categ$Cause
income_dis_categ$Causa[income_dis_categ$Causa %in% "All Causes"] <- "Todas las causas"
income_dis_categ$Causa[income_dis_categ$Causa %in% "Group I"] <- "Groupo I"
income_dis_categ$Causa[income_dis_categ$Causa %in% "Group II"] <- "Groupo II"
income_dis_categ$Causa[income_dis_categ$Causa %in% "Group III"] <- "Groupo III"
income_dis_categ$Causa <- factor(income_dis_categ$Causa,levels = c("Todas las causas", "Groupo I", "Groupo II", "Groupo III"))


############################
## dataset for 7th tab plot
############################
country_dis_spec <- country_dis_spec[!country_dis_spec$Year %in% "2016", ]
country_dis_spec <- merge(country_dis_spec, DiseaseGroup_translation, by="DiseaseGroup", all.x = TRUE)
country_dis_spec <- merge(country_dis_spec, country_translation, by="Country", all.x = TRUE)

## french
country_dis_spec$Fardeau <- as.character(country_dis_spec$Burden)
country_dis_spec$Fardeau[country_dis_spec$Fardeau %in% "DALY"] <- "AVAI"
country_dis_spec$Fardeau[country_dis_spec$Fardeau %in% "YLL"] <- "APVP"
country_dis_spec$Fardeau[country_dis_spec$Fardeau %in% "YLD"] <- "AVI"
country_dis_spec$Fardeau[country_dis_spec$Fardeau %in% "Mortality"] <- "Mortalité"


## Spanish
country_dis_spec$Carga <- as.character(country_dis_spec$Burden)
country_dis_spec$Carga[country_dis_spec$Carga %in% "DALY"] <- "AVAD"
country_dis_spec$Carga[country_dis_spec$Carga %in% "YLL"] <- "AVP"
country_dis_spec$Carga[country_dis_spec$Carga %in% "YLD"] <- "APD"
country_dis_spec$Carga[country_dis_spec$Carga %in% "Mortality"] <- "Mortalidad"


country_dis_spec <- country_dis_spec[order(country_dis_spec$Country, country_dis_spec$Burden), ]
country_dis_spec1 <- country_dis_spec[country_dis_spec$Country %in% "Global", ]
country_dis_spec2 <- country_dis_spec[!country_dis_spec$Country %in% "Global", ]
country_dis_spec <- rbind(country_dis_spec1, country_dis_spec2)



## clean workspace
remove(country_age_categ1, country_age_categ2,
       country_dis_categ1, country_dis_categ2,
       country_dis_spec1, country_dis_spec2,
       file.list, j, 
       translation, translationContent, 
       country_translation, DiseaseGroup_translation)

############################################################################################################################################                 
############################################################################################################################################                 

## save all data

save(country_age_categ, file ="../WHO_burden_and_mortality_visualization/clean datasets/country_age_categ.RData")
save(country_dis_categ, file ="../WHO_burden_and_mortality_visualization/clean datasets/country_dis_categ.RData")
save(country_dis_spec, file ="../WHO_burden_and_mortality_visualization/clean datasets/country_dis_spec.RData")
save(country_geo_code, file ="../WHO_burden_and_mortality_visualization/clean datasets/country_geo_code.RData")
save(income_dis_categ, file ="../WHO_burden_and_mortality_visualization/clean datasets/income_dis_categ.RData")
save(income_dis_spec, file ="../WHO_burden_and_mortality_visualization/clean datasets/income_dis_spec.RData")
save(region_dis_categ, file ="../WHO_burden_and_mortality_visualization/clean datasets/region_dis_categ.RData")
save(region_dis_spec, file ="../WHO_burden_and_mortality_visualization/clean datasets/region_dis_spec.RData")














############################################################################################################################################                 
############################################################################################################################################                 

# 
# bb <- country_dis_spec[country_dis_spec$Sex %in% c("Both") & 
#                    country_dis_spec$Year %in% c("2000", "2010", "2015") &
#                    country_dis_spec$Burden %in% "DALY" &
#                    country_dis_spec$Country %in% "Global", ]
# 
# 
#   data7 <- bb[, -c(1,3,4)]
#   data7$values <- round(data7$values, 2)
#   data7_wide <- recast(data7, Rank ~ variable+Year, id.var = c("Rank", "Year"))
#   
#   ## combine corresponding columns
#   data7_wide$`2000` <- paste0(data7_wide$DiseaseSubGroup_2000," " ,"(",data7_wide$values_2000, ")")
#   data7_wide$`2010` <- paste0(data7_wide$DiseaseSubGroup_2010," " ,"(",data7_wide$values_2010, ")")
#   data7_wide$`2015` <- paste0(data7_wide$DiseaseSubGroup_2015," " ,"(",data7_wide$values_2015, ")")
#   #ata7_wide <-  data7_wide[, c("Rank", "2000", "2010", "2015")]
#   
#   data7_wide <-   data7_wide[, c(1, 8:10)]
 
