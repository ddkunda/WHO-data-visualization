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


#™Chris D.A.
############################################################################################################################################                 
############################################################################################################################################                 

## load data
file.list <- list.files("../WHO_burden_and_mortality_visualization/clean datasets/",
                        recursive=T,
                        pattern='*.RData',
                        full.names = TRUE)

for (j in 1:length(file.list)){ 
  load(file.list[j])
}

## loadtranslation dictionary
load("../WHO_burden_and_mortality_visualization/translation and dictionary/translation.bin")

############################################################################################################################################                 
############################################################################################################################################                 

# rsconnect::setAccountInfo(name='dideagoc',
#                           token='BB5160165A58CDC671D9AEC6A129FB71',
#                           secret='5+ZxydNAWONtTILrbipc2eNqar7aMwTfZc/NRaDh')
# 
# rsconnect::deployApp("../WHO_burden_and_mortality_visualization/")

############################################################################################################################################                 
############################################################################################################################################                 

# #get your token to access google drive
# shiny_token <- gs_auth()
# saveRDS(shiny_token, "shiny_app_token.rds")
# 
# 
# #set up data sheet in google drive
#  Data <- gs_new("Data") %>%
#    gs_ws_rename(from = "Sheet1", to = "Data")
#  Data <- Data %>%
#    gs_edit_cells(ws = "Data", input = cbind("name", "sender.email", "my.email", "msg", "timestamp"), trim = TRUE)

## Note: for some reason it wont work if first row is blank so I went into the google sheet
## and put in some values in the first few rows

sheetkey <-  "1rxVKig8hjtOElWeGWunK5c96Oh-aIxpVjuIwf_D6UJM"
#  Data$sheet_key # you can get your key from Data$sheet_key, don't share your sheet key!
Data <- gs_key(sheetkey)

############################################################################################################################################                 
############################################################################################################################################                 

# Define a server for the Shiny app
server <- function(input, output){
  
 
  
############################################################################################################################################                 
### 1ST TAB-PLOT 
  
 ## Translation for TAB1
  tr1 <- function(text){ # translates text into current language
    sapply(text,function(s) translation[[s]][[input$language1]], USE.NAMES=FALSE)
  }
  
  # UI for TAB1
  output$head1 <- renderUI({
    helpText(h3(em(tr1("Country-level profiles for all causes of burden"))))
  })
             
    output$uiburden1 <- renderUI({
    selectInput(inputId =  "burden1", 
                label = tr1("Choose a burden measure:"), 
                choices = tr1(c("DALY", "YLL", "YLD", "Mortality")),
                selected  = tr1("DALY"))
    })
  
  output$uiyear1 <- renderUI({
    radioButtons("year", tr1("Choose a year:"),
                 choices = c("2000", "2010", "2015"),
                 inline = TRUE,
                 selected = "2000")
    })
  
  
  output$uicmt11 <- renderUI({
    
    helpText(tr1("Disability-Adjusted Life Year (DALY) is the summary measure that best give an indication of overall burden of disease."),
             strong(tr1("One DALY can be thought of as one lost year of healthy life.")),
             
             strong(tr1("In contrast, Mortality alone does not give a complete picture of the burden of disease borne by individuals in different populations.")),
             br(),
             br(),
             
             tr1("Assessing  the health of a population by both mortality and morbidity provides a more encompassing view on health issues affecting that population. The sum of these DALYs across the population, can be thought of as a measurement of the gap between current health status and an ideal health situation where the entire population lives to an advanced age, free of disease and disability."),
             tr1("The DALY for a disease or health condition are calculated as the sum of the Years of Life Lost (YLL) due to premature mortality in the population and the Years Lost due to Disability (YLD) for people living with the health condition or its consequences."),
             
             br(),
             br(),
             
             strong(tr1("DALY = YLL + YLD")),
             
             br(),
             br(),
             
             em(tr1("In general, we see that burden rates across the world have declined between 2000 and 2015. However, this decline varies from one country to another. In general, Sub-Saharan Africa has the highest burden rates.")))
    
  })
  
  ## MAINPANEL for TAB6
  output$msg11 <- renderText({paste(input$burden1, tr1("per 100,000 population (hover mouse pointer over dots to reveal statistics)"))})
  
  pal <- colorFactor(palette = c('green3', 'blue', 'orange', 'red', "black") , domain = country_geo_code[, 3])
  
  output$map <- renderLeaflet({
    
    req(input$burden1)
    req(input$year)
    
    df <- country_geo_code[country_geo_code$Year %in% input$year, ] 
    
    if(input$burden1 %in% paste(tr1("Mortality"))){
      leaflet(df)  %>%
        addTiles() %>%
        setView(lng = -5, lat = 30, zoom = 2)%>%
        addCircleMarkers(df,
                         lng = ~Longitude,
                         lat = ~Latitude,
                         radius = df[, input$burden1]*3,
                         weight = 1,
                         opacity = 4,
                         fill = TRUE,
                         #fillColor = pal(df[, 3]),
                         color = pal(df[, 3]),
                         #col = factor(df[ ,"Data QC code"]),
                         label = paste(tr1(paste(df$Country)), "(", input$year, input$burden1, ")", ":", df[, input$burden1])
                         #,
        )}
    
    else if(input$burden1 %in% paste(tr1("YLD"))){
      leaflet(df)  %>%
        addTiles() %>%
        setView(lng = -5, lat = 30, zoom = 2)%>%
        addCircleMarkers(df,
                         lng = ~Longitude,
                         lat = ~Latitude,
                         radius = sqrt(df[, input$burden1])*1.2,
                         weight = 1,
                         opacity = 4,
                         fill = TRUE,
                         #fillColor = pal,
                         color = pal(df[, 3]),
                         #col = factor(df[ ,"Data QC code"]),
                         label = paste(tr1(paste(df$Country)), "(", input$year, input$burden1, ")", ":", df[, input$burden1])
                         #,
        )}
    
    else if(input$burden1 %in% paste(tr1(c("DALY", "YLL")))){
      leaflet(df)  %>%
        addTiles() %>%
        setView(lng = -5, lat = 30, zoom = 2)%>%
        addCircleMarkers(df,
                         lng = ~Longitude,
                         lat = ~Latitude,
                         radius = sqrt(df[, input$burden1])/2.5,
                         weight = 1,
                         opacity = 4,
                         fill = TRUE,
                         #fillColor = pal,
                         color = pal(df[, 3]),
                         #col = factor(df[ ,"Data QC code"]),
                         label = paste(tr1(paste(df$Country)), "(", input$year, input$burden1, ")", ":", df[, input$burden1])
                         #,
        )
    }
  
  })
  
  output$msg12 <- renderText({tr1("LEGEND: Dot radius is function of burden; Small radius means low burden.")})
  
  output$msg13 <- renderText({tr1("Countries are color-coded based on the quality of the data (DQ) they provided.")})
      
  output$msg14 <- renderText({tr1("Green (DQ = 1): Multiple years of high quality death registration data are available;  Blue (DQ = 2): Multiple years of moderate quality death registration data are available;  Orange (DQ = 3): Multiple years of low quality death registration data are available;  Red (DQ = 4): Low HIV country without useable death registration data;  Black (DQ = 5): High HIV country without useable death registration.")})
  

  
  
############################################################################################################################################                 
### 2ND TAB-PLOT 
  
  ## Translation for TAB2
  tr2 <- function(text){
    sapply(text,function(s) translation[[s]][[input$language2]], USE.NAMES=FALSE)
  }
  
  # UI for TAB2
  output$head2 <- renderUI({
    helpText(h3(em(tr2("Country-level profiles by types of cause"))))
  })

  output$uiburden2 <- renderUI({
    selectInput(inputId =  "burden2", 
                label = tr2("Choose a burden measure:"), 
                choices = tr2(c("DALY", "YLL", "YLD", "Mortality")),
                selected  = tr2("DALY"))
  })
  
  output$uicountry2<- renderUI({
    selectInput("country2", 
                tr2("Choose a country:"),
                choices = tr2(unique(country_dis_categ$Country)),
                selected = tr2("Global"))
  })
  
  
  output$uicmt21 <- renderUI({
    
    helpText(em(tr2("All causes of burden can be broken down into three major categories of disease and disability including: Communicable, maternal, perinatal and nutritional conditions (Group I); Noncommunicable diseases (Group II); and Injuries (Group III). Here we show Global and country-level profiles.")),
                br(),
                br(),
                em(tr2("Overall, most countries have seen their All Causes of burden rates declined between 2000 and 2015. This decline is primarily due to a decrease in either Group I or Group II or both."))
                   )
  })
  
  
  ## MAINPANEL for TAB2
  df2 <- reactive({
    country_dis_categ[country_dis_categ$Year %in% c("2000","2010", "2015") & 
                        country_dis_categ$Sex %in% c("Both") & 
                        country_dis_categ[, tr2("Country")] %in% input$country2, ]
  })
  
  output$causePlot1 <- renderPlot({
    ## reactive data subset
    data2 <- df2()
    
    ##Render a barplot
    #if(input$xvar3 %in% "Year"){ 
      ggplot(data2,
           aes(data2[,tr2("Cause")],
               data2[,input$burden2],
               fill= data2[,tr2("Year")]))+
        geom_bar(stat="identity", position = "dodge", col="black", width = 0.6)+
        #scale_fill_manual(values = palette(rainbow(4)), aesthetics = "fill")+
        theme_bw() +
        labs(y=paste(input$burden2, tr2(" (per 100,000 population) ")), x="")+
        theme(legend.position = "right", axis.title = element_text(family = "", color="black", face="bold", size=16))+
        theme(axis.text = element_text(size=12, color = "black", face="bold", angle = 0, hjust = 0.5))+
        theme(panel.spacing = unit(2, "lines"))+
        ggtitle(paste(input$country2))+
        scale_fill_discrete(name = tr2("Year"))+
        #annotation_custom(rast, xmin = 4, ymin = 0, ymax = 1e+07)
        annotate("text", x = Inf, y = 0, label = "@Christian.Dide-Agossou", 
                 hjust=1, vjust=-1, col="grey40",cex=3, fontface = "bold", alpha = 0.8)
    })
    
 
  output$causePlot2 <- renderPlot({
    ## reactive data subset
    data2 <- df2()
    
    ##Render a barplot  ggplot(data2,
    ggplot(data2,
           aes(data2[,tr2("Year")],
               data2[,input$burden2],
               fill= data2[,tr2("Cause")]))+
          geom_bar(stat="identity", position = "dodge", col="black", width = 0.6)+
          #scale_fill_manual(values = palette(rainbow(4)), aesthetics = "fill")+
          theme_bw() +
          labs(y=paste(input$burden2, tr2(" (per 100,000 population) ")), x="")+
          theme(legend.position = "right", axis.title = element_text(family = "", color="black", face="bold", size=16))+
          theme(axis.text = element_text(size=12, color = "black", face="bold", angle = 0, hjust = 0.5))+
          theme(panel.spacing = unit(2, "lines"))+
          ggtitle(paste(input$country2))+
          scale_fill_discrete(name = tr2("Cause"))+
      #annotation_custom(rast, xmin = 4, ymin = 0, ymax = 1e+07)
        annotate("text", x = Inf, y = 0, label = "@Christian.Dide-Agossou", 
                 hjust=1, vjust=-1, col="grey40",cex=3, fontface = "bold", alpha = 0.8)
      })
  
  
  output$uicmt22 <- renderUI({
    
    helpText(strong(tr2("Group I =")), tr2("Communicable, maternal, perinatal and nutritional conditions;"),
             strong(tr2("Group II =")), tr2("Noncommunicable diseases;"),
             strong(tr2("Group III =")), tr2("Injuries."))
  })

  
 

############################################################################################################################################                 
### 3RD TAB-PLOT 
 
   ## Translation for TAB3
  tr3 <- function(text){
    sapply(text,function(s) translation[[s]][[input$language3]], USE.NAMES=FALSE)
  }
  
  # UI for TAB3
  output$head3 <- renderUI({
    helpText(h3(em(tr3("Country-level profiles by sex"))))
  })
  
  output$uiburden3 <- renderUI({
    selectInput(inputId =  "burden3", 
                label = tr3("Choose a burden measure:"), 
                choices = tr3(c("DALY", "YLL", "YLD", "Mortality")),
                selected  = tr3("DALY"))
  })
  
  output$uicountry3<- renderUI({
    selectInput("country3", 
                tr3("Choose a country:"),
                choices = tr3(unique(country_dis_categ$Country)),
                selected = tr3("Global"))
  })
  
  
  output$uicmt31 <- renderUI({
    
    helpText(em(tr3("Overall, the burden rates have declined in both males and females in most countries between 2000 and 2015. However, males are more likely to have higher burden of disease and disability than females.")))

  })
  
  
  ## MAINPANEL for TAB3
  df3 <- reactive({
    country_dis_categ[country_dis_categ$Year %in% c("2000","2010", "2015") & 
                        country_dis_categ$Sex %in% c("Female", "Male") & 
                        country_dis_categ$Cause %in% c("All Causes") &
                        country_dis_categ[, tr3("Country")] %in% input$country3, ]
  })
  
  output$sexPlot1 <- renderPlot({
    ## reactive data subset
    data3 <- df3()
    
    ##Render a barplot
    #if(input$xvar3 %in% "Year"){
      ggplot(data3,
           aes(data3[,tr3("Sex")],
               data3[,input$burden3],
               fill= data3[,tr3("Year")]))+
      #geom_bar(stat="identity", position = "dodge", col="black", width = 0.6)+
      stat_summary(fun.y=sum, geom="bar",position ="dodge", colour="black", width = 0.6)+
      #scale_fill_manual(values = palette(rainbow(4)), aesthetics = "fill")+
      theme_bw() +
      labs(y=paste(input$burden3, tr3(" (per 100,000 population) ")), x="")+
      theme(legend.position = "right", axis.title = element_text(family = "", color="black", face="bold", size=16))+
      theme(axis.text = element_text(size=12, color = "black", face="bold", angle = 0, hjust = 0.5))+
      theme(panel.spacing = unit(2, "lines"))+
      ggtitle(paste(input$country3))+
      scale_fill_discrete(name = tr3("Year"))+
        #annotation_custom(rast, xmin = 4, ymin = 0, ymax = 1e+07)
      annotate("text", x = Inf, y = 0, label = "@Christian.Dide-Agossou", 
               hjust=1, vjust=-1, col="grey40",cex=3, fontface = "bold", alpha = 0.8)
    })
    
   # else {
  output$sexPlot2 <- renderPlot({
    ## reactive data subset
    data3 <- df3()
    
    ggplot(data3,
             aes(data3[,tr3("Year")],
                 data3[,input$burden3],
                 fill= data3[,tr3("Sex")]))+
        #geom_bar(stat="identity", position = "dodge", col="black", width = 0.6)+
        stat_summary(fun.y=sum, geom="bar",position ="dodge", colour="black", width = 0.6)+
        #scale_fill_manual(values = palette(rainbow(4)), aesthetics = "fill")+
        theme_bw() +
        labs(y=paste(input$burden3, tr3(" (per 100,000 population) ")), x="")+
        theme(legend.position = "right", axis.title = element_text(family = "", color="black", face="bold", size=16))+
        theme(axis.text = element_text(size=12, color = "black", face="bold", angle = 0, hjust = 0.5))+
        theme(panel.spacing = unit(2, "lines"))+
        ggtitle(paste(input$country3))+
        scale_fill_discrete(name = tr3("Sex"))+
      #annotation_custom(rast, xmin = 4, ymin = 0, ymax = 1e+07)
        annotate("text", x = Inf, y = 0, label = "@Christian.Dide-Agossou", 
                 hjust=1, vjust=-1, col="grey40",cex=3, fontface = "bold", alpha = 0.8)
    
  })

  
  
  ############################################################################################################################################                 
  ### 4TH TAB-PLOT 
  
  ## Translation for TAB4
  tr4 <- function(text){
    sapply(text,function(s) translation[[s]][[input$language4]], USE.NAMES=FALSE)
  }
  
  # UI for TAB4
  output$head4 <- renderUI({
    helpText(h3(em(tr4("Country-level profiles by age"))))
  })
  
  output$uiburden4 <- renderUI({
    selectInput(inputId =  "burden4", 
                label = tr4("Choose a burden measure:"), 
                choices = tr4(c("DALY", "YLL", "YLD", "Mortality")),
                selected  = tr4("DALY"))
  })
  
  output$uicountry4<- renderUI({
    selectInput("country4", 
                tr4("Choose a country:"),
                choices = tr4(unique(country_age_categ$Country)),
                selected = tr4("Global"))
  })
  
  
  output$uicmt41 <- renderUI({
   
    helpText(em(tr4("In general, most countries have seen their burden rates decreased between 2000 and 2015, within each age-group. However, the age-groups 0-4 years or 60-69 years or 70+ years are more likely to have the highest burden of disease and disability.")))
   
  })
  
  
  ## MAINPANEL for TAB4
  df4 <- reactive({
    country_age_categ[country_age_categ$Year %in% c("2000","2010", "2015") & 
                        country_age_categ$Sex %in% c("Both") & 
                        country_age_categ$Cause %in% c("All Causes") &
                        country_age_categ[, tr4("Country")] %in% input$country4, ]
  })
  
  output$agePlot1 <- renderPlot({
    ## reactive data subset
    data4 <- df4()
    
    ##Render a barplot
    #if(input$xvar4 %in% "Year"){
      ggplot(data4,
           aes(data4[ , tr4("Age")],
               data4[ , input$burden4],
               fill= data4[ , tr4("Year")]))+
      geom_bar(stat="identity", position = "dodge", col="black", width = 0.6)+
      #stat_summary(fun.y=sum, geom="bar",position ="dodge", colour="black", width = 0.6)+
      #scale_fill_manual(values = palette(rainbow(4)), aesthetics = "fill")+
      theme_bw() +
      labs(y=paste(input$burden4, tr4(" (per 100,000 population) ")), x="")+
      theme(legend.position = "right", axis.title = element_text(family = "", color="black", face="bold", size=16))+
      theme(axis.text = element_text(size=12, color = "black", face="bold", angle = 0, hjust = 0.5))+
      theme(panel.spacing = unit(2, "lines"))+
      ggtitle(paste(input$country4))+
      scale_fill_discrete(name = tr4("Year"))+
        #annotation_custom(rast, xmin = 4, ymin = 0, ymax = 1e+07)
      annotate("text", x = Inf, y = 0, label = "@Christian.Dide-Agossou", 
               hjust=1, vjust=-1, col="grey40",cex=3, fontface = "bold", alpha = 0.8)
    })
    
  #  else {
  output$agePlot2 <- renderPlot({
    ## reactive data subset
    data4 <- df4()
    
    ggplot(data4,
             aes(data4[ , tr4("Year")],
                 data4[ , input$burden4],
                 fill= data4[ , tr4("Age")]))+
        geom_bar(stat="identity", position = "dodge", col="black", width = 0.6)+
        #stat_summary(fun.y=sum, geom="bar",position ="dodge", colour="black", width = 0.6)+
        #scale_fill_manual(values = palette(rainbow(4)), aesthetics = "fill")+
        theme_bw() +
        labs(y=paste(input$burden4, tr4(" (per 100,000 population) ")), x="")+
        theme(legend.position = "right", axis.title = element_text(family = "", color="black", face="bold", size=16))+
        theme(axis.text = element_text(size=12, color = "black", face="bold", angle = 0, hjust = 0.5))+
        theme(panel.spacing = unit(2, "lines"))+
        ggtitle(paste(input$country4))+
        scale_fill_discrete(name = tr4("Age"))+
      #annotation_custom(rast, xmin = 4, ymin = 0, ymax = 1e+07)
        annotate("text", x = Inf, y = 0, label = "@Christian.Dide-Agossou", 
                 hjust=1, vjust=-1, col="grey40",cex=3, fontface = "bold", alpha = 0.8)
   
  })
  
  
  
  
  
  ############################################################################################################################################                 
  ### 5TH TAB-PLOT 
  
  ## Translation for TAB5
  tr5 <- function(text){
    sapply(text,function(s) translation[[s]][[input$language5]], USE.NAMES=FALSE)
  }
  
  # UI for TAB5
  output$head5 <- renderUI({
    helpText(h3(em(tr5("WHO region-level profiles"))))
  })
  
  
  output$uiburden5 <- renderUI({
    selectInput(inputId =  "burden5", 
                label = tr5("Choose a burden measure:"), 
                choices = tr5(c("DALY", "YLL", "YLD", "Mortality")),
                selected  = tr5("DALY"))
  })
  
  output$uistrata5 <- renderUI({
    selectInput(inputId = "strata5",
                label = tr5("Stratified by:"),
                choices = tr5(c("None","Cause","Sex")),
                selected = tr5("None"))
  })
  
 
  
  output$uicmt51 <- renderUI({
    
    helpText(em(
      tr5("Overall, every WHO region has seen some decline in burden rates between 2000 and 2015. However, the WHO African Region had the highest burden rates and the greatest decline in burden rates between 2000 and 2015. A stratification by types of cause shows that Communicable, maternal, perinatal and nutritional conditions (Group I) are more predominant in the WHO African Region which include mostly developing nations. In contrary, Noncommunicable diseases (Group II) are more predominant in the Region of the Americas, the European and the Western Pacific Regions, which include the majority of developed nations. However, both Group I and Group II are equally predominant in the Eastern Mediterranean and South-East Asia Regions.")))
       })
  
  
  
  output$uicmt52 <- renderUI({
    
    helpText(em(
      tr5("A stratification by sex shows that males are more likely to have higher burden rates than females regardless of the WHO region. However, the gap between males and females has reduced in all WHO regions over the years.")))
  })
  
  
  output$uicmt53 <- renderUI({
    
    helpText(em(
               strong("AFR ="), tr5("African Region;"),
               strong("AMR ="), tr5("Region of the Americas;"),
               strong("EMR ="), tr5("Eastern Mediterranean Region;"),
               strong("EUR ="), tr5("European Region;"),
               strong("SEAR ="), tr5("South-East Asia Region;"),
               strong("WPR ="), tr5("Western Pacific Region.")))
      })
 

  
  ## MAINPANEL for TAB5
  output$regionPlot1 <- renderPlot({
    
    req(input$strata5)
    
      if(input$strata5 %in% paste(tr5("None"))){
      
      ## Render a barplot
      ggplot(region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                region_dis_categ$Cause %in% "All Causes" & 
                                region_dis_categ$Group %in% "Both", ],
             aes(region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                    region_dis_categ$Cause %in% "All Causes" & 
                                    region_dis_categ$Group %in% "Both", ][ , tr5("Region")],
                 region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                    region_dis_categ$Cause %in% "All Causes" & 
                                    region_dis_categ$Group %in% "Both", ][,input$burden5],
                 fill= region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                          region_dis_categ$Cause %in% "All Causes" & 
                                          region_dis_categ$Group %in% "Both", ][ , tr5("Year")]))+
        geom_bar(stat="identity", position = "dodge", col="black", width = 0.6)+
        #stat_summary(fun.y=sum, geom="bar",position ="dodge", colour="black", width = 0.6)+
        #scale_fill_manual(values = palette(rainbow(4)), aesthetics = "fill")+
        #facet_wrap(~)+
        theme_bw() +
        labs(y=paste(input$burden5, tr5(" (per 100,000 population) ")), x="")+
        theme(legend.position = "right", axis.title = element_text(family = "", color="black", face="bold", size=16))+
        theme(axis.text = element_text(size=12, color = "black", face="bold", angle = 0, hjust = 0.5))+
        theme(panel.spacing = unit(2, "lines"))+
        scale_fill_discrete(name = tr5("Year"))+
        annotate("text", x = Inf, y = 0, label = "@Christian.Dide-Agossou", 
                 hjust=1, vjust=-1, col="grey40",cex=3, fontface = "bold", alpha = 0.8)
      
    }
    
    else if(input$strata5 %in% paste(tr5("Cause"))){
       
       ## Render a barplot
      ggplot(region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                region_dis_categ$Cause %in% c("Group I","Group II", "Group III") &
                                region_dis_categ$Group %in% "Both", ],
             aes(interaction(region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                                region_dis_categ$Cause %in% c("Group I","Group II", "Group III") &
                                                region_dis_categ$Group %in% "Both", ][ , tr5("Cause")], 
                             region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                                region_dis_categ$Cause %in% c("Group I","Group II", "Group III") &
                                                region_dis_categ$Group %in% "Both", ][ , tr5("Region")]),
                 region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                    region_dis_categ$Cause %in% c("Group I","Group II", "Group III") &
                                    region_dis_categ$Group %in% "Both", ][,input$burden5],
                 fill= region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                          region_dis_categ$Cause %in% c("Group I","Group II", "Group III") &
                                          region_dis_categ$Group %in% "Both", ][ , tr5("Year")]))+
        geom_bar(stat="identity", position = "dodge", col="black", width = 0.6)+
        #stat_summary(fun.y=sum, geom="bar",position ="dodge", colour="black", width = 0.6)+
        #scale_fill_manual(values = palette(rainbow(4)), aesthetics = "fill")+
        #facet_wrap(~)+
        theme_bw() +
        labs(y=paste(input$burden5, tr5(" (per 100,000 population) ")), x="")+
        theme(legend.position = "right", axis.title = element_text(family = "", color="black", face="bold", size=16))+
        theme(axis.text.x = element_text(size=12, color = "black", face="bold", angle = 25, hjust = 1))+
        theme(axis.text.y = element_text(size=12, color = "black", face="bold"))+
        theme(panel.spacing = unit(2, "lines"))+
        scale_fill_discrete(name = tr5("Year"))+
        annotate("text", x = Inf, y = 0, label = "@Christian.Dide-Agossou", 
                 hjust=1, vjust=-1, col="grey40",cex=3, fontface = "bold", alpha = 0.8)
      
    }
    
    else if(input$strata5 %in% paste(tr5("Sex"))){
     
      ## Render a barplot
      ggplot(region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                region_dis_categ$Cause %in% "All Causes" &
                                region_dis_categ$Group %in% c("Male","Female"), ],
             aes(interaction(region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                                region_dis_categ$Cause %in% "All Causes" &
                                                region_dis_categ$Group %in% c("Male","Female"), ][ , tr5("Group")], 
                             region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                                region_dis_categ$Cause %in% "All Causes" &
                                                region_dis_categ$Group %in% c("Male","Female"), ][ , tr5("Region")]),
                 region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                    region_dis_categ$Cause %in% "All Causes" &
                                    region_dis_categ$Group %in% c("Male","Female"), ][,input$burden5],
             fill= region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                      region_dis_categ$Cause %in% "All Causes" &
                                      region_dis_categ$Group %in% c("Male","Female"), ][ , tr5("Year")]))+
        geom_bar(stat="identity", position = "dodge", col="black", width = 0.6)+
        #stat_summary(fun.y=sum, geom="bar",position ="dodge", colour="black", width = 0.6)+
        #scale_fill_manual(values = palette(rainbow(4)), aesthetics = "fill")+
        #facet_wrap(~Group)+
        theme_bw() +
        labs(y=paste(input$burden5, tr5(" (per 100,000 population) ")), x="")+
        theme(legend.position = "right", axis.title = element_text(family = "", color="black", face="bold", size=16))+
        theme(axis.text.x = element_text(size=12, color = "black", face="bold", angle = 25, hjust = 1))+
        theme(axis.text.y = element_text(size=12, color = "black", face="bold"))+
        theme(panel.spacing = unit(2, "lines"))+
        scale_fill_discrete(name = tr5("Year"))+
        annotate("text", x = Inf, y = 0, label = "@Christian.Dide-Agossou", 
                 hjust=1, vjust=-1, col="grey40",cex=3, fontface = "bold", alpha = 0.8)
      
    }
  })
  
  
  output$regionPlot2 <- renderPlot({
    
    req(input$strata5)
    
    if(input$strata5 %in% paste(tr5("None"))){
      
      ## reactive data subset
      ggplot(region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                region_dis_categ$Cause %in% "All Causes" & 
                                region_dis_categ$Group %in% "Both", ],
             aes(region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                    region_dis_categ$Cause %in% "All Causes" & 
                                    region_dis_categ$Group %in% "Both", ][ , tr5("Year")],
                 region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                    region_dis_categ$Cause %in% "All Causes" & 
                                    region_dis_categ$Group %in% "Both", ][,input$burden5],
                 fill= region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                          region_dis_categ$Cause %in% "All Causes" & 
                                          region_dis_categ$Group %in% "Both", ][ , tr5("Region")]))+
        geom_bar(stat="identity", position = "dodge", col="black", width = 0.6)+
        #stat_summary(fun.y=sum, geom="bar",position ="dodge", colour="black", width = 0.6)+
        #scale_fill_manual(values = palette(rainbow(4)), aesthetics = "fill")+
        #facet_wrap(~)+
        theme_bw() +
        labs(y=paste(input$burden5, tr5(" (per 100,000 population) ")), x="")+
        theme(legend.position = "right", axis.title = element_text(family = "", color="black", face="bold", size=16))+
        theme(axis.text = element_text(size=12, color = "black", face="bold", angle = 0, hjust = 0.5))+
        theme(panel.spacing = unit(2, "lines"))+
        scale_fill_discrete(name = tr5("Region"))+
        annotate("text", x = Inf, y = 0, label = "@Christian.Dide-Agossou", 
                 hjust=1, vjust=-1, col="grey40",cex=3, fontface = "bold", alpha = 0.8)
      
    }
    
    else if(input$strata5 %in% paste(tr5("Cause"))){
      
      ## reactive data subset
      ggplot(region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                region_dis_categ$Cause %in% c("Group I","Group II", "Group III") &
                                region_dis_categ$Group %in% "Both", ],
             aes(interaction(region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                                region_dis_categ$Cause %in% c("Group I","Group II", "Group III") &
                                                region_dis_categ$Group %in% "Both", ][ , tr5("Cause")], 
                             region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                                region_dis_categ$Cause %in% c("Group I","Group II", "Group III") &
                                                region_dis_categ$Group %in% "Both", ][ , tr5("Year")]),
                 region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                    region_dis_categ$Cause %in% c("Group I","Group II", "Group III") &
                                    region_dis_categ$Group %in% "Both", ][,input$burden5],
                 fill= region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                          region_dis_categ$Cause %in% c("Group I","Group II", "Group III") &
                                          region_dis_categ$Group %in% "Both", ][ , tr5("Region")]))+
        geom_bar(stat="identity", position = "dodge", col="black", width = 0.6)+
        #stat_summary(fun.y=sum, geom="bar",position ="dodge", colour="black", width = 0.6)+
        #scale_fill_manual(values = palette(rainbow(4)), aesthetics = "fill")+
        #facet_wrap(~)+
        theme_bw() +
        labs(y=paste(input$burden5, tr5(" (per 100,000 population) ")), x="")+
        theme(legend.position = "right", axis.title = element_text(family = "", color="black", face="bold", size=16))+
        theme(axis.text.x = element_text(size=12, color = "black", face="bold", angle = 25, hjust = 1))+
        theme(axis.text.y = element_text(size=12, color = "black", face="bold"))+
        theme(panel.spacing = unit(2, "lines"))+
        scale_fill_discrete(name = tr5("Region"))+
        annotate("text", x = Inf, y = 0, label = "@Christian.Dide-Agossou", 
                 hjust=1, vjust=-1, col="grey40",cex=3, fontface = "bold", alpha = 0.8)
      
    }  
    
    else if(input$strata5 %in% paste(tr5("Sex"))){
      
       ## reactive data subset
      ggplot(region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                region_dis_categ$Cause %in% "All Causes" &
                                region_dis_categ$Group %in% c("Male","Female"), ],
             aes(interaction(region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                                region_dis_categ$Cause %in% "All Causes" &
                                                region_dis_categ$Group %in% c("Male","Female"), ][ , tr5("Group")], 
                             region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                                region_dis_categ$Cause %in% "All Causes" &
                                                region_dis_categ$Group %in% c("Male","Female"), ][ , tr5("Year")]),
                 region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                    region_dis_categ$Cause %in% "All Causes" &
                                    region_dis_categ$Group %in% c("Male","Female"), ][,input$burden5],
                 fill= region_dis_categ[region_dis_categ$Year %in% c("2000","2010", "2015") & 
                                          region_dis_categ$Cause %in% "All Causes" &
                                          region_dis_categ$Group %in% c("Male","Female"), ][ , tr5("Region")]))+
        geom_bar(stat="identity", position = "dodge", col="black", width = 0.6)+
        #stat_summary(fun.y=sum, geom="bar",position ="dodge", colour="black", width = 0.6)+
        #scale_fill_manual(values = palette(rainbow(4)), aesthetics = "fill")+
        #facet_wrap(~Group)+
        theme_bw() +
        labs(y=paste(input$burden5, tr5(" (per 100,000 population) ")), x="")+
        theme(legend.position = "right", axis.title = element_text(family = "", color="black", face="bold", size=16))+
        theme(axis.text.x = element_text(size=12, color = "black", face="bold", angle = 25, hjust = 1))+
        theme(axis.text.y = element_text(size=12, color = "black", face="bold"))+
        theme(panel.spacing = unit(2, "lines"))+
        scale_fill_discrete(name = tr5("Region"))+
        annotate("text", x = Inf, y = 0, label = "@Christian.Dide-Agossou", 
                 hjust=1, vjust=-1, col="grey40",cex=3, fontface = "bold", alpha = 0.8)
      
    }
    
  })
  
  
  
############################################################################################################################################                 
### 6TH TAB-PLOT 
 
  ## Translation for TAB6
  tr6 <- function(text){
    sapply(text,function(s) translation[[s]][[input$language6]], USE.NAMES=FALSE)
  }
  
  ## UI for TAB6
  output$head6 <- renderUI({
    helpText(h3(em(tr6("World Bank Income-level profiles"))))
  })
  
  
  
  output$uiburden6 <- renderUI({
    selectInput(inputId =  "burden6", 
                label = tr6("Choose a burden measure:"), 
                choices = tr6(c("DALY", "YLL", "YLD", "Mortality")),
                selected  = tr6("DALY"))
  })
  
  output$uistrata6 <- renderUI({
    selectInput(inputId = "strata6",
                label = tr6("Stratified by:"),
                choices = tr6(c("None","Cause", "Sex")),
                selected = tr6("None"))
  })
  
  
  output$uicmt61 <- renderUI({
    
    helpText(em(
     
       tr6("Overall, every World Bank Income group has seen some decline in burden rates between 2000 and 2015. However, the Low-Income economies group had the highest burden rates and the greatest decline in burden rates between 2000 and 2015. In contrary, burden rates have changed little in the High-Income economies group between 2000 and 2015. A stratification by types of cause shows that Communicable, maternal, perinatal and nutritional conditions (Group I) are more predominant in the Low-Income economies group, which include mostly developing nations. In contrary, Noncommunicable diseases (Group II) are more predominant in the Upper-Middle-Income economies and High-Income economies groups, which include the majority of developed nations. However, both Group I and Group II are equally predominant in the Lower-Middle-Income economies group.")))
  })
  
  
  output$uicmt62 <- renderUI({
    
    helpText(em(
      tr6("A stratification by sex shows that males are more likely to have higher burden rates than females regardless of the World Bank Income group. Here as well, the gap between males and females has reduced in all World Bank Income groups over the years.")))
  })
  
  
  output$uicmt63 <- renderUI({
    
    helpText(em(
      strong("LI ="), tr6("Low-income economies;"),
      strong("LMI ="), tr6("Lower-middle-income economies;"),
      strong("UMI ="), tr6("Upper-middle-income economies;"),
      strong("HI ="), tr6("High-income economies.")))
  })
  
  
 
  ## MAINPANEL for TAB6
  output$wbiPlot1 <- renderPlot({
    
    req(input$strata6)

    if(input$strata6 %in% paste(tr6("None"))){
      
      ## Render a barplot
      ggplot(income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                income_dis_categ$Cause %in% "All Causes" & 
                                income_dis_categ$Group %in% "Both", ],
             aes(income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                    income_dis_categ$Cause %in% "All Causes" & 
                                    income_dis_categ$Group %in% "Both", ][, tr6("Income")],
                 income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                    income_dis_categ$Cause %in% "All Causes" & 
                                    income_dis_categ$Group %in% "Both", ][,input$burden6],
                 fill= income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                          income_dis_categ$Cause %in% "All Causes" & 
                                          income_dis_categ$Group %in% "Both", ][, tr6("Year")]))+
        geom_bar(stat="identity", position = "dodge", col="black", width = 0.6)+
        #stat_summary(fun.y=sum, geom="bar",position ="dodge", colour="black", width = 0.6)+
        #scale_fill_manual(values = palette(rainbow(4)), aesthetics = "fill")+
        #facet_wrap(~)+
        theme_bw() +
        labs(y=paste(input$burden6, tr6(" (per 100,000 population) ")), x="")+
        theme(legend.position = "right", axis.title = element_text(family = "", color="black", face="bold", size=16))+
        theme(axis.text = element_text(size=12, color = "black", face="bold", angle = 0, hjust = 0.5))+
        theme(panel.spacing = unit(2, "lines"))+
        scale_fill_discrete(name = tr6("Year"))+
        annotate("text", x = Inf, y = 0, label = "@Christian.Dide-Agossou", 
                 hjust=1, vjust=-1, col="grey40",cex=3, fontface = "bold", alpha = 0.8)
      
    }
    
    else if (input$strata6 %in% paste(tr6("Cause"))){
      
      ## Render a barplot
      ggplot(income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                income_dis_categ$Cause %in% c("Group I","Group II", "Group III") &
                                income_dis_categ$Group %in% "Both", ],
             aes(interaction(income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                                income_dis_categ$Cause %in% c("Group I","Group II", "Group III") &
                                                income_dis_categ$Group %in% "Both", ][, tr6("Cause")],
                             income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                                income_dis_categ$Cause %in% c("Group I","Group II", "Group III") &
                                                income_dis_categ$Group %in% "Both", ][, tr6("Income")]),
                 income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                    income_dis_categ$Cause %in% c("Group I","Group II", "Group III") &
                                    income_dis_categ$Group %in% "Both", ][,input$burden6],
                 fill= income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                          income_dis_categ$Cause %in% c("Group I","Group II", "Group III") &
                                          income_dis_categ$Group %in% "Both", ][, tr6("Year")]))+
        geom_bar(stat="identity", position = "dodge", col="black", width = 0.6)+
        #stat_summary(fun.y=sum, geom="bar",position ="dodge", colour="black", width = 0.6)+
        #scale_fill_manual(values = palette(rainbow(4)), aesthetics = "fill")+
        #facet_wrap(~)+
        theme_bw() +
        labs(y=paste(input$burden6, tr6(" (per 100,000 population) ")), x="")+
        theme(legend.position = "right", axis.title = element_text(family = "", color="black", face="bold", size=16))+
        theme(axis.text.x = element_text(size=12, color = "black", face="bold", angle = 25, hjust = 1))+
        theme(axis.text.y = element_text(size=12, color = "black", face="bold"))+
        theme(panel.spacing = unit(2, "lines"))+
        scale_fill_discrete(name = tr6("Year"))+
        annotate("text", x = Inf, y = 0, label = "@Christian.Dide-Agossou", 
                 hjust=1, vjust=-1, col="grey40",cex=3, fontface = "bold", alpha = 0.8)
      
    }
    
    else if(input$strata6 %in% paste(tr6("Sex"))){
      ## Render a barplot
      ggplot(income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                income_dis_categ$Cause %in% "All Causes" &
                                income_dis_categ$Group %in% c("Male","Female"), ],
             aes(interaction(income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                                income_dis_categ$Cause %in% "All Causes" &
                                                income_dis_categ$Group %in% c("Male","Female"), ][,tr6("Group")],
                             income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                                income_dis_categ$Cause %in% "All Causes" &
                                                income_dis_categ$Group %in% c("Male","Female"), ][,tr6("Income")]),
                 income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                    income_dis_categ$Cause %in% "All Causes" &
                                    income_dis_categ$Group %in% c("Male","Female"), ][,input$burden6],
                 fill= income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                          income_dis_categ$Cause %in% "All Causes" &
                                          income_dis_categ$Group %in% c("Male","Female"), ][,tr6("Year")]))+
        geom_bar(stat="identity", position = "dodge", col="black", width = 0.6)+
        #stat_summary(fun.y=sum, geom="bar",position ="dodge", colour="black", width = 0.6)+
        #scale_fill_manual(values = palette(rainbow(4)), aesthetics = "fill")+
        #facet_wrap(~Group)+
        theme_bw() +
        labs(y=paste(input$burden6, tr6(" (per 100,000 population) ")), x="")+
        theme(legend.position = "right", axis.title = element_text(family = "", color="black", face="bold", size=16))+
        theme(axis.text.x = element_text(size=12, color = "black", face="bold", angle = 25, hjust = 1))+
        theme(axis.text.y = element_text(size=12, color = "black", face="bold"))+
        theme(panel.spacing = unit(2, "lines"))+
        scale_fill_discrete(name = tr6("Year"))+
        annotate("text", x = Inf, y = 0, label = "@Christian.Dide-Agossou", 
                 hjust=1, vjust=-1, col="grey40",cex=3, fontface = "bold", alpha = 0.8)
      
    }
  })
  
  
  output$wbiPlot2 <- renderPlot({
    
    req(input$strata6)
    
    if(input$strata6 %in% paste(tr6("None"))){
      
      ## reactive data subset
      ggplot(income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                income_dis_categ$Cause %in% "All Causes" & 
                                income_dis_categ$Group %in% "Both", ],
             aes(income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                    income_dis_categ$Cause %in% "All Causes" & 
                                    income_dis_categ$Group %in% "Both", ][,tr6("Year")],
                 income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                    income_dis_categ$Cause %in% "All Causes" & 
                                    income_dis_categ$Group %in% "Both", ][,input$burden6],
                 fill= income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                          income_dis_categ$Cause %in% "All Causes" & 
                                          income_dis_categ$Group %in% "Both", ][,tr6("Income")]))+
        geom_bar(stat="identity", position = "dodge", col="black", width = 0.6)+
        #stat_summary(fun.y=sum, geom="bar",position ="dodge", colour="black", width = 0.6)+
        #scale_fill_manual(values = palette(rainbow(4)), aesthetics = "fill")+
        #facet_wrap(~)+
        theme_bw() +
        labs(y=paste(input$burden6, tr6(" (per 100,000 population) ")), x="")+
        theme(legend.position = "right", axis.title = element_text(family = "", color="black", face="bold", size=16))+
        theme(axis.text = element_text(size=12, color = "black", face="bold", angle = 0, hjust = 0.5))+
        theme(panel.spacing = unit(2, "lines"))+
        scale_fill_discrete(name = tr6("Income"))+
        annotate("text", x = Inf, y = 0, label = "@Christian.Dide-Agossou", 
                 hjust=1, vjust=-1, col="grey40",cex=3, fontface = "bold", alpha = 0.8)
      
    }
    
    else if(input$strata6 %in% paste(tr6("Cause"))){
      
      ## reactive data subset
      ggplot(income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                  income_dis_categ$Cause %in% c("Group I","Group II", "Group III") &
                                  income_dis_categ$Group %in% "Both", ],
               aes(interaction(income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                                  income_dis_categ$Cause %in% c("Group I","Group II", "Group III") &
                                                  income_dis_categ$Group %in% "Both", ][,tr6("Cause")],
                               income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                                  income_dis_categ$Cause %in% c("Group I","Group II", "Group III") &
                                                  income_dis_categ$Group %in% "Both", ][,tr6("Year")]),
                   income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                      income_dis_categ$Cause %in% c("Group I","Group II", "Group III") &
                                      income_dis_categ$Group %in% "Both", ][,input$burden6],
                   fill= income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                            income_dis_categ$Cause %in% c("Group I","Group II", "Group III") &
                                            income_dis_categ$Group %in% "Both", ][,tr6("Income")]))+
        geom_bar(stat="identity", position = "dodge", col="black", width = 0.6)+
        #stat_summary(fun.y=sum, geom="bar",position ="dodge", colour="black", width = 0.6)+
        #scale_fill_manual(values = palette(rainbow(4)), aesthetics = "fill")+
        #facet_wrap(~)+
        theme_bw() +
        labs(y=paste(input$burden6, tr6(" (per 100,000 population) ")), x="")+
        theme(legend.position = "right", axis.title = element_text(family = "", color="black", face="bold", size=16))+
        theme(axis.text.x = element_text(size=12, color = "black", face="bold", angle = 25, hjust = 1))+
        theme(axis.text.y = element_text(size=12, color = "black", face="bold"))+
        theme(panel.spacing = unit(2, "lines"))+
        scale_fill_discrete(name = tr6("Income"))+
        annotate("text", x = Inf, y = 0, label = "@Christian.Dide-Agossou", 
                 hjust=1, vjust=-1, col="grey40",cex=3, fontface = "bold", alpha = 0.8)
      
    }  
    
    else if(input$strata6 %in% paste(tr6("Sex"))){
      
      ## reactive data subset
      ggplot(income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                income_dis_categ$Cause %in% "All Causes" &
                                income_dis_categ$Group %in% c("Male","Female"), ],
             aes(interaction(income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                                income_dis_categ$Cause %in% "All Causes" &
                                                income_dis_categ$Group %in% c("Male","Female"), ][,tr6("Group")],
                             income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                                income_dis_categ$Cause %in% "All Causes" &
                                                income_dis_categ$Group %in% c("Male","Female"), ][,tr6("Year")]),
                 income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                    income_dis_categ$Cause %in% "All Causes" &
                                    income_dis_categ$Group %in% c("Male","Female"), ][,input$burden6],
                 fill= income_dis_categ[income_dis_categ$Year %in% c("2000","2010", "2015") & 
                                          income_dis_categ$Cause %in% "All Causes" &
                                          income_dis_categ$Group %in% c("Male","Female"), ][,tr6("Income")]))+
        geom_bar(stat="identity", position = "dodge", col="black", width = 0.6)+
        #stat_summary(fun.y=sum, geom="bar",position ="dodge", colour="black", width = 0.6)+
        #scale_fill_manual(values = palette(rainbow(4)), aesthetics = "fill")+
        #facet_wrap(~Group)+
        theme_bw() +
        labs(y=paste(input$burden6, tr6(" (per 100,000 population) ")), x="")+
        theme(legend.position = "right", axis.title = element_text(family = "", color="black", face="bold", size=16))+
        theme(axis.text.x = element_text(size=12, color = "black", face="bold", angle = 25, hjust = 1))+
        theme(axis.text.y = element_text(size=12, color = "black", face="bold"))+
        theme(panel.spacing = unit(2, "lines"))+
        scale_fill_discrete(name = tr6("Income"))+
        annotate("text", x = Inf, y = 0, label = "@Christian.Dide-Agossou", 
                 hjust=1, vjust=-1, col="grey40",cex=3, fontface = "bold", alpha = 0.8)
      
    }
    
  })
  
 
  
############################################################################################################################################                 
### 7TH TAB-PLOT 
  
  ## Translation for TAB7
  tr7 <- function(text){
    sapply(text,function(s) translation[[s]][[input$language7]], USE.NAMES=FALSE)
  }
  
  ## UI for TAB7
  output$head7 <- renderUI({
    helpText(h3(em(tr7("Leading causes of burden"))))
  })
  
  output$uiburden7 <- renderUI({
    selectInput("burden7", 
                tr7("Choose a burden measure:"), 
                choices=tr7(c("DALY", "YLL", "YLD", "Mortality")))
  })
  
#    c(paste(as.character(unique(country_dis_spec$Burden)), sep = ",")))
  output$uicountry7 <- renderUI({
    selectInput("country7", 
                label = tr7("Choose a country:"),
                choices = tr7(unique(country_dis_spec$Country)),
                selected = tr7("Global"))
  })
  
 
  output$uicmt71 <- renderUI({
    helpText(em(tr7("The 10 leading categories of burden between 2000 and 2015 are detailed globally and for every country. Over the years, categories classified under Group II are becoming more predominant whereas categories classified under Group I are becoming less predominant.")))
  })
  
  
  ## MAINPANEL for TAB7
  
  output$msg71 <- renderText({paste(input$country7, tr7(": 10 leading categories of: ("), input$burden7, tr7("per 100,000 population)"))})
  
  
  df7 <- reactive({
    country_dis_spec[country_dis_spec$Sex %in% c("Both") & 
                       country_dis_spec$Year %in% c("2000", "2010", "2015") &
                       country_dis_spec[, tr7("Burden")] %in% input$burden7 &
                       country_dis_spec[, tr7("Country")] %in% input$country7, ]
  })
  
  output$leadingPlot <- renderTable({
    ## reactive data subset
    data7 <- df7()
    #data7 <- data7[, -c(1,4,5, 8:13)]
    data7 <- data7[, -c(1,4,5, 10:13)]
    data7$values <- round(data7$values, 2)
    suppressWarnings(data7_wide <- recast(data7, Rank ~ variable+Year, id.var = c("Rank", "Year")))

    ## combine corresponding columns
    data7_wide$`2000` <- paste0(data7_wide[, tr7("DiseaseGroup_2000")], " ", "(", data7_wide$values_2000, ")")
    data7_wide$`2010` <- paste0(data7_wide[, tr7("DiseaseGroup_2010")], " ", "(", data7_wide$values_2010, ")")
    data7_wide$`2015` <- paste0(data7_wide[, tr7("DiseaseGroup_2015")], " ", "(", data7_wide$values_2015, ")")
    
    data7_wide$Rang <-  data7_wide$Rank 
    data7_wide$Rango <-  data7_wide$Rank
    data7_wide[, c(tr7("Rank"), "2000", "2010", "2015")]
    #data7_wide[, c(1, 11:13)]
    
    
  }, spacing = "xs", bordered = TRUE, align = "c", width = 1950)
  
  
############################################################################################################################################                 
### 8TH TAB-PLOT 
  
  ## Translation for TAB7
  tr8 <- function(text){
    sapply(text,function(s) translation[[s]][[input$language8]], USE.NAMES=FALSE)
  }
  
  # UI for TAB8
  output$uicmt81 <- renderUI({
    helpText(h4(em(tr8("This ShinyApp application provides a simple and convenient way to review data on the burden of disease and disability across the world – by cause, gender, age, WHO region, WBI group, and country. This application can be explored by anyone or any country."))))
  })
  
  output$uicmt82 <- renderUI({
    helpText(strong(tr8("References:")))
  })
  
  output$uicmt83 <- renderUI({
    helpText(tr8("Please visit the WHO website for more information on the data."))
  })
 # output$msg20 <- renderText({tr8("Please visit the WHO website for more information on the data.")})
  
  output$uicmt84 <- renderUI({
    helpText(tr8("Any questions or comments can be sent to:"))
  })
  
  
  output$uicmt85 <- renderUI({
    helpText(strong(tr8("Christian Dide-Agossou, MS, BS. (PhD Candidate)")))
  })
  
  output$uicmt86 <- renderUI({
  textInput("name", 
            tr8("Name:"),
            value="")
  })
  
  output$uicmt87 <- renderUI({
    textInput("from", 
              tr8("From:"), 
              value="xxx@gmail.com")
  })
 
   output$uicmt88 <- renderUI({
    textInput("to", 
              tr8("To:"), 
              value="christian.dide-agossou@cuanschutz.edu")
  })
  
   output$uimsg81 <- renderUI({
     textAreaInput("mgs", tr8("Message"), value = "", width = '100%', rows = 5, resize = "both")
   })
  
   
   output$uicmt89 <- renderUI({
    actionButton("send", 
                  tr8("Submit"))
  })
  
  
  ## MAINPANEL for TAB8
  #store the results
  Results <- reactive(c(
    input$name, input$from, input$to, input$mgs, Sys.time()
  ))
  
  #This will add the new row at the bottom of the dataset in Google Sheets.
  observeEvent(input$send, {                                                                 
    Data  <- Data  %>%                                                                      
      gs_add_row(ws = "Data", input = Results())                                                               
  })
  

############################################################################################################################################                 
  
}




############################################################################################################################################                 
############################################################################################################################################                 

# create my logo to be added to my plots
# img <- png::readPNG("../visualization/Chris QR code2.png")
# rast <- grid::rasterGrob(img, 
#                          x = unit(0.5, "npc"), y = unit(0.5, "npc"),
#                          width = 0.7, height = 0.3)



## import logo as raster image
# m <- png::readPNG("../visualization/Chris QR code2.png")
# w <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * 0.2), # adjust alpha
#             nrow=dim(m)[1])



# 
# library(shiny)    # for shiny apps
# library(leaflet)  # renderLeaflet function
# library(spData)   # loads the world dataset 
# ui = fluidPage(
#   sliderInput(inputId = "life", "Life expectancy", 49, 84, value = 80),
#   leafletOutput(outputId = "map")
# )
# server = function(input, output) {
#   output$map = renderLeaflet({
#     leaflet() %>% addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
#       addPolygons(data = world[world$lifeExp < input$life, ])})
# }
# shinyApp(ui, server)