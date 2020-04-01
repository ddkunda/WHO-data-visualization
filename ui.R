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


# Define a UI for the Shiny app
ui <- navbarPage(
  
  navbarPage(inverse=TRUE,
             "DISEASE BURDEN AND MORTALITY ESTIMATES",
             #tags$head(includeScript("google-analytics.js")),
             #tags$head(includeHTML("google-analytics.html")),
             #tags$head(includeText("google-analytics.txt")),
             #includeCSS("cerulean.css"),
             
             
############################################################################################################################################                 
### 1ST TAB-PANEL
        tabPanel("Global burden summary",
                 radioButtons(inputId = "language1", label = "",
                              choices = c("English" = "en", "Français" = "fr", "Español" = "es"),
                              inline = TRUE,
                              selected = "en"),
                 
                 titlePanel(uiOutput("head1")),
                             
                 sidebarLayout( 
                            sidebarPanel(width=4,
                                         tags$style(".well {background-color:beige}"),
                                         
                                         uiOutput("uiburden1"),
                                        
                                         hr(),
                                         
                                         uiOutput("uiyear1"),
                                        
                                         hr(),
                                         
                                         uiOutput("uicmt11")
                                         
                                         ),
                            
                            # Create a spot for the barplot
                            mainPanel(
                              span(textOutput("msg11"), style="color:red"),
                             
                               leafletOutput("map", height = 525),
                              
                              span(textOutput("msg12")),
                             
                              hr(),
                    
                              span(textOutput("msg13")),
                              
                              br(),
                              
                              span(textOutput("msg14"))
                              
                            )
                          )
                 ),
                 
                 

############################################################################################################################################                 
### 2ND TAB-PANEL
                tabPanel("Burden by Cause",
                         radioButtons(inputId = "language2", label = "",
                                      choices = c("English" = "en", "Français" = "fr", "Español" = "es"),
                                      inline = TRUE,
                                      selected = "en"),
                         
                         titlePanel(uiOutput("head2")),
                         
                         sidebarLayout(
                           sidebarPanel(width=3,
                             
                                        uiOutput("uiburden2"),
                                        
                                        uiOutput("uicountry2"),
                                        
                                        hr(),
                                        
                                        uiOutput("uicmt21")
                            ),
                              
                            # Create a spot for the barplot
                           mainPanel(
                           column(6,"",offset = 0,
                                  plotOutput("causePlot1")),
                           
                           column(6,"", offset = 0,
                                  plotOutput("causePlot2")),
                        
                           column(12, offset = 0,
                                 
                                   hr(),
                                  
                                  uiOutput("uicmt22"))
                        )
                        )
                        ),
                 
     
############################################################################################################################################                 
### 3RD TAB-PANEL
                 tabPanel("Burden by Sex",
                          radioButtons(inputId = "language3", label = "",
                                       choices = c("English" = "en", "Français" = "fr", "Español" = "es"),
                                       inline = TRUE,
                                       selected = "en"),
                          
                          titlePanel(uiOutput("head3")),
                        
                          sidebarLayout(
                            sidebarPanel(width=3,
                                         
                                         uiOutput("uiburden3"),
                                         
                                         uiOutput("uicountry3"),
                                         
                                         hr(),
                                         
                                         uiOutput("uicmt31")
                            ),
                            
                            # Create a spot for the barplot
                            mainPanel(
                              
                              column(6,"",offset = 0,
                                     plotOutput("sexPlot1")),
                              
                              column(6,"", offset = 0,
                                     plotOutput("sexPlot2"))
                              
                              )
                            )
                          ),


############################################################################################################################################                 
### 4TH TAB-PANEL
tabPanel("Burden by Age",
         radioButtons(inputId = "language4", label = "",
                      choices = c("English" = "en", "Français" = "fr", "Español" = "es"),
                      inline = TRUE,
                      selected = "en"),
         
         titlePanel(uiOutput("head4")),
       
         sidebarLayout(      
           sidebarPanel(width=3,
                        
                        uiOutput("uiburden4"),
                        
                        uiOutput("uicountry4"),
                        
                        hr(),
                        
                        uiOutput("uicmt41")
           ),
           
            
        
           # Create a spot for the barplot
           mainPanel(
             
             column(12,"",offset = 0,
                    plotOutput("agePlot1")),
             
             column(12,"", offset = 0,
                    plotOutput("agePlot2"))
          
              )
           )
         ),



############################################################################################################################################                 
### 5TH TAB-PANEL
tabPanel("Burden by WHO Region",
         radioButtons(inputId = "language5", label = "",
                      choices = c("English" = "en", "Français" = "fr", "Español" = "es"),
                      inline = TRUE,
                      selected = "en"),
         
         titlePanel(uiOutput("head5")),
         
         sidebarLayout(      
           sidebarPanel(width=3,
                        
                        uiOutput("uiburden5"),
                        
                        uiOutput("uistrata5"),
                        
                        hr(),
                        
                        uiOutput("uicmt51"),
                        
                        br(),
                        
                        uiOutput("uicmt52")
                       
           ),
           
                 
           
          # Create a spot for the barplot
          mainPanel(
           
             column(12,"",offset = 0,
                   plotOutput("regionPlot1")),
             
            column(12,"", offset = 0,
                   plotOutput("regionPlot2")),
            
            column(12, offset = 0,
                   uiOutput("uicmt53"))
                   
            )
           )
         ),


############################################################################################################################################                 
### 6TH TAB-PANEL
tabPanel("Burden by WBI Group",
         radioButtons(inputId = "language6", label = "",
                      choices = c("English" = "en", "Français" = "fr", "Español" = "es"),
                      inline = TRUE,
                      selected = "en"),
         
         titlePanel(uiOutput("head6")),
         
         sidebarLayout(      
           sidebarPanel(width=3,
                        
                         uiOutput("uiburden6"),
                         
                         uiOutput("uistrata6"),
                         
                         hr(),
                         
                         uiOutput("uicmt61"),
                         
                         br(),
                         
                         uiOutput("uicmt62")
                         
             ),        
             
           # Create a spot for the barplot
           mainPanel(
             
             column(12,"",offset = 0,
                    plotOutput("wbiPlot1")),
            
              column(12,"", offset = 0,
                    plotOutput("wbiPlot2")),
             
             column(12, offset = 0,
                    uiOutput("uicmt63"))
             
             )
           )
         ),

															

############################################################################################################################################                 
### 7TH TAB-PANEL
tabPanel("10 Leading Categories",
         
         radioButtons(inputId = "language7", label = "",
                      choices = c("English" = "en", "Français" = "fr", "Español" = "es"),
                      inline = TRUE,
                      selected = "en"),
         
         titlePanel(uiOutput("head7")),
        
         sidebarLayout(      
           sidebarPanel(width=2,
                        uiOutput("uiburden7"),
                        
                        uiOutput("uicountry7"),
                        
             hr(),
             
             uiOutput("uicmt71")
             
            
           ),
           
           # Create a spot for the barplot
           mainPanel(
             span(textOutput("msg71"), style="color:red"),
             
             br(),
             
             tableOutput("leadingPlot")
            
             )
           )
         ),



############################################################################################################################################                 
### 8TH TAB-PANEL
                tabPanel(
                  "More Information", 
                 
                   radioButtons(inputId = "language8", label = "",
                                choices = c("English" = "en", "Français" = "fr", "Español" = "es"),
                               inline = TRUE,
                               selected = "en"),
                  fluidRow(
                        column(6,
                               uiOutput("uicmt81"),
                         hr(),
                         
                         uiOutput("uicmt82"),
                         
                         a("(1) Global Health Estimates 2016: Disease burden by Cause, Age, Sex, by Country and by Region, 2000-2016. Geneva, World Health Organization; 2018.",
                         href="https://www.who.int/healthinfo/global_burden_disease/GlobalDALY_method_2000_2016.pdf"),
                         br(),
                         br(),
                         a("(2) World Population Prospects: The 2017 revision. New York: United Nations, Department of Economic and Social Affairs, Population Division; 2017.",
                                     href="https://esa.un.org/unpd/wpp/"),
                         br(),
                         br(),
                         a("(3) The World Health Organization's (WHO) new World Standard Population. Age Standardization of Rates: A New WHO Standard.",
                                   href="https://www.who.int/healthinfo/paper31.pdf"),
                         br(),
                         br(),
                         
                        uiOutput("uicmt83"),
                        
                         a("https://www.who.int/healthinfo/global_burden_disease/estimates/en/index1.html", href="https://www.who.int/healthinfo/global_burden_disease/estimates/en/index1.html"),
                         
                         hr(),
                  
                         a(strong("Codes"), href="https://www.dropbox.com/sh/w1ob7we5ko6anjf/AABv0JWlcEDlBjvu8LUGmHx0a?dl=0"),
                        
                         br(),
                         hr(),
                        
                        uiOutput("uicmt84"),
                        
                        uiOutput("uicmt85"),
                       
                        a("christian.dide-agossou@cuanschutz.edu",
                        br(),
                        href="mailto:christian.dide-agossou@cuanschutz.edu")

                         ),
                         
                         
                      column(4, offset = 1,
                             uiOutput("uicmt86"),
                             uiOutput("uicmt87"),
                             uiOutput("uicmt88"),
                             uiOutput("uimsg81"),
                             uiOutput("uicmt89")
                       
                       )
                      )
                )


############################################################################################################################################                 

)
)

         