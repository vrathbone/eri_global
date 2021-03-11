#----------------------------------
# Attach packages  
#----------------------------------

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(here)
library(ggplot2)
library(ggridges)
library(forcats)
library(tidyverse)
library(here)
library(janitor)
library(LBSPR)
library(dplyr)
library(kableExtra)
library(ggalt)
library(ggrepel)
library(knitr)
library(directlabels)
library(gridExtra)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(workflowr)
library(plotly)
library(DT)
library(randomcoloR)
library(shinyjs)
library(lemon)
library(bsplus)
library(shinyBS)
library(shinyalert)

#----------------------------------
#Create the user interface:
#----------------------------------

#----------------------------------
#Create the user interface:
#----------------------------------
ui <- fluidPage(
    navbarPage("ERI Global",
               theme = shinytheme("darkly"),
               
               #MAIN Tab
               # tabPanel(title = "About",
               #          icon = icon("info"),
               #          mainPanel(h1("Global Food Initiative", 
               #                       class = "text-secondary", 
               #                       position = "center"), "The University of California Global Food Initiative addresses one of the critical issues of our time: how to sustainably and nutritiously feed a world population expected to reach eight billion by 2025.",
               #                    br(),
               #                    br(),
               #                       "This is a database of faculty at UCSB who's research is involved with sustainable food systems. This purpose of this database is to serve as an up to date, live asset map for faculty involved to be hosted in one place. Currently, there are 50+ faculty in the database. If you don't see someone who you think should be included please send their information to XXXX. ")),
               
               #INTRO tab
               tabPanel(title = "Intro",
                        icon = icon("user-circle"),
                        mainPanel(width = 20,
                                  align = "center",
                                  h2(strong("Global Food Initiative")),
                                  img(src = "",
                                      width = "350px",
                                      height = "200px"),
                                  #Robert Lang, Spiny lobster, Getty Images
                                  hr(),
                                  h5("The University of California Global Food Initiative addresses one of the critical issues of our time: how to sustainably and nutritiously feed a world population expected to reach eight billion by 2025.",
                                     br(),
                                     br(),
                                     "This is a database of faculty at UCSB who's research is involved with sustainable food systems. This purpose of this database is to serve as an up to date, live asset map for faculty involved to be hosted in one place. Currently, there are 50+ faculty in the database. If you don't see someone who you think should be included please send their information to XXXX. "),
                                  hr(),
                                  fluidRow(column(4,
                                                  img(src = "",
                                                      width = "15%",
                                                      height = "auto"),
                                                  imageOutput('placeholder')),
                                           # imageOutput('fish_wildlife_logo'))),
                                           column(4,
                                                  img(src = "",
                                                      width = "15%",
                                                      height = "auto"),
                                                  imageOutput('placeholder2')),
                                           column(4,
                                                  img(src = "",
                                                      width = "15%",
                                                      height = "auto"),
                                                  imageOutput('placeholder3')),
                                  )
                                  
                        ),
                        br(),
                        br(),
                        hr(),
                        
                        ),

                #FOOD Tab
                tabPanel(title = "Food",
                        icon = icon("search"),
                        sidebarLayout(
                            sidebarPanel(h3("Explore Faculty",class = "text-success"),
                                         selectInput("select", label = h3("Select a specialization:"), 
                                                     choices = list("Fisheries" = 1, "Policy" = 2, "Soil" = 3, "Land Use" = 4, "Agriculture" = 5), 
                                                     selected = 1)),
                         mainPanel(h1("Food Faculty", class = "text-secondary"))
         )),
         
                #WATER Tab
                tabPanel(title = "Water",
                        icon = icon("search"),
                        sidebarLayout(
                            sidebarPanel(h3("Select a specialization",class = "text-success")),
                            mainPanel(h1("Water Faculty", class = "text-secondary"))
                  )),
         
                #ENERGY Tab
                tabPanel(title = "Energy",
                        icon = icon("search"),
                        sidebarLayout(
                            sidebarPanel(h3("Select a specialization",class = "text-success"),
                                   # selectInput(inputId = "pick_map", 
                                   #             label = "Select the metric to view on the map:", 
                                   #             choices = c(),
                                   
                            ),
                            mainPanel(h1("Energy Faculty", class = "text-secondary"))
                        ))
         
         )
         
)
               
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Images for intro page
  output$placeholder <- renderImage({
    list(src = "", 
         alt = "", 
         width = "200px", 
         height = "150px"
    )
  }, deleteFile = FALSE)
  
  #Images for intro page
  output$placeholder2 <- renderImage({
    list(src = "", 
         alt = "", 
         width = "200px", 
         height = "150px"
    )
  }, deleteFile = FALSE)
  
  #Images for intro page
  output$placeholder3 <- renderImage({
    list(src = "", 
         alt = "", 
         width = "200px", 
         height = "150px"
    )
  }, deleteFile = FALSE)
  
  #Select Widget
  output$value <- renderPrint({ input$select })

}

# Run the application 
shinyApp(ui = ui, server = server)
