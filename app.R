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
#Read in data
#----------------------------------

faculty_test <- read_csv(here("data", "faculty_test.csv"))

faculty_list <- read_csv(here("data", "faculty_list.csv"))




# #And to make those links responsive to your data:
# table_df %>%
#   mutate(website_html = sprintf('<a href="%s">UCSB website</a>', website_url),
#          image_html   = sprintf('<img src="%s"></img>', image_url))


#----------------------------------
#Create the user interface:
#----------------------------------
ui <- fluidPage(
    navbarPage("ERI Global",
               theme = shinytheme("darkly"),
               #theme = "style/style.css",

               ####INTRO tab####
               tabPanel(title = "Intro",
                        icon = icon("user-circle"),
                        #includeHTML("home.html"),
                        mainPanel(width = 20,
                                  align = "center",
                                  h2(strong("Global Food Initiative")),
                                  img(src = "images/UCSB_campus.jpg",
                                      width = "450px",
                                      height = "300px"),
                                  #imageOutput('campus'),
                                  hr(),
                                  h5("The University of California Global Food Initiative addresses one of the critical issues of our time: how to sustainably and nutritiously feed a world population expected to reach eight billion by 2025.",
                                     br(),
                                     br(),
                                     "This is a database of faculty at UCSB who's research is involved with sustainable food systems. This purpose of this database is to serve as an up to date, live asset map for faculty involved to be hosted in one place. Currently, there are 50+ faculty in the database. If you don't see someone who you think should be included please send their information to XXXX. "),
                                  hr(),
                                  fluidRow(column(4,
                                                  img(id = "food_home",
                                                      src = "images/food_home.jpeg",
                                                      width = "250px",
                                                      height = "200px",
                                                      style = "cursor:pointer;"),
                                                        useShinyjs(),
                                                          tabsetPanel(id="navbar",
                                                                      tabPanel("tab1", p("This is tab 1")),
                                                                      tabPanel("tab2", p("This is tab 2")))),
                                           column(4,
                                                  img(src = "images/water_home.jpeg",
                                                      width = "250px",
                                                      height = "200px")),
                                           column(4,
                                                  img(src = "images/energy_home.jpeg",
                                                      width = "250px",
                                                      height = "200px")),
                                  )
                                  
                        ),
                        br(),
                        br(),
                        hr(),
                        
                        ),

                ####FOOD Tab####
                tabPanel(title = "Food",
                        icon = icon("search"),
                        value = "Food_tab",
                        sidebarLayout(
                            sidebarPanel(h3("Explore Faculty", class = "text-success"),
                                         checkboxGroupInput("food_special", label = h3("Select Specializations:"),
                                                            # choices = list("Fisheries & Aquaculture" = 1,
                                                            #                "Economics & Policy" = 2,
                                                            #                "Soil" = 3,
                                                            #                "Land Use" = 4,
                                                            #                "Agriculture" = 5,
                                                            #                "Ecology & Conservation" = 6,
                                                            #                "Social Sciences & Anthropology" = 7,
                                                            #                "Physical science & Engineering" = 8,
                                                            #                "Climate Change" = 9),
                                                            # selected = 1)),
                            choices = unique(faculty_list$specialization))),
                                         # hr(),
                                         # fluidRow(column(3, verbatimTextOutput("value"))),
                         mainPanel(h1("Food Faculty", class = "text-secondary"),
                                   tableOutput("table"))
                                   
         )),
         
                ####WATER Tab####
                tabPanel(title = "Water",
                        icon = icon("search"),
                        sidebarLayout(
                            sidebarPanel(h3("Select a specialization", class = "text-success"),
                                         selectInput("select", label = h3("Select a specialization:"),
                                                     choices = list("Hydrology" = 1, "Water Resources" = 2, "Markets" = 3, "Conservation" = 4, "Climate Change" = 5, "Economics" = 6, "Policy" = 7),
                                                     selected = 1)),
                            mainPanel(h1("Water Faculty", class = "text-secondary"))
                  )),
         
                ####ENERGY Tab####
                tabPanel(title = "Energy",
                        icon = icon("search"),
                        sidebarLayout(
                            sidebarPanel(h3("Select a specialization",class = "text-success"),
                                         selectInput("select", label = h3("Select a specialization:"),
                                                     choices = list("Engineering" = 1, "Technology" = 2, "Social Sciences" = 3, "Climate Change" = 4, "Renewables" = 5, "Transportation" = 6, "Economics" = 7, "Policy" = 8),
                                                     selected = 1)),
                            mainPanel(h1("Energy Faculty", class = "text-secondary"))
                        ))
         
         )
         
)
               
#--------------------------
# Server
#--------------------------

server <- function(input, output) {
  
  #Food tab test
  # output$table <- renderDataTable({
  #   subset(faculty_test, specialization %in% input$specialization, select = "name")
  # })
  
  #Checkbox Widget
  # output$value <- renderPrint({ input$checkGroup })
  
  ####Food Tab####
  #Food input df
  table_df_food <- reactive({
    faculty_list %>%
      filter(specialization %in% input$food_special) %>% 
      select(department, name, email, role, website_url)
      # mutate(website_html = sprintf('<a href="%s">UCSB website</a>', website_url),
      #      image_html = sprintf('<img src="%s"></img>', image_url))
      # 
  })
  
  #Food output table
  output$table <- renderTable({
    table_df_food()
    
  })
  #   table_df_food %>%
  #     mutate(tmp = '<a href="https://github.com">blah</a>',
  #            img = '<img src="https://www.fws.gov/fisheries/freshwater-fish-of-america/images/originals/east_cold/American_shad_DuaneRavenArt.fw.png"></img>')
  # }, sanitize.text.function = function(x) x)
  
  
#   #Homepage images to tabs
#   shinyjs::onclick("food_home",  updateTabsetPanel(session, inputId="navbar", selected="tab2"))

}


#--------------------------
# Run the application 
#--------------------------

shinyApp(ui = ui, server = server)
