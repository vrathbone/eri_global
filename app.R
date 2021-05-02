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


# get all values of specialization
# a. combine all into one string, split by ';', trim whitespace
# b. for each value, split by ';', trim whitespace
#    then unnest or collapse all lists into one long list
# then make list unique


faculty_list <- read_csv(here("data", "faculty_list.csv")) %>% 
  # mutate(spec_single = str_split(specialization, ';')) %>%
  # unnest(spec_single) %>%
  mutate(website_url = sprintf('<a href="%s">UCSB website</a>', website_url),
         image_url = sprintf('<img src="%s"></img>', image_url))

sublist <- faculty_list %>% 
  select(specialization) %>% 
  mutate(specialization = str_split(specialization, ';')) %>% 
  unnest(specialization) %>%
  mutate(specialization = str_trim(specialization)) %>% 
  arrange(specialization)

sublist <- unique(sublist)

# #How to separate specializations using str_detect:
# filter(str_detect(specialization, paste0(input$checkbox_checked, collapse = '|'))
#        which turns the input$checkbox_checked vector into a pattern of values separated by | which is interpreted as “or” (so, do you detect checkbox 1 OR checkbox 2 OR so on…)


#----------------------------------
#Create the user interface:
#----------------------------------
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style/css_test.css")
  ),
      navbarPage("ERI GLOBAL",
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
                                                      src = "images/food_home.jpg",
                                                      width = "250px",
                                                      height = "200px",
                                                      style = "cursor:pointer;")
                                                  ),
                                                        # useShinyjs(),
                                                        #   tabsetPanel(id="navbar",
                                                        #               tabPanel("tab1", p("This is tab 1")),
                                                        #               tabPanel("tab2", p("This is tab 2")))),
                                           column(4,
                                                  img(src = "images/water_home.jpg",
                                                      width = "250px",
                                                      height = "200px")
                                                  ),
                                           column(4,
                                                  img(src = "images/energy_home.jpg",
                                                      width = "250px",
                                                      height = "200px")
                                                  ),
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
                                         checkboxGroupInput("food_checkbox", label = h3("Select Specializations:"),
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
                            choices = unique(faculty_list$spec_single))),
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

  ####Food Tab####
  #Food input df
  table_df_food <- reactive({
    faculty_list %>%
      filter(spec_single %in% input$food_checkbox) %>%
      filter(str_detect(specialization, paste0(input$food_checkbox, collapse = '|'))) %>%
      select(image_url, department, name, email, role, website_url) %>% 
      rename("-" = image_url,
             "Department" = department,
             "Name" = name,
             "Email" = email,
             "Role" = role,
             "UCSB Website" = website_url)
      # mutate(id = as.numeric(factor(spec_single))) %>%
      # pivot_wider(spec_single,
      #             names_from = spec_single,
      #             values_from = id)

  })
  
  #Food output table
  output$table <- renderTable({
    table_df_food()
  }, sanitize.text.function = function(x)x)
  
#   #Homepage images to tabs
#   shinyjs::onclick("food_home",  updateTabsetPanel(session, inputId="navbar", selected="tab2"))

}


#--------------------------
# Run the application 
#--------------------------

shinyApp(ui = ui, server = server)


#--------------------------
# Parking Lot
#--------------------------

#Checkbox Widget
# output$value <- renderPrint({ input$checkGroup })

#Food tab test
# output$table <- renderDataTable({
#   subset(faculty_test, specialization %in% input$specialization, select = "name")
# })
