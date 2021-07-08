
# Shiny App for the Global Food Initiative in collaboration with UCSB and ERI
# 

#----------------------------------
# Attach packages  
#----------------------------------

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(here)
library(tidyverse)
library(here)
library(janitor)
library(viridis)
library(RColorBrewer)
library(workflowr)
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

# Read in the faculty list dataframe, assign html links to individual faculty websites and image URLs
# If you want add more URls, assign them within the mutate() function, remember you will also need to call them in the reactive{()} function in the server. 

faculty_list <- read_csv(here("data", "faculty_list.csv")) %>% 
  mutate(website_url = sprintf('<a href="%s">UCSB website</a>', website_url),
         image_url = sprintf('<img src="%s"></img>', image_url))

# Make a list of unique specializations for checkboxes from the faculty list
# No need to change this code as long as the specializations are entered into the faculty_list.csv and are separated with a semicolon.

sublist <- faculty_list %>% 
  select(specialization) %>% 
  mutate(specialization = str_split(specialization, ';')) %>%
  unnest(specialization) %>%
  mutate(specialization = str_trim(specialization)) %>%  #removing whitespace
  arrange(specialization)

sublist <- unique(sublist) #making values unique

#make sublists for each tab (food, water, energy)

sublist_food <- sublist %>%
  filter(specialization %in% c("Agriculture", "Climate Change", "Ecology & Conservation", "Economics & Policy", "Fisheries & Aquaculture", "Land Use", "Physical Science & Engineering", "Social Sciences & Anthropology", "Soil"))

sublist_water <- sublist %>%
  filter(specialization %in% c("Climate Change", "Conservation", "Economics & Policy", "Hydrology", "Policy", "Social Sciences", "Water Resources"))

sublist_energy <- sublist %>%
  filter(specialization %in% c("Climate Change", "Economics & Policy", "Engineering", "Social Sciences", "Technology", "Transportation"))


#----------------------------------
#Create the user interface:
#----------------------------------

# Using fluidPage() to make the site reactive to different displays (i.e. works on a phone, tablet or desktop)
# Here, you can pull in the css file. It's stored in the www/style folder, the css right now only controls the size of the faculty images to display them as uniform thumbnails
# ANY images/logos (besides faculty images) need to be stored in the www folder or they will not be displayed

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style/css_test.css"),
    tags$script("src" = "test_link.js")), #testing image links
      navbarPage("ERI GLOBAL",
               theme = shinytheme("darkly"), #many theme options, can change
  # navbarPage(title = div(img(src='global_food_logo_white.png',style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 60)),
  #            windowTitle="ERI GLOBAL",

               ####INTRO tab####
               # This is the main "landing page" for the site
               tabPanel(title = "Intro",
                        icon = icon("user-circle"),
                        #includeHTML("home.html"),
                        mainPanel(width = 20,
                                  align = "center",
                                  h2(strong("Global Food Initiative")),
                                  img(src = "images/UCSB_campus.jpg",
                                      width = "450px",
                                      height = "300px"),
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
                                                      style = "cursor:pointer;",
                                                      tags$a(" ", onclick = "customHref('Food_tab')"))
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
                        value = "Food_tab", #have to keep camel case otherwise css won't work
                        sidebarLayout(
                            sidebarPanel(h3("Explore Faculty", class = "text-success"),
                                         checkboxGroupInput("food_checkbox", label = h3("Select Specializations:"), 
                                                            choices = unique(sublist_food$specialization))),
                         mainPanel(h1("Food Faculty", class = "text-secondary"),
                                   tableOutput("table_food"))
                                   
         )),
         
                ####WATER Tab####
                tabPanel(title = "Water",
                        icon = icon("search"),
                        value = "Water_tab",
                        sidebarLayout(
                            sidebarPanel(h3("Explore Faculty", class = "text-success"),
                                         checkboxGroupInput("water_checkbox", label = h3("Select Specializations:"), 
                                                            choices = unique(sublist_water$specialization))),
                        mainPanel(h1("Water Faculty", class = "text-secondary"),
                                  tableOutput("table_water"))
                  )),

                ####ENERGY Tab####
                tabPanel(title = "Energy",
                         icon = icon("search"),
                         value = "Energy_tab",
                         sidebarLayout(
                           sidebarPanel(h3("Explore Faculty", class = "text-success"),
                                        checkboxGroupInput("energy_checkbox", label = h3("Select Specializations:"),
                                                           choices = unique(sublist_energy$specialization))),
                           mainPanel(h1("Energy Faculty", class = "text-secondary"),
                                     tableOutput("table_energy"))
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
      filter(str_detect(specialization, paste0(input$food_checkbox, collapse = '|'))) %>%
      select(image_url, department, name, email, role, website_url) %>% 
      rename(" " = image_url,
             "Department" = department,
             "Name" = name,
             "Email" = email,
             "Role" = role,
             "UCSB Website" = website_url)
    
  })
  
  #Food output table
  output$table_food <- renderTable({
    table_df_food()
  }, sanitize.text.function = function(x)x)
  
  ####Water Tab####
  #Water input df
  table_df_water <- reactive({
    faculty_list %>%
      filter(str_detect(specialization, paste0(input$water_checkbox, collapse = '|'))) %>%
      select(image_url, department, name, email, role, website_url) %>%
      rename(" " = image_url,
             "Department" = department,
             "Name" = name,
             "Email" = email,
             "Role" = role,
             "UCSB Website" = website_url)

  })

  #Water output table
  output$table_water <- renderTable({
    table_df_water()
  }, sanitize.text.function = function(x)x)
  
  ####Energy Tab####
  #Energy input df
  table_df_energy <- reactive({
    faculty_list %>%
      filter(str_detect(specialization, paste0(input$energy_checkbox, collapse = '|'))) %>%
      select(image_url, department, name, email, role, website_url) %>%
      rename(" " = image_url,
             "Department" = department,
             "Name" = name,
             "Email" = email,
             "Role" = role,
             "UCSB Website" = website_url)
    
  })
  
  #Water output table
  output$table_energy <- renderTable({
    table_df_energy()
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
#   subset(faculty_list, specialization %in% input$specialization, select = "name")
# })
