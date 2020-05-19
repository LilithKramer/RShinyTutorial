##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Author: Lilith Kramer & Willem Stolte
## Goal: R Shiny practise app
## Version: 0.1 -- 08-apr-2019 -- script set up
##          0.2 -- 25-feb-2020 -- added functionaly
##          0.2 -- 15-may-2020 -- translated to eng.
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##~~~~~~~~~~~~~~~~~~~~~
##==== PREPARATION ====
##~~~~~~~~~~~~~~~~~~~~~


##==legend=====

## COMMENTS
## ##      = comment
## #       = outcommented code
## #!#     = should be checked if location of script changes 
## #?#     = to do
## ##==    = block of info separator

## OBJECTS
## fnXXX   = filename
## dirXXX  = directory (path) / foldername
## oXXX    = loaded object
## dfXXX   = dataframe      
## dtXXX   = datatable 



##==notes to self============
## tooltip: https://stackoverflow.com/questions/36670065/tooltip-in-shiny-ui-for-help-text



##==install packages and open libraries ====

# install.packages("shiny")          ## shiny
# install.packages("shinydashboard") ## nice layouts for shiny
# install.packages("data.table")     ## working with data.frames the data.table way
# install.packages("leaflet")     
# install.packages("plotly")     
# install.packages("shinyjs")     
# install.packages("shinycssloaders")     
# install.packages("tidyverse")     
# install.packages("rgdal")     

library(shiny)         
library(shinydashboard)
library(data.table)
library(leaflet)
library(plotly)
library(shinyjs)
library(shinycssloaders)
library(tidyverse)
library(rgdal)


##==settings=======

options(scipen = 999)  ## no scientific numbering
options(shiny.reactlog = T) ## for help with debugging
options(shiny.maxRequestSize = 30*1024^2) ## maximum upload size is now 30Mb, standard for R shiny is 5Mb. 
## rshiny shinyapps.io max limit size = 32MB. 


##== load functions ====

source('functions/testfunction.R')


##==set paths ========

#!# directories
dirDATA <- "data/"

#!# files
fnKM <- "km_min_max.csv"


##== set variables =======
variables = reactiveValues(something = NULL)


##==load files=========
dtKM <- fread(paste(dirDATA, fnKM, sep = ""))



##==User Interface Start==================

header  <- dashboardHeader(title = "Practise app")

sidebar <- dashboardSidebar(
  sidebarMenu(id = "side_tabs",
              menuItem(text = "Home", tabName = "start", icon = icon("home")),
              menuItem(text = "Test", tabName = "test", icon = icon("upload")),
              menuItem(text = "Help", tabName = "help", icon = icon("question"))
  ))

## rows have a grid width of 12, so a box with width = 4, takes up one third of the space
## tops will be lined out, bottoms not
## heights are in pixels.. 

body    <- dashboardBody(
  
  tabItems(
    #===start_tab==============
    tabItem(tabName = "start",
            fluidRow(
              box(title = "Welcome ",
                  solidHeader = T,
                  status = "success",
                  collapsible = F,
                  p("This is a practise app .."),
                  p("Check out this pug picture from pixabay!"),
                  img(src='pug-801826_640.jpg'),
                  width = 12))),

    #== test_tab==============
    tabItem(tabName = "test", 
            fluidRow(
              box(title = "Testpagina",
                  solidHeader = T,
                  status = "success",
                  collapsible = T,
                  collapsed = F,
                  width = 12,
                  p("Example text, followed by an example list:"), 
                    tags$ol(tags$li("This is a"),
                            tags$li("This is b")),
                  p("You can open the boxes below by clicking the '+'-sign.")
                  ),
              
             
              ##==== choose something =====
              box(title = "1. Select something",
                  solidHeader = T,
                  status = "success",
                  collapsible = T,
                  collapsed = T,
                  width = 12,
                  p("Click on a radiobutton."),
                  radioButtons(inputId = "rb_something", 
                               label = NULL, 
                               choices = c("Something 1", "Something 2", "Something 3")),
                  textOutput("selected_thing")),
              
            
              ##==== load data ========
              box(title = "2. Upload something",
                  solidHeader = T, 
                  status = "success",
                  collapsible = T,
                  collapsed = T,
                  width = 12,
                  p("Upload X here. Maximum file size is 30MB."),
                  fileInput("testInputFile", NULL,
                            multiple = FALSE,
                            accept = c("text/csv",
                                       "text/plain"),
                            buttonLabel = "Choose file",
                            placeholder = "No file selected."),
                  tabBox(id = "logSomeFile",
                         title = "",
                         side = "left", 
                         width = 12,
                         selected = "Display",
                         tabPanel("Display", 
                                  tableOutput("testDisplay") %>% withSpinner(type = 3, color = "#01A65A", color.background = "#ffffff")),
                         tabPanel("Download", 
                                  downloadButton("downloadData", "Download data again")),
                         tabPanel("Extra", 
                                  tableOutput("testDisplayInternalData"))
                         )),
        
              ##==== display data ========
              box(title = "3. Display something",
                  solidHeader = T, 
                  status = "success",
                  collapsible = T,
                  collapsed = T,
                  width = 12,
                  tabBox(width = 12,
                         tabPanel("GGPlot",
                                  checkboxInput("more_options", "Shore more options"),
                                  conditionalPanel(condition = "input.more_options == true",
                                                   checkboxInput("add_line", "Add linear fit")),
                                  plotOutput("ggplot_01")),
                         tabPanel("Plotly",
                                  plotlyOutput("plotly_01")),
                         tabPanel("Map",
                                  leafletOutput("leaflet_map_01")
                         )
                  )
              ))),

    ##====help_tab====
    tabItem(tabName = "help",
            fluidRow(
              box(title = "Help",
                  solidHeader = T,
                  status = "success",
                  collapsible = T,
                  collapsed = F,
                  width = 12,
                  h3("Help"),
                  downloadButton("testdocument", "Download document")
                  )))))
    

ui <- dashboardPage(skin = "green", header, sidebar, body,  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), tags$style(".leaflet-top {z-index:999!important;}"))) ## tags$head etc is necessary to get zoom-buttons of leaflet behind dropdown box  


##==SET SERVER=========

server <- function(input, output, session) {
  
  
  ##========= assign variables ==================
  observeEvent(input$rb_something, {
    
    if(input$rb_something == 'Something 1'){
    variables$something = "Something no. 1"
    } else if(input$rb_something == 'Something 2'){
      variables$something = "Something no. 2"
    } else if(input$rb_something == 'Something 3'){
      variables$something = "Something no. 3"}})
    
 
  ##========= display text ==================
  output$selected_thing <- renderText({
    req(input$rb_something)
    paste("Selected thing: ", input$rb_something, sep = "")
    })

  testfile <- reactive({
    req(input$testInputFile)
    temp_file <- read_file(input$testInputFile$datapath)
  })
  
  ##========= display table ==================
  output$testDisplay <- renderTable({
    req(testfile())
    df <- as.data.frame(testfile())
    if(nrow(df)>10){df <- df[1:10, ]}
    return(df)
  })
  
  ##========= display previously loaded data ===========
  output$testDisplayInternalData <- renderTable({
    req(dtKM)
    as.data.frame(dtKM)
  })

  ##========= display plot ==================
 output$ggplot_01 <- renderPlot({
    req(testfile())
    
    if(input$add_line == FALSE){p <- ggplot(testfile()) +
      geom_point(aes(min, max)) +
      theme_bw()}

    if(input$add_line == TRUE){ p <- ggplot(testfile()) +
      geom_point(aes(min, max)) +
      geom_smooth(aes(min, max), method = "lm") + 
      theme_bw()}
    
    return(p)
    
  })

##========= display interactive plot ==================
  
  output$plotly_01 <- renderPlotly({
    req(testfile())
    
    p <- ggplot(testfile(), aes(min, max)) +
      geom_point() +
      geom_smooth(method = "lm")
    
    return(ggplotly(p))
    
  })
  
  
  
  ##==== display map ========
  output$leaflet_map_01 <- renderLeaflet({
    req(testfile())
  
    df <- testfile()
    coordinates(df) <- (~locX+locY)
    proj4string(df) <- CRS("+init=epsg:28992")
    df.wgs <- spTransform(df, CRS("+proj=longlat +datum=WGS84"))
    
    leaflet(df.wgs) %>%
      addTiles('http://{s}.tile.openstreetmap.de/tiles/osmde/{z}/{x}/{y}.png') %>%
      addCircleMarkers(color = "grey", label = ~ paste(plant, value), labelOptions = options(noHide = T, textsize = "15px"))
    
  })

##====downloads======================

output$downloadData <- downloadHandler(
  filename = function(){
    paste(input$testInputFile, ".csv", sep = "")
  },
  content = function(file){  
  write.csv(testfile(), file, row.names = FALSE)
  }
)  

output$testdocument <- downloadHandler(
  filename = function(){paste("Cool_ferrari.pdf")},
  content <- function(file){file.copy("documentation/164_330_gt.pdf", file)},
  contentType = "application/pdf"
)
}

shinyApp(ui, server)


