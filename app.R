#
# This is a Shiny web application for estimating privacy risks posed 
# by common FGB data types and research populations
#

library(shiny)
library(bslib)
library(tidyverse)
library(shiny.destroy)


#load data 

load("data/rskData.RData")

# source helper functions

source("helper_fxs.R")


# Define UI for application:

# - design sidebar that allows users to:
#  > select all of the participant types used in their study
#  > select the general data types used so that the specific list
# is not super long
#  > select all of the specific data types used in their study based on
# the general data types chosen
#  > have an on-off button for "sensitive research topics"

# - design plot output that places result at the correct X,Y coordinate based on 
# the re-identification risk posed by data type (i.e. X), as well as any interacting factors
# that arise from combinations of data types, and the vulnerability risk posed
# by the research subjects (i.e. Y), with option to toggle sensitive on and off
# resulting plot should show where the overall risk lands against a colour background
# to show what colour category is achieved. The background will change if sensitivity
# is on so that higher risk colour is achieved.

# UI should also
# - have space for short text that states the resulting colour code
# - have option of tooltips or other form of "additional info" on answer options as needed
# - further additions to UI will come in future versions



# Define UI ----
ui <- page_sidebar(
  title = "Data risk levels",
  sidebar = sidebar(
    accordion(
      # adjust font size of choice options for genDataType
      tags$style("#genDataType input, .checkbox-inline {
        font-size: 13px;
        }"
      ),
      accordion_panel(
        "Data types",
        # select inputs for general data types to narrow down options of specific data types
        checkboxGroupInput("genDataType", 
                           label = span("Select general type(s) of data (optional)",
                                          span(
                                            tooltip(
                                              bsicons::bs_icon("info-circle"),
                                              "You may select one or more of the following general data types in 
                                              order to filter the choices shown when selecting specific types of data"
                                                )
                                              )
                                        ),
                           choices = c("Show all options", "Administrative data", "Location data", 
                                       "Audiovisual data", "Textual data", "Experimental/lab data",
                                       "Neuroimaging data", "Questionnaire data", "Sociodemographic data", "Economic/political data",
                                       "Educational data", "Physical characteristics/medical data", "Biological/genetic data"), inline = TRUE),
        # select inputs for specific data types to go on x-axis
        selectInput("dataType", 
                    label = span("Select specific type(s) of data",
                                 span(
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "Select all of the types of data you wish to assess in the current risk analysis"
                                    )
                                  )
                                 ),
                    choices = rskDatTyp$ShortDescription, multiple = TRUE, selectize = TRUE)
      ),
    
    # adjust font size of choice options for adultChild
    tags$style("#adultChild input, .checkbox {
      font-size: 13px;
      }"
    ),
    accordion_panel(
      "Participant types",
      # select inputs for whether participants are adults or children
      checkboxGroupInput("adultChild", 
                         label = span("Select participant age group (optional)",
                                      span(
                                        tooltip(
                                          bsicons::bs_icon("info-circle"),
                                          "You may select the age grouping of the participants in order to 
                                          filter the choices shown when selecting the specific participant type(s)"
                                        )
                                      )),
                         choices = c("Adult participants", "Children participants")),
      # select inputs for vulnerability of participants on y-axis
      selectInput("particType", 
                  label = span("Select type(s) of research participants",
                               span(
                                 tooltip(
                                   bsicons::bs_icon("info-circle"),
                                   "Select all of the participant types you wish to assess in the current risk analysis"
                                 )
                               )),
                  choices = rskResPart$ShortDescription, multiple = TRUE, selectize = TRUE)
      )
    ),
    actionButton("view", "View results"),
    actionButton("reset", "Clear all")
    
  ),
  card(plotOutput("XYplot")),
  card(textOutput("text"))
)

# Define server logic ----
server <- function(input, output, session) {
  
  genDatSubset <- reactive({
    req(input$genDataType)
    combined_pattern <- paste(input$genDataType, collapse = "|")
    filter(rskDatTyp, str_detect(GenDataType, combined_pattern))
  })
  
  observeEvent(genDatSubset(), {
    choices <- genDatSubset()$ShortDescription
    updateSelectInput(inputId = "dataType", choices = choices)
  })
  
  maxX <- eventReactive(input$view, {
    req(input$dataType)
    
    df <- rskDatTyp %>% filter(ShortDescription %in% input$dataType) 
    
    lvlCalcFx(excepToIncrFx(adjustINX(df, 
                                      typeINXfx(df), 
                                      resultINX)))
    
  })
  
  particSubset <- reactive({
    req(input$adultChild)
    partTypes <- str_remove(input$adultChild, " participants")
    combined_pattern <- paste(partTypes, collapse = "|")
    filter(rskResPart, str_detect(AgeCateg, combined_pattern))
  })
  
  observeEvent(particSubset(), {
    choices <- particSubset()$ShortDescription
    updateSelectInput(inputId = "particType", choices = choices)
  })
  
  maxY <- eventReactive(input$view, {
    req(input$particType)
    
    rskResPart %>% filter(ShortDescription %in% input$particType) %>%
      summarise(maxY = max(Lvl))
  })
  
  
  
  xyPair <- reactive({
    cbind(maxX(), maxY())
  })
  

  
  output$XYplot <- renderPlot({
    ggplot(xyPair(), aes(x = maxX, y = maxY)) +
      geom_point(size = 10) +
      xlim(1,5) +
      ylim(1,3.5) +
      xlab("Re-identifiability risk") +
      ylab("Participant vulnerability risk") +
      theme(axis.text=element_text(size=20),
            axis.title=element_text(size=24,face="bold"))
    
  })
  
  output$text <- renderText({
    paste("Data risk level is", maxX(), "and participant vulnerability level is", maxY())
  }
    
  )
  
  observeEvent(input$reset, {
    #when actionButton reset is clicked clear all inputs
    updateSelectInput(inputId = "genDataType", selected=character(0))
    updateSelectInput(inputId = "dataType", selected=character(0))
    updateSelectInput(inputId = "adultChild", selected=character(0))
    updateSelectInput(inputId = "particType", selected=character(0))
  }
  ) 
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)