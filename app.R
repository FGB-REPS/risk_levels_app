#
# This is a Shiny web application for estimating privacy risks posed 
# by common FGB data types and research populations
#

library(shiny)
library(shinyvalidate)
library(bslib)
library(tidyverse)
library(markdown)
library(rmarkdown)
library(knitr)



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
  theme = bs_theme(bootswatch = "cerulean"),
  title = "Data risk levels",
  sidebar = sidebar(
    accordion(
      # style adjustments first
      # adjust font size of choice options for input 'genDataType'
      tags$style("#genDataType .checkbox-inline {
        font-size: 13px;
        }"
      ),
      # adjust font size of choice options for input 'adultChild'
      tags$style("#adultChild .checkbox-inline {
      font-size: 13px;
      }"
      ),
      # adjust font size of choice options for input 'datActiv'
      tags$style("#datActiv .checkbox-inline {
      font-size: 15px;
      }"
      ),
      open = "Data types", # set first panel to open upon load
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
                           choices = c("Show all options", sort(c("Administrative data", "Location data", 
                                       "Audiovisual data", "Textual data", "Experimental/lab data",
                                       "Neuroimaging data", "Questionnaire data", "Sociodemographic data", "Economic/political data",
                                       "Educational data", "Physical characteristics/medical data", "Biological/genetic data"))), inline = TRUE),
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
                    choices = sort(rskDatTyp$ShortDescription), multiple = TRUE, selectize = TRUE),
        #create button to clear data type inputs
        actionButton("dataClear", "Clear inputs", class="btn-outline-primary")
        
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
                         choices = c("Adult participants", "Children participants"),
                         inline = TRUE),
        # select inputs for vulnerability of participants on y-axis
        selectInput("particType", 
                  label = span("Select type(s) of research participants",
                               span(
                                 tooltip(
                                   bsicons::bs_icon("info-circle"),
                                   "Select all of the participant types you wish to assess in the current risk analysis"
                                 )
                               )),
                  choices = sort(rskResPart$ShortDescription), multiple = TRUE, selectize = TRUE),
        #create button to clear participant type inputs
        actionButton("particClear", "Clear inputs", class="btn-outline-primary")
        ),
        accordion_panel(
          "Data sensitivity",
          radioButtons("sensitLvl",
                   label = span("Is the information within the data about the participants sensitive in nature?",
                                span(
                                  tooltip(
                                    bsicons::bs_icon("info-circle"),
                                    "Select 'yes' if the information could
                                    be harmful to the research subjects if leaked to the public"
                                  )
                                )),
                   choices= c("No", "Yes"),
                   selected = "No"
          )
      
        ),
        accordion_panel(
          "OPTIONAL: Data processing activities",
          checkboxGroupInput("datActiv", 
                           label = span("What data processing activities would you like advice on?",
                                        span(
                                          tooltip(
                                            bsicons::bs_icon("info-circle"),
                                            "If you only want guidance on specific activities, select
                                            the ones you would like to know about"
                                          )
                                        )),
                           choices = c("Select all", c("Storage during current project", 
                                                      "Sharing during current project",
                                                      "Use by students during current project", 
                                                      "Archiving after current project", 
                                                      "Reuse after current project")),
                           selected = "Select all",
                           inline = TRUE),
        )
    
      ),
    # create button for viewing results once specific data types and participants are selected
    actionButton("view", "View results", class="btn-primary"),
    #create button to clear everything and start over
    actionButton("reset", "Clear all", class="btn-outline-primary")
    
  ),
  # create panel of output information as well as supplementary instructions
  navset_card_underline(
    # panel for plot output and specific results (colour-categorization and risk level)
    nav_panel(
      "Risk level results",
      card(plotOutput("XYplot")),
      card(textOutput("text"))
    ),
    # panel for specific recommendations on storage, data sharing etc
    nav_panel(
      "Risk level recommendations",
      uiOutput("recommendations")
    ),
    # panel for instructions on how to use the tool (if needed)
    nav_panel(
      "How to use this tool",

        uiOutput("instructions")

    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  # render instructions.Rmd to md and convert to an output that can be presented in instructions panel on UI
  # this doesn't necessarily need to be done in the server unless the goal is for the md to be reactive
  # this will matter more for the recommendations panel

  
  output$instructions <- renderUI({
    includeMarkdown(render("mdFiles/instructions.Rmd", 
                           output_file = 'instructions', 
                           output_dir ='mdFiles', 
                           quiet = TRUE))
  })
  
  # recommendations test
  # test works, now need to create combo Rmd file based on results of analysis and chosen data processing activities
  
  output$recommendations <- renderUI({
    comboTest <- c(readLines("mdFiles/test1.Rmd"), readLines("mdFiles/test2.Rmd"), readLines("mdFiles/test3.Rmd"))
    writeLines(comboTest, "mdFiles/comboTest.Rmd")
    includeMarkdown(render("mdFiles/comboTest.Rmd", 
                           output_file = 'comboTest', 
                           output_dir ='mdFiles', 
                           quiet = TRUE))
  })
  
  
  # add validation for required fields (only required fields are dataType and particType)
  
  iv <- InputValidator$new()
  iv$add_rule("dataType", sv_required())
  iv$add_rule("particType", sv_required())

  
  #create a reactive Value reset$clear that can be used in observeEvents for clearing all inputs and outputs
  # will not be needed until the observeEvent actions for the view and reset buttons
  
  reset <- reactiveValues(clear = NA)
  
  choicesDat <- reactive({
    req(input$genDataType)
    combined_pattern <- paste(input$genDataType, collapse = "|")
    genDatSubset <- filter(rskDatTyp, str_detect(GenDataType, combined_pattern))
    genDatSubset$ShortDescription
  })
  
  
  observe({
      updateSelectInput(inputId = "dataType", choices = sort(choicesDat()), selected = input$dataType)
  })
  
  maxX <- eventReactive(input$view, {
    req(input$dataType)
    
    df <- rskDatTyp %>% filter(ShortDescription %in% input$dataType) 
    
    lvlCalcFx(excepToIncrFx(adjustINX(df, 
                                      typeINXfx(df), 
                                      resultINX)))
    
  })
  
  choicesPart <- reactive({
    req(input$adultChild)
    partTypes <- str_remove(input$adultChild, " participants")
    combined_pattern <- paste(partTypes, collapse = "|")
    particSubset <- filter(rskResPart, str_detect(AgeCateg, combined_pattern))
    particSubset$ShortDescription
  })
  
  observe( {
    updateSelectInput(session, inputId = "particType", choices = sort(choicesPart()), selected = input$particType)
  })
  
  maxY <- eventReactive(input$view, {
    req(input$particType)
    
    rskResPart %>% filter(ShortDescription %in% input$particType) %>%
      summarise(maxY = max(Lvl))
  })
  
  # adjust sensitLvl default input to "Yes" when maxY > 3.0
  # need to figure out how to make this change on the default, but still allow
  # user input to hold if actively changed to opposite of the default
  
  #observeEvent(input$particType, {
  #  if (maxY() > 3.0) {
   # updateSelectInput(inputId = "sensitLvl", selected = "Yes")
  #  } else {
   # updateSelectInput(inputId = "sensitLvl", selected = "No")
   #   }
  #}
 # ) 

  
  xyPair <- reactive({
    cbind(maxX(), maxY())
  })
  
  # risk categorizations
  
  sensitLvl <- reactive({
    input$sensitLvl
  })
  
  grnDatSum <- eventReactive(input$view, {
    req(input$dataType)
    
    rskDatTyp %>% filter(ShortDescription %in% input$dataType) %>%
      summarise(grnDatSum = sum(GreenData))
    
  })
  
  rskCateg <- eventReactive(input$view, {
    req(input$sensitLvl)
    
    if (maxX() >= 3.9 & maxY() >= 2.2 & sensitLvl() == "Yes") {
      return("RED")
    } 
    if (maxX() >= 3.9 & maxY() >= 2.2 & sensitLvl() == "No") {
      return("ORANGE")
    }
    if (maxX() >= 3.9 & maxY() < 2.2 & (sensitLvl() == "Yes"|sensitLvl() == "No")) {
      return("ORANGE")
    }
    if ((maxX() >= 3.0 & maxX() <= 3.8) & maxY() > 3.0 & sensitLvl() == "Yes") {
      return("RED")
    }
    if ((maxX() >= 3.0 & maxX() <= 3.8) & maxY() > 3.0 & sensitLvl() == "No") {
      return("ORANGE")
    }
    if ((maxX() >= 3.0 & maxX() <= 3.8) & (maxY() >= 2.2 & maxY() <= 3.0) & sensitLvl() == "Yes") {
      return("ORANGE")
    }
    if ((maxX() >= 3.0 & maxX() <= 3.8) & (maxY() >= 2.2 & maxY() <= 3.0) & sensitLvl() == "No") {
      return("YELLOW")
    }
    if ((maxX() >= 3.0 & maxX() <= 3.8) & maxY() < 2.2 & sensitLvl() == "No") {
      return("YELLOW")
    }
    if ((maxX() >= 3.0 & maxX() <= 3.8) & maxY() < 2.2 & sensitLvl() == "Yes") {
      return("ORANGE")
    }
    if ((maxX() >= 1.8 & maxX() < 3.0) & maxY() > 3.0 & sensitLvl() == "Yes") {
      return("ORANGE")
    }
    if ((maxX() >= 1.8 & maxX() < 3.0) & maxY() > 3.0 & sensitLvl() == "No") {
      return("YELLOW")
    }
    if ((maxX() >= 1.8 & maxX() < 3.0) & (maxY() >= 2.2 & maxY() <= 3.0) & (sensitLvl() == "Yes"|sensitLvl() == "No")) {
      return("YELLOW")
    }
    if (maxX() < 1.8 & grnDatSum() > 0 & maxY() < 2.2 & (sensitLvl() == "Yes"|sensitLvl() == "No")) {
      return("GREEN")
    }
    if (maxX() < 1.8 & grnDatSum() > 0 & maxY() >= 2.2 & sensitLvl() == "Yes") {
      return("YELLOW")
    }
    if (maxX() < 1.8 & grnDatSum() > 0 & maxY() >= 2.2 & sensitLvl() == "No") {
      return("GREEN")
    }
    if (maxX() < 1.8 & grnDatSum() == 0 & maxY() < 2.2 & (sensitLvl() == "Yes"|sensitLvl() == "No")) {
      return("BLUE")
    }
    if (maxX() < 1.8 & grnDatSum() == 0 & maxY() >= 2.2 & sensitLvl() == "Yes") {
      return("GREEN")
    }
    if (maxX() < 1.8 & grnDatSum() == 0 & maxY() >= 2.2 & sensitLvl() == "No") {
      return("BLUE")
    }

  } 
  )
  
  
  observeEvent(input$view, {
    # when the action button view is pressed the event reactives to produce maxX and maxY occur
    # input validation for these fields is only enable upon clicking view
    iv$enable()
    # this action also causes the reactive Value reset$clear to become 1 (and therefore not NA)
    reset$clear <- 1
  })
  
  observeEvent(input$dataClear, {
    # clear data inputs upon click
    updateSelectInput(inputId = "genDataType", selected=character(0))
    updateSelectInput(inputId = "dataType", selected=character(0))
  }
  )
  
  observeEvent(input$particClear, {
    # clear participant inputs upon click
    updateSelectInput(inputId = "adultChild", selected=character(0))
    updateSelectInput(inputId = "particType", selected=character(0))
  }
  )
  
  observeEvent(input$reset, {
    #when actionButton reset is clicked all inputs are cleared (except sensitLvl & datActiv return to default) and reactive value reset$clear is
    # returned to NA
    updateSelectInput(inputId = "genDataType", selected=character(0))
    updateSelectInput(inputId = "dataType", selected=character(0))
    updateSelectInput(inputId = "adultChild", selected=character(0))
    updateSelectInput(inputId = "particType", selected=character(0))
    updateSelectInput(inputId = "sensitLvl", selected="No")
    updateSelectInput(inputId = "datActiv", selected="Select all")
    reset$clear <- NA
    
  }
  ) 
  
  output$XYplot <- renderPlot({
    # if reactive value reset$clear is NA (only upon initial session or after action button reset is clicked)
    # then no output will be returned. If anything else, the plot will be returned
    if (is.na(reset$clear)) return()
    
    ggplot(xyPair(), aes(x = maxX, y = maxY)) +
      geom_point(size = 10) +
      scale_x_continuous(name = "Re-identifiability risk", 
                         breaks = c(1:5),
                         labels = c("1" = "Low risk", "2" = "", "3" = "Moderate risk", "4" = "", "5" = "High risk"),
                         limits = c(1, 5)) +
      scale_y_continuous(name = "Participant vulnerability risk", 
                         breaks = c(1, 1.5, 2, 2.25, 2.5, 3, 3.5),
                         labels = c("1.0" = "Low risk", "1.5" = "", "2.0" = "", "2.25" = "Moderate risk", "2.5" = "", "3.0" = "", "3.5" = "High risk"),
                         limits = c(1, 3.5)) +
      theme(axis.text.x=element_text(size=15),
            axis.text.y=element_text(size=15, angle = 90, hjust = 0.6),
            #axis.ticks = element_blank(),
            axis.title=element_text(size=24,face="bold")
            )
    
  })
  
  
 
  
  
  output$text <- renderText({
    # if reactive value reset$clear is NA (only upon initial session or after action button reset is clicked)
    # then no output will be returned. If anything else, the plot will be returned
    if (is.na(reset$clear)) return()
    
    paste("Data risk level is", maxX(), 
          "and participant vulnerability level is", maxY(), 
          "Risk category is", rskCateg(),
          "And presence of green data is", grnDatSum())
  }
  
  )
  
  
  

  
  

  
}

# Run the app ----
shinyApp(ui = ui, server = server)