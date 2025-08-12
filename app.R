#
# This is a Shiny web application for estimating privacy risks posed 
# by common FGB data types and research populations
#

library(shiny)
library(shinyvalidate)
library(shinyjs)
library(bslib)
library(tidyverse)
library(shinycssloaders)
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
  useShinyjs(),
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
      # adjust padding for (de)select all action button 'selAllGenDat' and 'deselAllGenDat'
      tags$style("#selAllGenDat.btn-outline-primary, #deselAllGenDat.btn-outline-primary {
        margin-bottom: 15%;
        }"
      ),
      # adjust width for action button swtchReuseYel
      tags$style("#swtchReuseYel.btn-outline-primary {
        width: 10%;
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
                           choices = c(sort(c("Administrative data", "Location data", 
                                       "Audiovisual data", "Textual data", "Experimental/lab data",
                                       "Neuroimaging data", "Questionnaire data", "Sociodemographic data", "Economic/political data",
                                       "Educational data", "Physical characteristics/medical data", "Biological/genetic data"))), inline = TRUE),
        # create button to select all general data types
        actionButton("selAllGenDat", "Select all", class="btn-outline-primary"),
        # create button to deselect all data processing activities
        shinyjs::hidden(actionButton("deselAllGenDat", "Deselect all", class="btn-outline-primary")),
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
                           # use choiceNames and choiceValues here to make
                           # selection of processing activities in server easier
                           choiceNames = list("Storage during current project", 
                                              "Sharing during current project",
                                              "Use by students during current project", 
                                              "Archiving & publiashing after current project", 
                                              "Reuse after current project"),
                           choiceValues = list("storage",
                                               "transfer",
                                               "students",
                                               "archiving",
                                               "reuse"),
                           selected = c("storage",
                                        "transfer",
                                        "students",
                                        "archiving",
                                        "reuse"),
                           inline = TRUE),
          # create button to select all data processing activities
          actionButton("deselAllActiv", "Deseelect all", class="btn-outline-primary"),
          # create button to deselect all data processing activities
          shinyjs::hidden(actionButton("selAllActiv", "Select all", class="btn-outline-primary"))
        )
    
      ),
    # create button for viewing results once specific data types and participants are selected
    actionButton("view", "View results", class="btn-primary"),
    #create button to clear everything and start over
    actionButton("reset", "Reset", class="btn-outline-primary")
    
  ),
  # create panel of output information as well as supplementary instructions
  navset_card_underline(
    # panel for plot output and specific results (colour-categorization and risk level)
    nav_panel(
      "Risk level results",
      card(plotOutput("XYplot", height = "70%") ),
      card(textOutput("text") %>% withSpinner(type = getOption("spinner.type", default = 5),
                                              color = getOption("spinner.color", default = "#2da4e7")))
    ),
    # panel for specific recommendations on storage, data sharing etc
    nav_panel(
      "Risk level recommendations",
      uiOutput("recommendations") %>% withSpinner(type = getOption("spinner.type", default = 5),
                                                  color = getOption("spinner.color", default = "#2da4e7")),
     # shinyjs::hidden(actionButton("swtchReuseYel", "Switch to yellow data guidance", class="btn-outline-primary")) # not sure if will include this button and action; may add back later
    ),
    # panel for instructions on how to use the tool (if needed)
    nav_panel(
      "How to use this tool",

        uiOutput("instructions") %>% withSpinner(type = getOption("spinner.type", default = 5),
                                                 color = getOption("spinner.color", default = "#2da4e7"))

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
  
  maxX <- reactive({
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
  
  maxY <- reactive({
    req(input$particType)
    
    rskResPart %>% filter(ShortDescription %in% input$particType) %>%
      summarise(maxY = max(Lvl))
  })
  
  # auto-adjust sensitLvl default input to "Yes" when maxY > 3.0
  # user can still reset back to "No" if desired

  
  observeEvent(input$particType, {
    if (maxY() > 3.0) {
    updateSelectInput(inputId = "sensitLvl", selected = "Yes")
    } else {
    updateSelectInput(inputId = "sensitLvl", selected = "No")
      }
  }
  ) 

  # create a basic XY pair of maxX and maxY to plot with ggplot
  
  xyPair <- reactive({
    cbind(maxX(), maxY())
  })
  
  # define colour coded risk categorizations
  
  sensitLvl <- reactive({
    req(input$sensitLvl)
    
    input$sensitLvl
  })
  
  # create a reactive that detects if green data are present
  # (if low enough risk, but green data present, will be assigned
  # green instead of blue)
  
  grnDatSum <- reactive({
    req(input$dataType)
    
    rskDatTyp %>% filter(ShortDescription %in% input$dataType) %>%
      summarise(grnDatSum = sum(GreenData))
    
  })
  
  # determine color of risk category
  
  rskCateg <- reactive({
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
    if ((maxX() >= 1.8 & maxX() < 3.0) & maxY() <= 3.0 & (sensitLvl() == "Yes"|sensitLvl() == "No")) {
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
  
  # create reactive based on color category to plug into plot output
  
  colorCat <- reactive({
    if (rskCateg() == "RED") {
      return("#ed0000")
    }
    if (rskCateg() == "ORANGE") {
      return("#ff9000")
    }
    if (rskCateg() == "YELLOW") {
      return("#ffDf00")
    }
    if (rskCateg() == "GREEN") {
      return("#379e00")
    }
    if (rskCateg() == "BLUE") {
      return("#6e99c4")
    }
  })

  
  
  observeEvent(input$selAllGenDat, {
    # select all general data types
    updateSelectInput(inputId = "genDataType", selected=c("Administrative data", "Location data", 
                                                          "Audiovisual data", "Textual data", "Experimental/lab data",
                                                          "Neuroimaging data", "Questionnaire data", "Sociodemographic data", 
                                                          "Economic/political data","Educational data", 
                                                          "Physical characteristics/medical data", "Biological/genetic data"))
    shinyjs::hide(id = "selAllGenDat")
    shinyjs::show(id = "deselAllGenDat")
    }
  )
  
  observeEvent(input$deselAllGenDat, {
    # deselect all data processing activities
    updateSelectInput(inputId = "genDataType", selected=character(0))
    shinyjs::hide(id = "deselAllGenDat")
    shinyjs::show(id = "selAllGenDat")
  }
  )
  
  observeEvent(input$deselAllActiv, {
    # deselect all data processing activities
    updateSelectInput(inputId = "datActiv", selected=character(0))
    shinyjs::hide(id = "deselAllActiv")
    shinyjs::show(id = "selAllActiv")
  }
  )
  
  observeEvent(input$selAllActiv, {
    # select all data processing activities
    updateSelectInput(inputId = "datActiv", selected=c("storage","transfer","students","archiving","reuse"))
    shinyjs::hide(id = "selAllActiv")
    shinyjs::show(id = "deselAllActiv")
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
    updateSelectInput(inputId = "datActiv", selected=c("storage","transfer","students","archiving","reuse"))
    reset$clear <- NA
    
  }
  ) 
  
  output$XYplot <- renderPlot({
    # if reactive value reset$clear is NA (only upon initial session or after action button reset is clicked)
    # then no output will be returned. If anything else, the plot will be returned
    if (is.na(reset$clear)) return()
    
    ggplot(xyPair(), aes(x = maxX, y = maxY)) +
      geom_point(size = 15, color = "black", fill = colorCat(), shape = 21) +
      scale_x_continuous(name = "Re-identifiability risk", 
                         breaks = c(1,3,5),
                         labels = c("1" = "Low risk", "3" = "Moderate risk",  "5" = "High risk"),
                         limits = c(1, 5)) +
      scale_y_continuous(name = "Participant vulnerability risk", 
                         breaks = c(1,  2.25,  3.5),
                         labels = c("1.0" = "Low risk",  "2.25" = "Moderate risk",  "3.5" = "High risk"),
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
  
  # create a reactive that identifies which data processing activities were chosen
  
  datProc <- eventReactive(input$view, {
    req(input$datActiv)
    
    datProc <- input$datActiv
    
  })
  
  # recommendations text

  
  output$recommendations <- renderUI({

      combineMD <- c(readLines("mdFiles/introRec.Rmd"), readLines("mdFiles/genRec.Rmd")) #read in Rmd that provides the yaml and css at very top & Rmd that gives general guidance
      for(i in 1:length(datProc())) {
        mdFile <- paste0("mdFiles/",datProc()[i],rskCateg(), ".Rmd")
        combineMD <- c(combineMD, readLines(mdFile))
      }

    
    
  
    writeLines(combineMD, "mdFiles/combinedRec.Rmd")
    includeMarkdown(render("mdFiles/combinedRec.Rmd", 
                           output_file = 'combinedRec', 
                           output_dir ='mdFiles', 
                           quiet = TRUE))
  
    })
  
  # allow to switch from green reuse advice to yellow
  # show button to allow switch
  
  # not sure if I want to do the following
  # added complexity, may add back later if desired
  
  #observe({
  #  if (rskCateg() == "GREEN" & "reuse" %in% datProc()) {
  #    shinyjs::show(id = "swtchReuseYel")
  #  }
  #}
 # )
  
  #observeEvent(input$swtchReuseYel, {
   # output$recommendations <- renderUI({
      
   #   newLength <- length(datProc()) - 1
    
    #  combineMD <- c(readLines("mdFiles/introRec.Rmd"), readLines("mdFiles/genRec.Rmd")) #read in Rmd that provides the yaml and css at very top & Rmd that gives general guidance
    #  for(i in 1:newLength) {
     #  mdFile <- paste0("mdFiles/",datProc()[i],rskCateg(), ".Rmd")
     #   combineMD <- c(combineMD, readLines(mdFile))
    #  }
    #  combineMD <- c(combineMD, readLines("mdFiles/reuseYELLOW.Rmd"))
    
    
    
    #  writeLines(combineMD, "mdFiles/combinedRec.Rmd")
    #  includeMarkdown(render("mdFiles/combinedRec.Rmd", 
    #                       output_file = 'combinedRec', 
    #                       output_dir ='mdFiles', 
     #                      quiet = TRUE))
    
    
   # }
    
   # )
 # }
  #)

  
}

# Run the app ----
shinyApp(ui = ui, server = server)