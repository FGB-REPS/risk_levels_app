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
library(kableExtra)



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
#  > have an on-off button for "sensitive research topics / elevated risk of harm"

# - design plot output that places result at the correct X,Y coordinate based on 
# the re-identification risk posed by data type (i.e. X), as well as any interacting factors
# that arise from combinations of data types, and the vulnerability risk posed
# by the research subjects (i.e. Y), with option to toggle risk of harm on and off
# resulting in different risk colour categories that show up on the graph

# UI should also
# - have space for short text that states the resulting colour code
# - have option of tooltips or other form of "additional info" on answer options as needed
# - further additions to UI will come in future versions



# Define UI ----
ui <- page_sidebar(
  useShinyjs(),
  theme = bs_theme(bootswatch = "cerulean"),
  #  custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")  
  ),
  title = "Data Risk Classification",
  sidebar = sidebar(
    accordion(
      open = c("Data types", "Participant types", "Risk of harm"), # set first panel to open upon load
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
        actionButton("dataClear", "Clear inputs", class="btn btn-outline-primary")
        
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
        actionButton("particClear", "Clear inputs", class="btn btn-outline-primary")
        ),
        accordion_panel(
          "Risk of harm",
          radioButtons("harmLvl",
                   label = span("Does the information contained in the data pose additional risks of harm to the participants?",
                                span(
                                  tooltip(
                                    bsicons::bs_icon("info-circle"),
                                    "Select 'yes' if the risk of harm to the research subjects, should the information be leaked,
                                    is elevated by the content of the data, in combination with and/or beyond what
                                    is already known about the research subjects' vulnerability"
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
                                              "Archiving & publishing after current project", 
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
          actionButton("deselAllActiv", "Deseelect all", class="btn btn-outline-primary"),
          # create button to deselect all data processing activities
          shinyjs::hidden(actionButton("selAllActiv", "Select all", class="btn btn-outline-primary"))
        )
    
      ),
    # create button for viewing results once specific data types and participants are selected
    actionButton("view", "View results", class="btn btn-primary"),
    #create button to clear everything and start over
    actionButton("reset", "Reset", class="btn btn-outline-primary")
    
  ),
  # create panel of output information as well as supplementary instructions
  navset_card_underline(
    # panel for plot output and specific results (colour-categorization and risk level)
    nav_panel(
      "Risk level results",
      card(plotOutput("XYplot", height = "800px") ),
      card(uiOutput("textResults") %>% withSpinner(type = getOption("spinner.type", default = 5),
                                              color = getOption("spinner.color", default = "#2da4e7")))
    ),
    # panel for specific recommendations on storage, data sharing etc
    nav_panel(
      "Risk level recommendations",
      uiOutput("recommendations") %>% withSpinner(type = getOption("spinner.type", default = 5),
                                                  color = getOption("spinner.color", default = "#2da4e7")),
      shinyjs::hidden(actionButton("shwYelReus", "Show reuse of yellow data", class="btn-outline-primary"))
    ),
    # panel for instructions on how to use the tool (if needed)
    nav_panel(
      "How to use this tool",

        uiOutput("instructions") %>% withSpinner(type = getOption("spinner.type", default = 5),
                                                 color = getOption("spinner.color", default = "#2da4e7"))

    ),
    # panel for instructions on how to use the tool (if needed)
    nav_panel(
      "Additional guidance",
      uiOutput("datTypTab")  %>% withSpinner(type = getOption("spinner.type", default = 5),
                                                    color = getOption("spinner.color", default = "#2da4e7"))
      
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {

  
  # create reactive from genDataType that alters which choices are shown in dataType input section
  
  choicesDat <- reactive({
    req(input$genDataType)
    combined_pattern <- paste(input$genDataType, collapse = "|")
    genDatSubset <- filter(rskDatTyp, str_detect(GenDataType, combined_pattern))
    genDatSubset$ShortDescription
  })
  

  # use reactive to update choices available in dataType
  
  observe({
      inptGenDat <- input$genDataType
      if (!is.null(inptGenDat)) {
        updateSelectInput(inputId = "dataType", choices = sort(choicesDat()), selected = input$dataType)
      } else {
        updateSelectInput(inputId = "dataType", choices = sort(rskDatTyp$ShortDescription), selected = input$dataType)
      }
  })
  
  # calculate the maximum value for the x-axis (re-identification risk)
  # create reactive maxX to represent this max value
  # reactive below uses function from helper_fxs.R script
  
  maxX <- reactive({
    req(input$dataType)
    
    df <- rskDatTyp %>% filter(ShortDescription %in% input$dataType) 
    
    lvlCalcFx(excepToIncrFx(adjustINX(df, 
                                      typeINXfx(df), 
                                      resultINX)))
    
  })
  
  # create reactive from adultChile that alters which choices are shown in particType input section
  
  choicesPart <- reactive({
    req(input$adultChild)
    partTypes <- str_remove(input$adultChild, " participants")
    combined_pattern <- paste(partTypes, collapse = "|")
    particSubset <- filter(rskResPart, str_detect(AgeCateg, combined_pattern))
    particSubset$ShortDescription
  })
  
  # use reactive to update choices available in particType
  
  observe({
    inptPartTyp <- input$adultChild
    
    if(!is.null(inptPartTyp)) {
      updateSelectInput(session, inputId = "particType", choices = sort(choicesPart()), selected = input$particType)
    } else {
      updateSelectInput(session, inputId = "particType", choices = sort(rskResPart$ShortDescription), selected = input$particType)
    }
  })
  
  # calculate the maximum value for the y-axis (vulnerability risk)
  # create reactive maxY to represent this max value
  
  maxY <- reactive({
    req(input$particType)
    
    rskResPart %>% filter(ShortDescription %in% input$particType) %>%
      summarise(maxY = max(Lvl))
  })
  
  # auto-adjust harmLvl from default input of "No" to "Yes" when maxY > 3.0
  # user can still reset back to "No" if desired
  
  observeEvent(input$particType, {
    if (maxY() > 3.0) {
    updateSelectInput(inputId = "harmLvl", selected = "Yes")
    } else {
    updateSelectInput(inputId = "harmLvl", selected = "No")
      }
  }
  ) 

  # create a basic XY pair reactive from maxX and maxY reactives to use for output plot with ggplot
  
  xyPair <- reactive({
    cbind(maxX(), maxY())
  })
  
  # capture the input from harmLvl as reactive harmLvl() so that it can be used in determining risk level categorizations
  
  harmLvl <- reactive({
    req(input$harmLvl)
    
    input$harmLvl
  })
  

  # create reactive grnDatSum() that detects if green data are present
  # if risks in xyPair are low, but green data is present, rskCateg will be assigned
  # as green instead of blue
  
  grnDatSum <- reactive({
    req(input$dataType)
    
    rskDatTyp %>% filter(ShortDescription %in% input$dataType) %>%
      summarise(grnDatSum = sum(GreenData))
    
  })
  
  # determine color of risk category as reactive rskCateg()
  
  rskCateg <- reactive({
    
    if (maxX() >= 3.9 & maxY() >= 2.2 & harmLvl() == "Yes") {
      return("RED")
    } 
    if (maxX() >= 3.9 & maxY() >= 2.2 & harmLvl() == "No") {
      return("ORANGE")
    }
    if (maxX() >= 3.9 & maxY() < 2.2 & (harmLvl() == "Yes"|harmLvl() == "No")) {
      return("ORANGE")
    }
    if ((maxX() >= 3.0 & maxX() <= 3.8) & maxY() > 3.0 & harmLvl() == "Yes") {
      return("RED")
    }
    if ((maxX() >= 3.0 & maxX() <= 3.8) & maxY() > 3.0 & harmLvl() == "No") {
      return("ORANGE")
    }
    if ((maxX() >= 3.0 & maxX() <= 3.8) & (maxY() >= 2.2 & maxY() <= 3.0) & harmLvl() == "Yes") {
      return("ORANGE")
    }
    if ((maxX() >= 3.0 & maxX() <= 3.8) & (maxY() >= 2.2 & maxY() <= 3.0) & harmLvl() == "No") {
      return("YELLOW")
    }
    if ((maxX() >= 3.0 & maxX() <= 3.8) & maxY() < 2.2 & harmLvl() == "No") {
      return("YELLOW")
    }
    if ((maxX() >= 3.0 & maxX() <= 3.8) & maxY() < 2.2 & harmLvl() == "Yes") {
      return("ORANGE")
    }
    if ((maxX() >= 1.8 & maxX() < 3.0) & maxY() > 3.0 & harmLvl() == "Yes") {
      return("ORANGE")
    }
    if ((maxX() >= 1.8 & maxX() < 3.0) & maxY() > 3.0 & harmLvl() == "No") {
      return("YELLOW")
    }
    if ((maxX() >= 1.8 & maxX() < 3.0) & maxY() <= 3.0 & (harmLvl() == "Yes"|harmLvl() == "No")) {
      return("YELLOW")
    }
    if (maxX() < 1.8 & grnDatSum() > 0 & maxY() < 2.2 & (harmLvl() == "Yes"|harmLvl() == "No")) {
      return("GREEN")
    }
    if (maxX() < 1.8 & grnDatSum() > 0 & maxY() >= 2.2 & harmLvl() == "Yes") {
      return("YELLOW")
    }
    if (maxX() < 1.8 & grnDatSum() > 0 & maxY() >= 2.2 & harmLvl() == "No") {
      return("GREEN")
    }
    if (maxX() < 1.8 & grnDatSum() == 0 & maxY() < 2.2 & (harmLvl() == "Yes"|harmLvl() == "No")) {
      return("BLUE")
    }
    if (maxX() < 1.8 & grnDatSum() == 0 & maxY() >= 2.2 & harmLvl() == "Yes") {
      return("GREEN")
    }
    if (maxX() < 1.8 & grnDatSum() == 0 & maxY() >= 2.2 & harmLvl() == "No") {
      return("BLUE")
    }

  } 
  )
  
  # create reactive based on color category called colorCat() to use in plot output
  
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

  
  ### ACTION BUTTONS
  
  # run Event that selects all general data types (for input field genDataType) if action button selAllGenDat is clicked
  
  observeEvent(input$selAllGenDat, {
    # select all general data types
    updateSelectInput(inputId = "genDataType", selected=c("Administrative data", "Location data", 
                                                          "Audiovisual data", "Textual data", "Experimental/lab data",
                                                          "Neuroimaging data", "Questionnaire data", "Sociodemographic data", 
                                                          "Economic/political data","Educational data", 
                                                          "Physical characteristics/medical data", "Biological/genetic data"))
    shinyjs::hide(id = "selAllGenDat") # hide selAllGenDat action button
    shinyjs::show(id = "deselAllGenDat") # reveal deselAllGenDat action button
    }
  )
  
  # run Event that deselects all general data types (for input field genDataType) if action button deselAllGenDat is clicked
  
  observeEvent(input$deselAllGenDat, {
    # deselect all data processing activities
    updateSelectInput(inputId = "genDataType", selected=character(0))
    shinyjs::hide(id = "deselAllGenDat") # hide deselAllGenDat action button
    shinyjs::show(id = "selAllGenDat") # reveal selAllGenDat action button
  }
  )
  
  # run event that deselects all data processing activities (from input field datActiv) if action button deselAllActiv is clicked
  # default is set to all activities being selected on load
  
  observeEvent(input$deselAllActiv, {
    # deselect all data processing activities
    updateSelectInput(inputId = "datActiv", selected=character(0))
    shinyjs::hide(id = "deselAllActiv") # hide deselAllActiv action button
    shinyjs::show(id = "selAllActiv") # reveal selAllActiv action button
  }
  )
  
  # run event that selects all data processing activities (from input field datActiv) if action button selAllActiv is clicked
  
  observeEvent(input$selAllActiv, {
    # select all data processing activities
    updateSelectInput(inputId = "datActiv", selected=c("storage","transfer","students","archiving","reuse"))
    shinyjs::hide(id = "selAllActiv") # hide selAllActiv action button
    shinyjs::show(id = "deselAllActiv") # reveal deselAllActiv action button
  }
  )
  
  # add validation for required fields (only required fields are dataType and particType; harmLvl is always either yes or no)
  
  iv <- InputValidator$new()
  iv$add_rule("dataType", sv_required())
  iv$add_rule("particType", sv_required())
  
  
  #create a reactive Value reset$clear that can be used in observeEvents for clearing all inputs and outputs
  # will not be needed until the observeEvent actions for the view and reset buttons
  
  reset <- reactiveValues(clear = NA)
  
  # create event for action button "view" so that input validation for dataType and particType is activated
  
  observeEvent(input$view, {
    # input validation for these fields is only enabled upon clicking view
    iv$enable()
    # this action also causes the reactive Value reset$clear to become 1 (and therefore not NA)
    reset$clear <- 1
  })
  
  # clear data inputs upon click on action button dataClear
  
  observeEvent(input$dataClear, {
    updateSelectInput(inputId = "genDataType", selected=character(0))
    updateSelectInput(inputId = "dataType", selected=character(0))
  }
  )
  
  # clear participant inputs upon click on action. button particClear
  
  observeEvent(input$particClear, {
    updateSelectInput(inputId = "adultChild", selected=character(0))
    updateSelectInput(inputId = "particType", selected=character(0))
  }
  )
  
  
  # reset all values when actionButton reset is clicked; all inputs are cleared (except harmLvl & datActiv which return to default) 
  # and reactive value reset$clear is
  # returned to NA
  
  observeEvent(input$reset, {
    updateSelectInput(inputId = "genDataType", selected=character(0))
    updateSelectInput(inputId = "dataType", selected=character(0))
    updateSelectInput(inputId = "adultChild", selected=character(0))
    updateSelectInput(inputId = "particType", selected=character(0))
    updateSelectInput(inputId = "harmLvl", selected="No")
    updateSelectInput(inputId = "datActiv", selected=c("storage","transfer","students","archiving","reuse"))
    reset$clear <- NA
    
  }
  ) 
  
  
  #### OUTPUTS
  
  # plot output of overall results
  
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
  
  
 # render basic text explaining risk category assigned to user
  
  
  output$textResults <- renderUI({
    # if reactive value reset$clear is NA (only upon initial session or after action button reset is clicked)
    # then no output will be returned. If anything else, the plot will be returned
    if (is.na(reset$clear)) return()
    
    if (harmLvl() == "Yes") {
      harmTxt <- "are"
    } else {
      harmTxt <- "are no"
    }
    
    HTML(paste0("Your data risk classification is ", strong(rskCateg()), 
          ". You have indicated that there ",  strong(harmTxt), " increased risks of
          harm to your participants."))
  }
  
  )
  
  
  # make output for recommendations text on the second tab of UI
  
  # first create a reactive that identifies which data processing activities were chosen by user
  
  datProc <- reactive({
    req(input$datActiv)
    
    datProc <- input$datActiv
    
  })
  
  # set up action button shwYelReus that only appears if rskCateg is GREEN and datProc is "reuse"
  
  # first create reactive value show$yellow that will always be NA except for the conditions when
  # we want the action button shwYelReus to be shown
  
  show <- reactiveValues(yellow = NA)
  
  # allow shwYelReus action button to be seen
  
  observe({
    if (rskCateg()=="GREEN" & "reuse" %in% datProc() & is.na(show$yellow)) {
      shinyjs::show(id = "shwYelReus")
    } 
  }
  )
  
  # on click of shwYelReus show$yellow reactive value becomes 1 which will impact the chosen markdown
  # outputs in the following renderUI section
  
  observeEvent(input$shwYelReus, {
    
    show$yellow <- 1
    shinyjs::hide(id = "shwYelReus")
    
  })
  
  # return show$yellow to NA and hide action button shwYelReus again if reuse is deselected or data is no longer GREEN
  
  observe({
    if (rskCateg()!="GREEN" | !("reuse" %in% datProc())) {
      show$yellow <- NA
      shinyjs::hide(id = "shwYelReus")
    } 
  }
  )
  
  # based on datProc as well as rskCateg calculations, concat recommendations text and render into UI
  # if the action button shwYelReus has been clicked and the rskCateg is GREEN the first action will carry out
  # and include the yellow reuse instructions. For all other cases, the recommendations will simply be
  # combined based on rskCateg color and which datProc activities are currently selected by the user
  
  combineMD <- reactive({
    if (!is.na(show$yellow)) {
      combineMD <- c(readLines("mdFiles/genRec.Rmd")) #read in Rmd that provides the yaml and css at very top & Rmd that gives general guidance
      for(i in 1:length(datProc())) {
        mdFile <- paste0("mdFiles/",datProc()[i],rskCateg(), ".Rmd")
        combineMD <- c(combineMD, readLines(mdFile))
      }
      combineMD <- c(combineMD, readLines("mdFiles/reuseYELLOW.Rmd"))
      } else {
      combineMD <- c(readLines("mdFiles/genRec.Rmd")) #read in Rmd that provides the yaml and css at very top & Rmd that gives general guidance
      for(i in 1:length(datProc())) {
        mdFile <- paste0("mdFiles/",datProc()[i],rskCateg(), ".Rmd")
        combineMD <- c(combineMD, readLines(mdFile))
      }
    }
    
    writeLines(combineMD, "mdFiles/combinedRec.Rmd")
    
    # originally rendered Rmd to markdown but changed to rendering to html below
    # just need to ensure that markdownToHTML command includes template = FALSE to not
    # mess up formatting
    
    #includeMarkdown(render("mdFiles/combinedRec.Rmd", 
     #                      output_file = 'combinedRec', 
      #                     output_dir ='mdFiles', 
       #                    quiet = TRUE))
    
    HTML(markdown::markdownToHTML(knit('mdFiles/combinedRec.Rmd', 
                                       output ='mdFiles/combinedRec.html',
                                       quiet = TRUE),
                                      template = FALSE ))
    
  })
  
  output$recommendations <- renderUI({
    # if reactive value reset$clear is NA (only upon initial session or after action button reset is clicked)
    # then no output will be returned. If anything else, the plot will be returned
    if (is.na(reset$clear)) return()
    
    combineMD()
  
    })
  

  
  # render instructions.Rmd to md and convert to an output that can be presented in the "How to use this tool" tab of the UI
  
  output$instructions <- renderUI({
    
    # originally rendered Rmd to markdown but changed to rendering to html below
    # just need to ensure that markdownToHTML command includes template = FALSE to not
    # mess up formatting
    
    #includeMarkdown(render("mdFiles/instructions.Rmd", 
    #           output_file = 'instructions', 
    #         output_dir ='mdFiles', 
    #         quiet = TRUE))
    
    HTML(markdown::markdownToHTML(knit('mdFiles/instructions.Rmd', 
                                       output ='mdFiles/instructions.html',
                                       quiet = TRUE),
                                      template = FALSE ))
    
  })
  
  
  # include additional guidance on the various data types and also further info on the privacy risk categorizations and examples
  
  output$datTypTab <- renderUI({
      HTML(markdown::mark_html(knit('mdFiles/addGuidance.Rmd', 
                                    output ='mdFiles/addGuidance.html',
                                    quiet = TRUE), 
                                    template = FALSE)) 
    # note: template FALSE needs to be included in markdownToHTML to prevent
    # the knitted html doc from altering the format and style of the shiny app when rendered
    }
  )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)