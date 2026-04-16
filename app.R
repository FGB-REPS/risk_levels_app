# ==============================================================================
# Project: Risk Level Categorization App
# Author(s): Jessica Hrudey (e.j.hrudey@vu.nl)
# Description: This is a Shiny web application for estimating privacy risks posed 
# by common FGB data types and research populations
# ==============================================================================



if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "shiny",
  "shinyvalidate",
  "shinyjs",
  "shinyStorePlus",
  "bslib",
  "dplyr",
  "stringr",
  "ggplot2",
  "shinycssloaders",
  "htmltools",
  "markdown",
  "rmarkdown",
  "knitr",
  "kableExtra"
)

#---- 0.0: set and source parameters ----
## ---- 0.1: Set filepaths----

# Running in container
if(!grepl("Users", getwd())){
  setwd("/home/shiny/app")
  # Specify the application port
  options(shiny.host = "127.0.0.1")
  options(shiny.port = 7950)
  
  # source helper functions
  
  source("helper_fxs.R")
  
  #load data 
  
  load("data/rskData.RData")
  
  # Running locally
} else {
  #load data 
  
  load("data/rskData.RData")
  
  # source helper functions
  
  source("helper_fxs.R")
}




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
  # Initialize shinyStorePlus
  initStore(),
  theme = bs_theme(bootswatch = "cerulean"),
  #  custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")  
  ),
  includeCSS("www/styles.css"),
  title = "Data Risk Classification",
  sidebar = sidebar(
    accordion(
      open = c("Data types", "Participant types", "Risk of harm", "Additional confidentiality concerns"), # set which panels are open upon load
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
        selectizeInput("dataType", 
                    label = span("Select specific type(s) of data",
                                 span(
                                   tooltip(
                                     bsicons::bs_icon("info-circle"),
                                     "You can type in the field below to search for a data type. See the 'How to use this tool' tab for more information on choosing which data types to include and the 'Additional Guidance' tab for more information on the types of data"
                                    )
                                  )
                                 ),
                    choices = sort(rskDatTyp$ShortDescription), 
                    multiple = TRUE, 
                    #selectize = TRUE,
                    options = list(
                      plugins = list("remove_button"),
                      create = FALSE, # don't allow creation of new data type entries by user
                      persist = FALSE # don't allow creation of new data type entries by user
                    )),
        #create button to clear data type inputs
        actionButton("dataClear", "Clear inputs", class="btn btn-outline-primary")
        
      ),
      accordion_panel(
        "Participant types",
        # select inputs for whether participants are adults or children
        checkboxGroupInput("adultChild", 
                         label = span("Select research subject age group (optional)",
                                      span(
                                        tooltip(
                                          bsicons::bs_icon("info-circle"),
                                          "You may select the age grouping of the research subjects in order to 
                                          filter the choices shown when selecting the specific type(s)"
                                        )
                                      )),
                         choices = c("Adult participants", "Children participants"),
                         inline = TRUE),
        # select inputs for vulnerability of participants on y-axis
        selectizeInput("particType", 
                  label = span("Select type(s) of research subjects",
                               span(
                                 tooltip(
                                   bsicons::bs_icon("info-circle"),
                                   "You can type in the field below to search for a specific research subject type. 
                                   See the 'How to use this tool' tab for more information on research subject types."
                                 )
                               )),
                  choices = sort(rskResPart$ShortDescription), 
                  multiple = TRUE, 
                  #selectize = TRUE,
                  options = list(
                    plugins = list("remove_button"),
                    create = FALSE, # don't allow creation of new data type entries by user
                    persist = FALSE # don't allow creation of new data type entries by user
                  )),
        #create button to clear participant type inputs
        actionButton("particClear", "Clear inputs", class="btn btn-outline-primary")
        ),
        accordion_panel(
          "Risk of harm",
          radioButtons("harmLvl",
                   label = span("Does the information contained in the data pose additional risks of harm to the research subjects?",
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
          "Additional confidentiality concerns",
          radioButtons("confidCommVal",
                     label = span("Is the data commercially valuable?",
                                  span(
                                    tooltip(
                                      bsicons::bs_icon("info-circle"),
                                      "Select 'yes' if the data has a clear potential for commercial purposes in the future
                                      or if the data is sourced from an organization that is using the data for commercial purposes"
                                    )
                                  )
                                  ),
                     choices= c("No", "Yes"),
                     selected = "No"
          ),
          radioButtons("confidOblig",
                       label = span("Is the data subject to confidentiality obligations set forth in any collaboration or data transfer agreements?",
                                    span(
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "Select 'yes' if there are contracts or agreements between yourself and other parties 
                                        that require that the data be kept confidential"
                                      )
                                    )
                                    ),
                       choices= c("No", "Yes"),
                       selected = "No"
          ),
          radioButtons("confidKnowlSec",
                       label = span("Does the data contain unique knowledge, methods or algorithms?",
                                    span(
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "Select 'yes' if this unique information is proprietary, patented or subject to
                                        any other intellectual property rules"
                                      )
                                    )
                                    ),
                       choices= c("No", "Yes"),
                       selected = "No"
          ),
          radioButtons("confidIP",
                       label = span("Is the data or information therein patentable or related to an unpublished innovation?",
                                    span(
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "Select 'yes' if the data/information have a clear potential to be patented or to be
                                        developed further into a unique innovation"
                                      )
                                    )
                                    ),
                       choices= c("No", "Yes"),
                       selected = "No"
          ),
          radioButtons("confidCommContr",
                       label = span("Are there commercial contracts tied to this data?",
                                    span(
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "Select 'yes' if the data are subject to agreements such as licensing agreements, non-disclosure,
                                        agreements or any other agreements with a commercial partner"
                                      )
                                    )),
                       choices= c("No", "Yes"),
                       selected = "No"
          ),
          radioButtons("confidSecret",
                       label = span("If the data is leaked or shared inappropriately, could that pose a risk to national security?",
                                    span(
                                      tooltip(
                                        bsicons::bs_icon("info-circle"),
                                        "Select 'yes' if the data contain state secrets or other information that could be used to 
                                        disrupt critical national infrastructure, defense or economic stability"
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
    actionButton("reset", "Reset", class="btn btn-outline-primary"),
    # create download button to download BIV
    # first create action button that user clicks so that a modal can pop up
    # make this button disabled until the user puts in the necessary fields for the export
    actionButton("downloadTrigger", "Download classification", class="btn btn-primary", icon = icon("download")
                 , disabled = TRUE,
                 ),
    # then make a hidden download button the server clicks for us
    # needs to be down this way because if we want a modal to pop up the button the user clicks
    # on must be an actionButton, but if we want to carry out the downloadHandler function in the server
    # to generate the data classification, we need a downloadButton to be "clicked". In this case
    # it's clicked in secret!
    downloadButton("downloadHidden", class="invisibleBtn")
    
    
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
      
    ),
    # panel for instructions on how to use the tool (if needed)
    nav_panel(
      "Feedback",
      withTags({
        div(h2("Do you have feedback on this app?"),
            br(),
            p("This app is currently in its first release and will undergo further refinement. Your feedback will help greatly in making this app function better for you and your colleagues!"),
            br(),
            a(href="https://vuamsterdam.eu.qualtrics.com/jfe/form/SV_6F3lfyQDtjLf2rs", target="_blank", id="feedbackBtn", class="btn btn-primary", "Give feedback!")
        )
      })
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
  # make sure that selecting generic data types after entering some specific types doesn't clear the already entered specific types
  
  observe({
      inptGenDat <- input$genDataType
      inptSelDat <- input$dataType
      if (!is.null(inptGenDat) & is.null(inptSelDat)) {
        updateSelectInput(inputId = "dataType", choices = sort(choicesDat()), selected = input$dataType)
      } else if (!is.null(inptGenDat) & !is.null(inptSelDat)) { # makes sure entered specific types aren't cleared if general types selected after specific entered
        updateSelectInput(inputId = "dataType", choices = sort(c(choicesDat(), input$dataType)), selected = input$dataType)
      }
      else {
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
  
  
  
  # create reactive from adultChild that alters which choices are shown in particType input section
  
  choicesPart <- reactive({
    req(input$adultChild)
    partTypes <- str_remove(input$adultChild, " participants")
    combined_pattern <- paste(partTypes, collapse = "|")
    particSubset <- filter(rskResPart, str_detect(AgeCateg, combined_pattern))
    particSubset$ShortDescription
  })
  
  # use reactive to update choices available in particType
  # make sure that selecting age group after entering some participant types doesn't clear the already entered participant types
  
  observe({
    inptAgeTyp <- input$adultChild
    inptPartType <- input$particType
    
    if(!is.null(inptAgeTyp) & is.null(inptPartType)) {
      updateSelectInput(session, inputId = "particType", choices = sort(choicesPart()), selected = input$particType)
    } else if (!is.null(inptAgeTyp) & !is.null(inptPartType)) { # makes sure entered partic types aren't cleared if age group selected after participant type entered
      updateSelectInput(session, inputId = "particType", choices = sort(c(choicesPart(), input$particType)), selected = input$particType)
    }
      else {
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
    if (maxY() >= 3.0) {
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
  
  # account for extra confidentiality questions and results to incorporate into rskCateg
  
  confidSecret <- reactive({
    req(input$confidSecret)
    confidSecret <- input$confidSecret
  })
  
  confidOblig <- reactive({
    req(input$confidOblig)
    confidOblig <- input$confidOblig
  })
  
  confidKnowlSec <- reactive({
    req(input$confidKnowlSec)
    confidKnowlSec <- input$confidKnowlSec
  })
  
  confidIP <- reactive({
    req(input$confidIP)
    confidIP <- input$confidIP
  })
  
  confidCommContr <- reactive({
    req(input$confidCommContr)
    confidCommContr <- input$confidCommContr
  })
  
  confidCommVal <- reactive({
    req(input$confidCommVal)
    confidCommVal <- input$confidCommVal
  })
  
  confidRes <- reactive({
    
    if (confidSecret() == "Yes") {
      return("secret")
    } else if (sum(confidOblig() == "Yes",
                   confidKnowlSec() == "Yes",
                   confidIP() == "Yes",
                   confidCommContr() == "Yes",
                   confidCommVal() == "Yes") >= 2) {
      return("at least 2")
    } else if (sum(confidOblig() == "Yes",
                   confidKnowlSec() == "Yes",
                   confidIP() == "Yes",
                   confidCommContr() == "Yes",
                   confidCommVal() == "Yes") >= 1) {
      return("at least 1")
    } else {
      return("none")
    }
    
  })
  
  
  # determine color of risk category as reactive rskCateg()
  # see inline comments for reasoning for cutoffs
  # general reasoning: 
  # -maxX >= 4.0 is directly identifiable
  # -maxY >= 2.0 is increased, mid-range vulnerability 
  # -maxY >=3.0 is extreme vulnerability due to additional stigma 
  # -maxY >=3.0 is where harmLvl auto changes to Yes and requires manual change to No by user
  
  rskCateg <- reactive({
    
    # first define all results based on maxX maxY and harmLvl as base_colour
    
    base_colour <- 
      if (maxX() >= 4.0 & maxY() >= 2.0 & harmLvl() == "Yes") {
      "RED" # fully identifiable, increased vulnerability and increased risk of harm
    } else if (maxX() >= 4.0 & maxY() >= 3.0 & harmLvl() == "No") {
      "RED" # fully identifiable, extreme vulnerability but no increased risk of harm 
      #-> made still RED because of extreme vulnerability characteristics; subject to debate and discusion
    } else if ((maxX() >= 3.0 & maxX() < 4.0) & maxY() >= 3.0 & harmLvl() == "Yes") {
      "RED" # high risk indirectly identifiable (easy to reidentify), extreme vulnerability and increased risk of harm
    } else if (maxX() >= 4.0 & (maxY() >= 2.0 & maxY() < 3.0) & harmLvl() == "No") {
      "ORANGE" # fully identifiable, increased but not extreme vulnerability and no increased risk of harm 
    } else if (maxX() >= 4.0 & maxY() < 2.0 & (harmLvl() == "Yes"|harmLvl() == "No")) {
      "ORANGE" # fully identifiable and low vulnerability, risk of harm is irrelevant
      #-> potentially consider red for elevated risk of harm; subject to debate and discussion
    } else if ((maxX() >= 3.0 & maxX() < 4.0) & maxY() >= 3.0 & harmLvl() == "No") {
      "ORANGE" # high risk indirectly identifiable (easy to reidentify), extreme vulnerability but no increased risk of harm
    } else if ((maxX() >= 3.0 & maxX() < 4.0) & (maxY() >= 2.0 & maxY() < 3.0) & harmLvl() == "Yes") {
      "ORANGE" # high risk indirectly identifiable (easy to reidentify), increased but not extreme vulnerability and increased risk of harm
    } else if ((maxX() >= 3.0 & maxX() < 4.0) & (maxY() >= 2.4 & maxY() < 3.0) & harmLvl() == "No") {
      "ORANGE" # high risk indirectly identifiable (easy to reidentify), higher level of increased but not extreme vulnerability and no increased risk of harm
    } else if ((maxX() >= 3.5 & maxX() < 4.0) & (maxY() >= 2.0 & maxY() < 2.4) & harmLvl() == "No") {
      "ORANGE" # higher end of high risk indirectly identifiable (easier to reidentify), lower level of increased vulnerability 
      #and no increased risk of harm
    } else if ((maxX() >= 3.0 & maxX() < 4.0) & maxY() < 2.0 & harmLvl() == "Yes") {
      "ORANGE" # high risk indirectly identifiable (easy to reidentify), low vulnerability and increased risk of harm
    } else if ((maxX() >= 1.8 & maxX() < 3.0) & maxY() >= 3.0 & harmLvl() == "Yes") {
      "ORANGE" # low risk indirectly identifiable (harder to reidentify), extreme vulnerability and increased risk of harm
    } else if ((maxX() >= 3.0 & maxX() < 3.5) & (maxY() >= 2.0 & maxY() < 2.4) & harmLvl() == "No") {
      "YELLOW" # lower end of high risk indirectly identifiable (easier to reidentify), lower level of increased vulnerability 
      #and no increased risk of harm
    } else if ((maxX() >= 3.0 & maxX() < 4.0) & maxY() < 2.0 & harmLvl() == "No") {
      "YELLOW" # high risk indirectly identifiable (easy to reidentify), low vulnerability and no increased risk of harm
    } else if ((maxX() >= 1.8 & maxX() < 3.0) & maxY() >= 3.0 & harmLvl() == "No") {
      "YELLOW" # low risk indirectly identifiable (harder to reidentify), extreme vulnerability and NO increased risk of harm
      #-> could argue making ORANGE because of extreme vulnerability characteristics; subject to debate and discusion
    } else if ((maxX() >= 1.8 & maxX() < 3.0) & maxY() < 3.0 & (harmLvl() == "Yes"|harmLvl() == "No")) {
      "YELLOW" # low risk indirectly identifiable (harder to reidentify), vulnerability anything other than extreme (i.e. either low or increased, but not extreme), risk of harm is irrelevant
    } else if (maxX() < 1.8 & grnDatSum() > 0 & maxY() >= 2.0 & harmLvl() == "Yes") {
      "YELLOW" # technically personal data, increased and extreme vulnerability and increased risk of harm
    } else if (maxX() < 1.8 & grnDatSum() > 0 & maxY() < 2.0 & (harmLvl() == "Yes"|harmLvl() == "No")) {
      "GREEN" # technically personal data, low vulnerability, risk of harm is irrelevant
    } else if (maxX() < 1.8 & grnDatSum() > 0 & maxY() >= 2.0 & harmLvl() == "No") {
      "GREEN" # technically personal data, increased and extreme vulnerability and no increased risk of harm
      #-> potentially handle extreme vulernability separately here and consider as yellow even with no increased risk of harm; subject to debate and discussion
    } else if (maxX() < 1.8 & grnDatSum() == 0 & maxY() >= 2.0 & harmLvl() == "Yes") {
      "GREEN" # not personal data, increased and extreme vulnerability and increased risk of harm
      #-> could argue if not personal data, should not elevate, but risks of harm suggest need to be careful. May also want to consider handling extreme vulnerability PLUS increased risk of harm as YELLOW; subject to debate and discussion
    } else if (maxX() < 1.8 & grnDatSum() == 0 & maxY() < 2.0 & (harmLvl() == "Yes"|harmLvl() == "No")) {
      "BLUE" # not personal data, low vulernability, risk of harm is irrelevant
    } else if (maxX() < 1.8 & grnDatSum() == 0 & maxY() >= 2.0 & harmLvl() == "No") {
      "BLUE"
      # not personal data, increased and extreme vulnerability and NO increased risk of harm
      #-> could argue at least for extreme vulnerability to consider as GREEN; subject to debate and discussion
    }
    
  # then account for any answers to confidentiality questions that are not "No" to modify the base_colour to the final_colour
    
  final_colour <- base_colour
    
  if (confidRes() == "secret") {
    final_colour <- "RED"
  } else if (confidRes() == "at least 2"){
    if (base_colour == "YELLOW") {
      final_colour <- "ORANGE"
    }
    if (base_colour == "GREEN" | base_colour == "BLUE") {
      final_colour <- "ORANGE"
    }
  } else if (confidRes() == "at least 1") {
    if (base_colour == "YELLOW") {
      final_colour <- "ORANGE"
    }
    if (base_colour == "GREEN" | base_colour == "BLUE") {
      final_colour <- "YELLOW"
    }
  }
  
  #call return final_colour to set that colour to the rskCateg() reactive
  
  return(final_colour)
  
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
  
  # create reactive called rskCategLvl() to use as a translation/definition of the colour categories
  # and to use in the data classification output
  
  rskCategLvl <- reactive({
    if (rskCateg() == "RED") {
      return("Very high")
    }
    if (rskCateg() == "ORANGE") {
      return("High")
    }
    if (rskCateg() == "YELLOW") {
      return("Medium")
    }
    if (rskCateg() == "GREEN") {
      return("Medium to low")
    }
    if (rskCateg() == "BLUE") {
      return("Low")
    }
  })
  
  # create reactive called confidResLvl to translate what the confidRes values mean into something understandable 
  # for the user to read
  
  confidResLvl <- reactive({
    if (confidRes() == "secret") {
      return("very high because the data is secret (i.e. it poses risks to national security)")
    }
    if (confidRes() == "at least 2") {
      return("elevated")
    }
    if (confidRes() == "at least 1") {
      return("slightly elevated")
    }
    if (confidRes() == "none") {
      return("not elevated")
    }
  })

  # create reactive that summarizes in one variable all confidentiality questions with the answer "yes"
  # text will be used in BIV export
  
  confidConc <- reactive({
    
    confidConc <- c()
    if (confidRes() != "none") {
      if (confidSecret() == "Yes") {
        confidConc <- c(confidConc, "The data pose risk(s) to national security")
      }
      if (confidOblig() == "Yes") {
        confidConc <- c(confidConc, "The data are subject to confidentiality obligations")
      }
      if (confidKnowlSec() == "Yes") {
      confidConc <- c(confidConc, "The data contain unique knowledge, methods or algorithms")
      }
      if (confidIP() == "Yes") {
        confidConc <- c(confidConc, "The data are potentially patentable or related to unpublished innovation(s)")
      }
      if (confidCommContr() == "Yes") {
        confidConc <- c(confidConc, "The data are subject to commercial contracts")
      }
      if (confidCommVal() == "Yes") {
        confidConc <- c(confidConc, "The data are commercially valuable")
      } 
    } else {
      confidConc <- c(confidConc, "None")
    }
    return(confidConc)
  }
  )
  
  # create a reactive that identifies which data processing activities were chosen by user
  # text will be used in BIV export
  
  datProc <- reactive({
    req(input$datActiv)
    
    datProc <- input$datActiv
    
  })
  
  datProcDefn <- reactive({
    datProcDefn <- datProc()
    datProcDefn <- str_replace(datProcDefn, "storage", "Data storage during active research")
    datProcDefn <- str_replace(datProcDefn, "transfer", "Data transfer to research collaborators")
    datProcDefn <- str_replace(datProcDefn, "students", "Data use by students")
    datProcDefn <- str_replace(datProcDefn, "archiving", "Data archiving and publishing after active research")
    datProcDefn <- str_replace(datProcDefn, "reuse", "Data reuse after active research")
  })
  
  
  # create a reactive that identifies when all required input fields are filled (esp when those fields are used with the req call within a reactive)
  # so that this individual reactive can be used to determine when the app is ready for the final output or for the export etc (to prevent
  # early or unnecessary results displaying or downloading or whatever else gets added)
  # note that technically only dataType and particType need to be checked for input in the current iteration (v1.0) and not the radio
  # button inputs, but this allows protection if the radio buttons ever get changed to something requiring user input
  
  is_ready <- reactive({
    # List the core text inputs
    required_inputs <- c("dataType", "particType")
    
    # List radio buttons
    radio_inputs <- c(
      "harmLvl", "confidSecret", "confidOblig", "confidKnowlSec", 
      "confidIP", "confidCommContr", "confidCommVal"
    )
    
    # Check if EVERY input in both lists is 'truthy'
    all(sapply(c(required_inputs, radio_inputs), function(x) isTruthy(input[[x]])))
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
    updateSelectInput(inputId = "confidCommVal", selected="No")
    updateSelectInput(inputId = "confidOblig", selected="No")
    updateSelectInput(inputId = "confidKnowlSec", selected="No")
    updateSelectInput(inputId = "confidIP", selected="No")
    updateSelectInput(inputId = "confidCommContr", selected="No")
    updateSelectInput(inputId = "confidSecret", selected="No")
    updateSelectInput(inputId = "datActiv", selected=c("storage","transfer","students","archiving","reuse"))
    reset$clear <- NA
    
  }
  ) 
  
  
  #### OUTPUTS
  
  # create reactive for plot so that can also use in downloadHandler as a png in the exported report
  
  XYplot <-  reactive({
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
  }
  )
  
  # plot output of overall results
  
  output$XYplot <- renderPlot({
    # if reactive value reset$clear is NA (only upon initial session or after action button reset is clicked)
    # then no output will be returned. If anything else, the plot will be returned
    if (is.na(reset$clear)) return()
    
   return(XYplot())
    
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
    
    if (rskCateg() == "RED") {
      contactTxt <- span(# use span() and html tags to include a hyperlink and other formatting within the text of contactTxt
        " - contact ", 
        tags$a(href = "mailto:research.data.fgb@vu.nl", "research.data.fgb@vu.nl"), 
        " to inform the FGB REPS team that you will be working with ",
        tags$strong("very high risk data")
      )
    } else {
      contactTxt <- NULL 
    }
    
    # create unordered list of results in text form. Use tags to create correct html tags via R
    
    tags$ul(
      tags$li(
        "Your data risk classification is ", strong(rskCateg()), 
        " which means your risk level is ", strong(tolower(rskCategLvl())),
        contactTxt
      ),
      tags$li(
        "You have indicated that there ",  strong(harmTxt), 
          " increased risks of harm to your research subjects"
      ),
      tags$li(
        "You have indicated that the confidentiality risks (addressed in the additional questions on confidentiality) are ",
        strong(confidResLvl())
      )
      # optional line for testing e.g. when need to see maxX and maxY values or other info not relevant to final app
      # , tags$li(strong("Re-identification risk is", maxX(), "and vulnerability risk is",  maxY()))
      
    )
    
    }
  )
  
  # once results have been generated enable the downloadTrigger button 
  
  observe({
    shinyjs::toggleState("downloadTrigger", condition = is_ready())
  })
  
  
  
  # make output for recommendations text on the second tab of UI
  
  # set up action button shwYelReus that only appears if rskCateg is GREEN and datProc is "reuse"
  
  # first create reactive value show$yellow that will always be NA except for the conditions when
  # we want the action button shwYelReus to be shown
  
  show <- reactiveValues(yellow = NA)
  
  # allow shwYelReus action button to be seen
  
  observe({
    # 1. Use is_ready reactive to test that all required fields are complete
    ready <- is_ready()
    
    # 2. check conditions of whether rskCateg is green and datProc includes data reuse
    is_green  <- ready && rskCateg() == "GREEN"
    has_reuse <- ready && "reuse" %in% input$datActiv
    
    if (is_green && has_reuse) {
      if (is.na(show$yellow)) {
        shinyjs::show("shwYelReus")
      } else {
        shinyjs::hide("shwYelReus")
      }
    } else {
      # This block now ALWAYS runs if inputs are cleared!
      shinyjs::hide("shwYelReus")
      show$yellow <- NA 
    }
  })
  
  # on click of shwYelReus show$yellow reactive value becomes 1 which will impact the chosen markdown
  # outputs in the following renderUI section
  
  observeEvent(input$shwYelReus, {
    
    show$yellow <- 1
    shinyjs::hide(id = "shwYelReus")
    
  })
  
  # based on datProc as well as rskCateg calculations, concat recommendations text and render into UI
  # if the action button shwYelReus has been clicked and the rskCateg is GREEN the first action will carry out
  # and include the yellow reuse instructions. For all other cases, the recommendations will simply be
  # combined based on rskCateg color and which datProc activities are currently selected by the user
  
  combineMD <- reactive({
    if (isTruthy(input$datActiv)) {
      if (rskCateg() == "RED") {
        combineMD <- c(readLines("mdFiles/genRecRED.Rmd")) #read in Rmd that provides the yaml and css at very top & Rmd that gives general guidance
        for(i in 1:length(datProc())) {
          mdFile <- paste0("mdFiles/",datProc()[i],rskCateg(), ".Rmd")
          combineMD <- c(combineMD, readLines(mdFile))
        }
      } else if (!is.na(show$yellow)) {
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
    } else if (rskCateg() == "RED") {
      combineMD <- c(readLines("mdFiles/genRecRED.Rmd"))
    } else {
      combineMD <- c(readLines("mdFiles/genRec.Rmd"))
    }
    
    
    # Copy to a temp directory to avoid issues with multiple users working with the app at the same time
    tempRecRMD <- file.path(tempdir(), "combinedRec.Rmd")
    tempRecHTML <- file.path(tempdir(), "combinedRec.html")
    
    writeLines(combineMD, tempRecRMD)
    
    # originally rendered Rmd to markdown but changed to rendering to html below
    # just need to ensure that markdownToHTML command includes template = FALSE to not
    # mess up formatting
    
    #includeMarkdown(render("mdFiles/combinedRec.Rmd", 
     #                      output_file = 'combinedRec', 
      #                     output_dir ='mdFiles', 
       #                    quiet = TRUE))
    
    HTML(markdown::markdownToHTML(knit(tempRecRMD, 
                                       output = tempRecHTML,
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
  
  # save user input so that it doesn't disappear on refresh
  # uses shinyStorePlus package https://github.com/oobianom/shinyStorePlus
  appid <- "app666" # id doesn't matter too much AFAIK
  setupStorage(appId = appid, inputs = TRUE) 
  
  
  
  # set up downloadHandler 
  # and connect to download button
  # to generate BIV report from user input on request
  
  # 1. create a modal that pops up with a message when downloadTrigger is clicked
  
  observeEvent(input$downloadTrigger, {
    
    # Disable while download happens so that user cannot repeatedly click if download is slow
    shinyjs::disable("downloadTrigger")

    # show modal message
    showModal(modalDialog(
      title = "Downloading data classification",
      p("Please wait while your data classification is downloaded"),
      p("Once complete, review your classification and fill in any fields not addressed by this tool"),
      p("Also, review the sections in your classification on 'Availability' and 'Integrity' to determine if your situation 
        deviates from the default"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    
    # trigger hidden download button using JavaScript
    # This 'clicks' the hidden button for the user
    delay(50, shinyjs::click("downloadHidden"))
    
    # Re-enable button once download is complete
    shinyjs::enable("downloadTrigger")

  })
  
  
  # 3. results of the downloadHidden button being clicked behind the scenes
  
  output$downloadHidden <- downloadHandler(
    filename = function() {
      paste0("biv_", format(Sys.time(), '%Y%m%d_%H%M%S'), '.docx')
    },
    content = function(file) {
      

      # only generate plot for export if it exists
      if (isTruthy(input$dataType) && isTruthy(input$particType)) {
        # adjust plot settings for word export
      
        current_plot <- XYplot() + 
          theme(axis.text.x=element_text(size=12),
              axis.text.y=element_text(size=12, angle = 90, hjust = 0.6),
              axis.title=element_text(size=18,face="bold")
          )
      
        # adjust size of point on plot (need to do with following to adjust existing ggplot layer
        # rather than just creating a new layer on top)
      
        current_plot$layers[[1]]$aes_params$size <- 8
      
        # create a temp path for saving XYplot as a PNG
        plot_path <- file.path(tempdir(), "temp_user_plot.png")
        # Save it to the temp path
        ggsave(plot_path, plot = current_plot, device = "png")
      
      } else {
        plot_path <- file.path(tempdir(), "blank.png")
        file.copy(normalizePath("img/blank.png"), plot_path, overwrite = TRUE)
      }
      
      # Copy the template to a temp directory to avoid permission issues
      tempReport <- file.path(tempdir(), "templateBIV.Rmd")
      file.copy("mdFiles/templateBIV.Rmd", tempReport, overwrite = TRUE)
      
      # Also copy the style template to the tempdir so that it can be read during render
      tempStyle <- file.path(tempdir(), "style_template.docx")
      file.copy("mdFiles/style_template.docx", tempStyle, overwrite = TRUE)
      
      
      
      
      # Knit the document and pass Shiny inputs to RMarkdown params
      rmarkdown::render(
        input = tempReport,
        output_file = file,
        params = list(
          data_types = if(isTruthy(input$dataType)) input$dataType else "\\<describe data types collected and used\\>",
          partic_types = if(isTruthy(input$particType)) input$particType else "\\<describe the research subjects whose data will be used\\>",
          extra_harm = input$harmLvl,
          data_proc = if (isTruthy(input$datActiv)) datProcDefn() else "\\<describe the data processing activities planned\\>",
          risk_categ = if (isTruthy(input$dataType) && isTruthy(input$particType)) rskCategLvl() else "NA",
          plot_output = plot_path,
          confid_conc = confidConc()
        ),
        output_options = list(reference_docx = tempStyle), # ensures that render can find the reference style document
        envir = new.env(parent = globalenv())
      )
    }
    
  )
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)