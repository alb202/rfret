library(shiny)
library(shinyjs)

ui <- fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "yeti.css")),
    titlePanel(strong("RFret - Rapid FRET Analysis in R"), windowTitle = "RFret - Rapid FRET Analysis in R"),
    sidebarLayout(
        sidebarPanel(width = 3,
                     tabsetPanel(id="sidebar", type = "tabs",
                                 tabPanel(title = " Data",
                                          value = "get", icon = icon("folder-open", lib="glyphicon"),
                                          wellPanel(
                                              fluidRow(
                                                  column(10, fileInput('data_file', buttonLabel = "Browse ...", placeholder = "None",
                                                                       label = 'Select FRET data file(s):',
                                                                       accept=c('text/csv',
                                                                                'text/comma-separated-values',
                                                                                'text/plain',
                                                                                '.csv'),
                                                                       multiple = TRUE
                                                  )),
                                                  column(10, numericInput('skip_rows',
                                                                          'How many rows of each file should be skipped?',
                                                                          4,
                                                                          min = 0)),

                                                  column(10, tags$hr()),
                                                  column(10, checkboxInput("save", " Save Files to folder ...", value = FALSE)),
                                                  column(10, h5(textOutput("save_dir"))),
                                                  column(10, shinyFiles::shinyDirButton(id = "find_dir",
                                                                                        label = "Select folder ...",
                                                                                        title = "Select folder ..."))))),
                                 tabPanel(title = "Filter", value = "advanced", icon = icon("ok-circle", lib="glyphicon"),
                                          wellPanel(
                                              fluidRow(
                                                  column(width = 12,
                                                         disabled(actionButton(inputId = "accept_all",
                                                                               icon = icon("check", lib = "glyphicon"),
                                                                               label="Accept All"
                                                         ))),
                                                  fluidRow(div(style = "height:5px;")),
                                                  column(width = 12,
                                                         disabled(actionButton(inputId = "process_all",
                                                                               icon = icon("play", lib = "glyphicon"),
                                                                               label="Process selected files..."
                                                         ))),
                                                  column(width = 10,tags$hr()),
                                                  column(11,
                                                         radioButtons('algorithm',
                                                                      'Select the algorithm for fitting the binding model:',
                                                                      c('Hyperbolic'='hyperbolic',
                                                                        'Quadratic'="quadratic")),
                                                         #tags$hr(),
                                                         checkboxInput("hill_coefficient",
                                                                       "Fit the Hill Coefficient (optional if algorithm is quadratic)",
                                                                       value = FALSE),
                                                         numericInput('donor_concentration',
                                                                      'Donor Concentration (required if algorithm is quadratic)',
                                                                      value = NULL),
                                                         tags$style("#conc_required{color: red;
                                                                             font-size: 12px;
                                                                             }"),
                                                                    hidden(textOutput(outputId="conc_required"))
                                                         )))),
                                 tabPanel(title = "Help", value = "help", icon = icon("question-sign", lib="glyphicon"),
                                          tags$hr(),
                                          h5("Help information")
                                 )
                     ), style='width: 110%;'
        ),
        mainPanel(width = 9,
                  tabsetPanel(id="main",
                              tabPanel(title = "Welcome", value = "welcome", icon = icon("home", lib="glyphicon"),
                                       wellPanel(
                                           fluidRow(column(width=12, offset=0)),
                                           plotOutput("splash_screen"),
                                           fluidRow(column(width=12, offset=0))
                                       )),
                              tabPanel(title = "Inspect Raw Data", value = "inspect", icon = icon("search", lib="font-awesome"),
                                       #fluidRow(
                                       wellPanel(
                                           #fluidRow(column(width=12, offset=0)),
                                           column(1,
                                                  fluidRow(align="center", div(style = "display:inline-block;width:5px;"), disabled(actionButton(inputId = "accept", label = "", icon = icon("ok", lib = "glyphicon")))),
                                                  fluidRow(align="center", div(style = "display:inline-block;width:5px;"), disabled(actionButton(inputId = "reject", label = "", icon = icon("remove", lib = "glyphicon")))),
                                                  #fluidRow(div(style = "height:5px;")),
                                                  #fluidRow(align="center", disabled(actionButton(inputId = "previous", label ="", icon = icon("arrow-up", lib = "glyphicon")))),
                                                  #fluidRow(align="center", disabled(actionButton(inputId = "next1", label ="", icon = icon("arrow-down", lib = "glyphicon")))),
                                                  fluidRow(div(style = "height:5px;")),
                                                  fluidRow(align="left", div(style = "display:inline-block;content-align:left;",
                                                      column(2, div(style = "align-content:left;background-color:none;", #width:5px; align="left",
                                                                    div(style = "height:7px;"),
                                                                    hidden(plotOutput(inline=FALSE, outputId = "decision_indicator")))),
                                                      column(2, div(style = "align-content:left;background-color:none;",#style = "align-content:left;width:10px;background-color:none;",
                                                                    hidden(uiOutput(inline=FALSE,'file_selector')))) #display:inline-block;align:left;
                                                  ))),
                                           column(11,
                                                  plotOutput(outputId = "raw_output", width = "100%", height = "100%")),
                                           fluidRow(column(width=12, offset=0))
                                       )),
                              tabPanel(title = "Fit the Binding Model", value = "fit_new", icon = icon("cogs", lib="font-awesome"),
                                       wellPanel(
                                           #fluidRow(column(width=12, offset=0)),
                                           column(1,
                                                  #fluidRow(align="center", div(style = "display:inline-block;width:5px;"), disabled(actionButton(inputId = "accept", label = "", icon = icon("ok", lib = "glyphicon")))),
                                                  #fluidRow(align="center", div(style = "display:inline-block;width:5px;"), disabled(actionButton(inputId = "reject", label = "", icon = icon("remove", lib = "glyphicon")))),
                                                  fluidRow(align="center", div(style = "display:inline-block;width:5px;"), disabled(actionButton(inputId = "previous_res", label = "", icon = icon("arrow_up", lib = "glyphicon")))),
                                                  fluidRow(align="center", div(style = "display:inline-block;width:5px;"), disabled(actionButton(inputId = "next_res", label = "", icon = icon("arrow_down", lib = "glyphicon")))),
                                                  #fluidRow(div(style = "height:5px;")),
                                                  #fluidRow(align="center", disabled(actionButton(inputId = "previous", label ="", icon = icon("arrow-up", lib = "glyphicon")))),
                                                  #fluidRow(align="center", disabled(actionButton(inputId = "next1", label ="", icon = icon("arrow-down", lib = "glyphicon")))),
                                                  fluidRow(div(style = "height:5px;")),
                                                  fluidRow(align="left", div(style = "display:inline-block;content-align:left;",
                                                                             column(2, div(style = "align-content:left;background-color:none;", #width:5px; align="left",
                                                                                           div(style = "height:7px;"),
                                                                                           hidden(plotOutput(inline=FALSE, outputId = "results_indicator")))),
                                                                             column(2, div(style = "align-content:left;background-color:none;",#style = "align-content:left;width:10px;background-color:none;",
                                                                                           hidden(uiOutput(inline=FALSE,'results_selector')))) #display:inline-block;align:left;
                                                  ))),
                                           column(11,
                                                  plotOutput(outputId = "processed_output", width = "100%", height = "100%")),
                                           fluidRow(column(width=12, offset=0))
                                       ))
                              # tabPanel(title = "Fit the Binding Model", value = "fit", icon = icon("cogs", lib="font-awesome"),
                              #          wellPanel(
                              #              h1(textOutput("Batch Analysis")),
                              #              plotOutput("processed_output", width = "100%", height = "100%")
                              #          ))
                  )
        )
    )
)
