library(shiny)
library(shinyjs)

ui <- fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "yeti.css")
        #tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');")
    ),
    #tags$style(HTML("
    #.tabbable > .nav > li > a                  {background-color: aqua;  color:black}
    #.tabbable > .nav > li[class=active]    > a {background-color: black; color:white}")),
    #.tabbable > .nav > li > a[data-value='t1'] {background-color: red;   color:white}
    #.tabbable > .nav > li > a[data-value='t2'] {background-color: blue;  color:white}
    #.tabbable > .nav > li > a[data-value='t3'] {background-color: green; color:white}
    #")),
    titlePanel(strong("RFret - Rapid FRET Analysis in R"), windowTitle = "RFret - Rapid FRET Analysis in R"),
    sidebarLayout(
        sidebarPanel(width = 4,
            tabsetPanel(id="sidebar", type = "tabs",
                        tabPanel(title = "Datasets",
                                 value = "get",
                                 wellPanel(
                                     fluidRow(
                                         column(10, fileInput('data_file',
                                                              'Choose 1 or more FRET data files:',
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
                                         column(10, checkboxInput("save", " Save Files to local folder ...", value = FALSE)),
                                         column(10, h5(textOutput("save_dir"))),
                                         column(10, shinyFiles::shinyDirButton(id = "find_dir",
                                                                               label = "Select the save directory",
                                                                               title = "Select directory ..."))))),
                        tabPanel(title = "Algorithm", value = "advanced",
                                 wellPanel(
                                     fluidRow(
                                         column(width = 5,
                                                disabled(actionButton(inputId = "accept_all",
                                                                      label="Accept All",
                                                                      width = 100))),
                                         column(width = 5,
                                                disabled(actionButton(inputId = "process_all",
                                                                      label="Process the files ...",
                                                                      width = 150))),
                                         column(width = 10,tags$hr()),
                                         column(10,
                                                radioButtons('algorithm',
                                                             'Select the algorithm for fitting the binding model:',
                                                             c('Hyperbolic'='hyperbolic',
                                                               'Quadratic'="quadratic")),
                                                tags$hr(),
                                                checkboxInput("hill_coefficient",
                                                              "Fit the Hill Coefficient (optional if algorithm is quadratic)",
                                                              value = FALSE),
                                                numericInput('donor_concentration',
                                                             'Donor Concentration (required if algorithm is quadratic)',
                                                             value = NULL)
                                                #textOutput("Enter a donor concentration or change the algorithm!")
                                         )))),
                        tabPanel(title = "Help", value = "help",
                                 tags$hr(),
                                 h5("Help information")
                        )
            )
        ),
        mainPanel(width = 8,
            tabsetPanel(id="main",
                        tabPanel(title = "Welcome", value = "welcome",
                                 wellPanel(
                                     fluidRow(column(width=12, offset=0)),
                                     plotOutput("splash_screen"),
                                     fluidRow(column(width=12, offset=0))
                                 )),
                        tabPanel(title = "Inspect Raw Data", value = "inspect",
                                 wellPanel(
                                     fluidRow(
                                         tags$html(
                                             tags$table(width="100%", cellspacing=0, cellpadding=0, bordercolor="none", border=0,  height="5%",
                                                        #tags$style(type='text/css','#table {background-color: transparent; border-color: red}'),
                                                        tags$colgroup(
                                                            tags$col(width="40%"),
                                                            tags$col(width="10%"),
                                                            tags$col(width="10%"),
                                                            tags$col(width="10%"),
                                                            tags$col(width="10%"),
                                                            tags$col(width="10%"),
                                                            tags$col(width="10%")
                                                        ),
                                                        tags$tr(bordercolor="red",
                                                            tags$td(colspan=7, valign="bottom", height="50%",
                                                                    hidden(plotOutput("decision_indicator", height = "5%")))),
                                                        tags$tr(
                                                            tags$td(valign="top", h3(textOutput("filename"))),
                                                            tags$td(valign="bottom", actionButton(inputId = "accept", label="Accept")),
                                                            tags$td(valign="bottom", actionButton(inputId = "reject", label="Reject")),
                                                            tags$td(""),
                                                            tags$td(valign="bottom", disabled(actionButton(inputId = "previous", label ="Previous"))),
                                                            tags$td(valign="bottom", disabled(actionButton(inputId = "next1", label ="Next"))),
                                                            tags$td("")
                                                        )))),
                                     plotOutput(outputId = "raw_output", width = "100%", height = "100%"))),
                        tabPanel(title = "Fit the Binding Model", value = "fit",
                                 wellPanel(
                                     h1(textOutput("Batch Analysis")),
                                     plotOutput("processed_output")
                                     # tableOutput("table_output"),
                                     # downloadButton("plot_download")
                                 ))
            )
        )
    )
)
