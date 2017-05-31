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
        sidebarPanel(
            tabsetPanel(
                tabPanel("Input datasets",
                         tags$hr(),
                         fileInput('data_file', 'Choose 1 or more FRET data files:',
                                   accept=c('text/csv',
                                            'text/comma-separated-values,text/plain',
                                            '.csv'),
                                   multiple = TRUE
                         ),
                         #tags$h4("Options"),
                         #checkboxInput('skip_inspection', 'Skip inspection of raw data?', FALSE),
                         #tags$hr(style="border-color: gray; height: 33px;"),
                         #tags$h4("Data formatting options"),
                         textInput('skip_rows', 'How many rows should be skipped?', 4, width = 250),
                         wellPanel(
                                   fluidRow(
                                       column(5, radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'),',')),
                                       column(5, radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"','Single Quote'="'"),''))
                                   ))),
                tabPanel("Advanced Options", tags$hr(), radioButtons('algorithm', 'Algorithm for main analysis:', c('Hyperbola'='basic',
                        'Quadratic'="advanced")),
                        hidden(
                         checkboxInput('algorithm_option', 'Option2', value = FALSE)),
                        textInput("fret_min", "Minimum FRET signal", value = "", width = NULL, placeholder = NULL),
                        textInput("fret_max", "Maximum FRET signal", value = "", width = NULL, placeholder = NULL),
                        textInput("fret_kd", "kd", value = "", width = NULL, placeholder = NULL),
                         selectInput('option3', label ='Option3',
                                     choices=c("1" = 1,"2"=2,"3"=3),
                                     multiple=FALSE, selectize=TRUE,selected="1"),
                         selectInput('option4', label ='Option4',
                                     choices=c("4" = 4,"5"=5,"6"=6),
                                     multiple=TRUE, selectize=TRUE,selected="4")
                ),
                tabPanel("Help",
                         tags$hr(),
                         h5("Help information")
                )
            )
        ),
        mainPanel(
            tabsetPanel("main",
                        tabPanel("Pre-processing",
                                 wellPanel(
                                           # fluidRow(
                                           #     column(width = 2, offset = 4, hidden(actionButton(inputId = "process_all", label ="Process the selected files")))
                                           #     ),
                                           fluidRow(
                                               column(width = 1, offset = 0, imageOutput("decision_image", width = 30, height = 30)),
                                               column(width = 11, offset = 0, h3(textOutput("filename")))
                                               ),
                                           plotOutput("splash_screen"),
                                           fluidRow(
                                               column(width = 3, offset = 0,
                                                    column(width = 1, offset = 0, hidden(actionButton(inputId = "previous", label ="Previous"))),
                                                    column(width = 1, offset = 4 , hidden(actionButton(inputId = "next1", label ="Next")))
                                                    ),
                                               column(width = 3, offset = 0,
                                                    column(width = 1, offset = 0, hidden(actionButton(inputId = "accept", label="Accept"))),
                                                    column(width = 1, offset = 3, hidden(actionButton(inputId = "reject", label="Reject")))
                                                    ),
                                               column(width = 6, offset = 0,
                                                    column(width = 1, offset = 0, hidden(actionButton(inputId = "accept_all", label="Accept All"))),
                                                    #column(width = 1, offset = 2, hidden(actionButton(inputId = "accept_all_subsequent", label ="Accept all subsequent"))),
                                                    column(width = 1, offset = 5, hidden(actionButton(inputId = "process_all", label="Process the files ...")))
                                                    )
                                           ),tags$hr(),
                                           plotOutput(outputId = "raw_output", width = "100%", height = "100%")
                                           )),
                        tabPanel(title = "Full analysis", value = "full_analysis",
                                 wellPanel(
                                           h1(textOutput("Batch Analysis")),
                                           plotOutput("processed_output"),
                                           #tableOutput("table_output"),
                                           downloadButton("plot_download")
                                 ))
            )
        )
    )
)
