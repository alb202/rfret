library(shiny)
library(shinyjs)

ui <- fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("RFret", windowTitle = "RFret - Rapid FRET Analysis in R"),
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
                         checkboxInput('view_quality_filter',
                                       'Do you want to skip inspection of raw data?', FALSE),
                         #tags$hr(),
                         textInput('skip_rows', 'How many rows should be skipped?', 4, width = 250),
                         radioButtons('sep', 'Separator',
                                      c(Comma=',',
                                        Semicolon=';',
                                        Tab='\t'),
                                      ','),
                         radioButtons('quote', 'Quote',
                                      c(None='',
                                        'Double Quote'='"',
                                        'Single Quote'="'"),
                                      '"')
                ),
                tabPanel("Advanced Options",
                         tags$hr(),
                         radioButtons('algorithm', 'Algorithm',
                                      c('Basic algorithm'='basic',
                                        'Advanced algorithm'="advanced")),
                         checkboxInput('option2', 'Option2', value = FALSE) ,
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
                                 wellPanel(style = "background-color: #ffffff;",
                                     fluidRow(
                                         column(10, h2(textOutput("filename", inline = TRUE))),
                                         column(2, imageOutput("decision_image", width = 40, height = 40))
                                     ),

                                     plotOutput("splash_screen"),
                                     plotOutput(outputId = "raw_output"),
                                     hidden(
                                         actionButton(inputId = "accept", label ="Accept"),
                                         actionButton(inputId = "reject", label ="Reject"),
                                         actionButton(inputId = "previous", label ="Previous"),
                                         actionButton(inputId = "next1", label ="Next"),
                                         actionButton(inputId = "accept_all", label ="Accept All"),
                                         actionButton(inputId = "accept_all_subsequent", label ="Accept all subsequent"),
                                         actionButton(inputId = "process_all", label ="Process the selected files"))
                                 )),
                        tabPanel(title = "Full analysis", value = "full_analysis",
                                 wellPanel(style = "background-color: #ffffff;",
                                           h1(textOutput("Batch Analysis")),
                                           plotOutput("processed_output"),
                                           #tableOutput("table_output"),
                                           downloadButton("plot_download")
                                 ))
            )
        )
    )
)
