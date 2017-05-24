library(shiny)

fluidPage(
    titlePanel("RFret"),
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
                tabPanel("Input datasets",
                         fileInput('data_file', 'Choose 1 or more FRET data files:',
                                   accept=c('text/csv',
                                            'text/comma-separated-values,text/plain',
                                            '.csv'),
                                   multiple = TRUE
                         ),
                         tags$hr(),
                         textInput('skip_rows', 'How many rows should be skipped?', 4, width = 250),
                         checkboxInput('header', 'Are the column names present?', TRUE),
                         radioButtons('sep', 'Separator',
                                      c(Comma=',',
                                        Semicolon=';',
                                        Tab='\t'),
                                      ','),
                         radioButtons('quote', 'Quote',
                                      c(None='',
                                        'Double Quote'='"',
                                        'Single Quote'="'"),
                                      '"'),
                         checkboxInput('view_quality_filter',
                                       'Do you want to view the quality filter plots?', TRUE)
                ),
                tabPanel("Advanced Options",
                         checkboxInput('option1', 'Option1', value = FALSE) ,
                         checkboxInput('option2', 'Option2', value = FALSE) ,
                         selectInput('option3', label ='Option3',
                                     choices=c("1" = 1,"2"=2,"3"=3),
                                     multiple=FALSE, selectize=TRUE,selected="1"),
                         selectInput('option4', label ='Option4',
                                     choices=c("4" = 4,"5"=5,"6"=6),
                                     multiple=TRUE, selectize=TRUE,selected="4")
                ),
                tabPanel("Help",
                         h5("Help information")
                )
            )
        ),
        mainPanel("",
                  #h1(textOutput("value1")),
                  h1(textOutput("filename")),
                  plotOutput("raw_output"),
                  actionButton("accept", "Accept"),
                  actionButton("remove", "Remove"),
                  actionButton("accept_all", "Accept All")
        )
    )
)
