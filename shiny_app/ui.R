library(shiny)
library(shinyjs)

ui <- fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "yeti.css")),
    #tags$head(tags$style(".rightAlign{float:right;padding:0;}")),

        #tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');")
    #),
    #tags$style(HTML("
    #.tabbable > .nav > li > a                  {background-color: aqua;  color:black}
    #.tabbable > .nav > li[class=active]    > a {background-color: black; color:white}")),
    #.tabbable > .nav > li > a[data-value='t1'] {background-color: red;   color:white}
    #.tabbable > .nav > li > a[data-value='t2'] {background-color: blue;  color:white}
    #.tabbable > .nav > li > a[data-value='t3'] {background-color: green; color:white}
    #")),
    titlePanel(strong("RFret - Rapid FRET Analysis in R"), windowTitle = "RFret - Rapid FRET Analysis in R"),
    sidebarLayout(
        sidebarPanel(width = 3,
            tabsetPanel(id="sidebar", type = "tabs",
                        tabPanel(title = "Data",
                                 value = "get",
                                 wellPanel(
                                     fluidRow(
                                         column(10, fileInput('data_file',
                                                              'Select FRET data file(s):',
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
                                         #column(10, h5(textOutput("save_dir"))),
                                         column(10, shinyFiles::shinyDirButton(id = "find_dir",
                                                                               label = "Select folder ...",
                                                                               title = "Select folder ..."))))),
                        tabPanel(title = "Filter", value = "advanced",
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
                                                             value = NULL)
                                                #textOutput("Enter a donor concentration or change the algorithm!")
                                         )))),
                        tabPanel(title = "?", value = "help",
                                 tags$hr(),
                                 h5("Help information")
                        )
            )
        ),
        mainPanel(width = 9,
                  tabsetPanel(id="main",
                              tabPanel(title = "Welcome", value = "welcome",
                                       wellPanel(
                                           fluidRow(column(width=12, offset=0)),
                                           plotOutput("splash_screen"),
                                           fluidRow(column(width=12, offset=0))
                                       )),
                              tabPanel(title = "Inspect Raw Data", value = "inspect",
                                       #fluidRow(
                                       wellPanel(
                                           fluidRow(column(width=12, offset=0)),
                                           column(1,
                                                  fluidRow(actionButton(inputId = "accept", label = "", icon = icon("ok", lib = "glyphicon"))),
                                                  fluidRow(actionButton(inputId = "reject", label = "", icon = icon("remove", lib = "glyphicon"))),
                                                  fluidRow(div(style = "height:5px;")),
                                                  fluidRow(disabled(actionButton(inputId = "previous", label ="", icon = icon("arrow-up", lib = "glyphicon")))),
                                                  fluidRow(disabled(actionButton(inputId = "next1", label ="", icon = icon("arrow-down", lib = "glyphicon")))),
                                                  fluidRow(div(style = "height:5px;")),
                                                  fluidRow(hidden(plotOutput(inline=TRUE, outputId = "decision_indicator")))
                                           ),
                                           column(11,
                                                  plotOutput(outputId = "raw_output", width = "100%", height = "100%")),
                                           fluidRow(column(width=12, offset=0))
                                       )),
                                       #)),
                                #       wellPanel(
                                #           fluidRow(plotOutput(outputId = "raw_output", width = "100%", height = "100%")))

                              #          wellPanel(
                              #              #fluidRow(column(12, style = "background-color:orange;padding:0;", div(style = "margin: 0"), h3(textOutput("filename")))),
                              #              column(2,  #style = "display:inline-block;padding:0;background-color:green;",
                              #                     column(6,
                              #                            fluidRow(
                              #                                column(12, align = "center", style = "background-color:none;padding:0;",
                              #                                       div(style = ""), actionButton(inputId = "accept", label = "", icon = icon("ok", lib = "glyphicon")),
                              #                                       style="float:rightt;padding:0;")),
                              #                            fluidRow(
                              #                                column(12, align = "center", style = "background-color:none;padding:0;",
                              #                                       div(style = "height:100%", hidden(plotOutput(inline=TRUE, outputId = "decision_indicator")))))),
                              #                     column(6,
                              #                            fluidRow(
                              #                                column(12, align = "center", style = "background-color:none;padding:0;",
                              #                                       div(style = ""), actionButton(inputId = "reject", label = "", icon = icon("remove", lib = "glyphicon")),
                              #                                       style="float:left;padding:0;")),
                              #                            fluidRow(
                              #                                column(12, align = "right", style = "background-color:none;padding:0;",
                              #                                       div(style = ""), disabled(actionButton(inputId = "previous", label ="", icon = icon("arrow-up", lib = "glyphicon"))),
                              #                                       style="float:center;padding:0;")),
                              #                            fluidRow(
                              #                                column(12, align = "right", style = "background-color:none;padding:0;",
                              #                                       div(style = ""), disabled(actionButton(inputId = "next1", label ="", icon = icon("arrow-down", lib = "glyphicon"))),
                              #                                       style="float:center;padding:0;")))),
                              #              column(9, style = "background-color:none;padding:0;",
                              #                     div(style = ""), plotOutput(outputId = "raw_output", width = "100%", height = "100%")))
                              # ),

                        #                            ),
                        #                        #tags$br(),
                        #                        tags$td(
                        #                            ,
                        #                            ),

                        #              fluidRow(
                        #                  tags$html(
                        #                      tags$style(".wrapper {display: grid; grid-template-columns: repeat(3, 1fr); grid-gap: 2px; grid-auto-rows: minmax(10px, auto);}",
                        #                                 ".top {grid-column: 1 / 3; grid-row: 1;}",
                        #                                 ".accept { grid-column: 2 / 4; grid-row: 1 / 3;}",
                        #                                 ".reject {grid-column: 1; grid-row: 2 / 5;}",
                        #                                 ".previous {grid-column: 3; grid-row: 3;}",
                        #                                 ".next {grid-column: 2; grid-row: 4;}",
                        #                                 ".figure {grid-column: 3; grid-row: 4;}"),
                        #                      tags$div(class="wrapper",
                        #                               tags$div(class="top", "top"),
                        #                               tags$div(class="accept", "accept"),
                        #                               tags$div(class="reject", "reject"),
                        #                               tags$div(class="previous" ,"previous"),
                        #                               tags$div(class="next" ,"next"),
                        #                               tags$div(class="indicator", "indicator"),
                        #                               tags$div(class="figure", "figure")
                        #                               ))
                        #              ))),
                        #
                        # #
                        #                      tags$table(cellspacing=0, padding=0, cellpadding=0, bordercolor="black",width="95%",  #border=1,
                        #                                 tags$style(type='text/css', "table tr td {border: 1px solid black; border-collapse: collapse; border-bottom: 1px solid black; background-color: none; bordercolor: #000000}"),
                        #                                 #tags$style(style=table-layout="auto", margin=0,
                        #                         tags$colgroup(
                        #                             tags$col(width="10%"),
                        #                             tags$col(width="90%")),
                        #                         tags$tr(
                        #                             tags$td(colspan = 2, "top row for name of file")),
                        #                         tags$tr(
                        #                             tags$td(colspan = 1, valign="top", align="center", "column for decisions"),
                        #                             tags$td(colspan = 1, valign="top", align="center", "column for figure display")
                        #                             )
                        #                         )
                        #                     )
                        #              )
                        #          )
                        # ),

                                         #                        tags$td(colspan = 1, align="left", valign="bottom", ))
                                         #     tags$td(colspan = 1, ""),
                                         #     tags$td(colspan = 1, align="right", valign="bottom", ),
                                         #     tags$td(colspan = 1, align="left", ))#,
                                         # tags$td(colspan = 1, ""))


                                             #tags$title(textOutput("filename")),
                                             # tags$table(cellspacing=0, cellpadding=0, bordercolor="black", border=0,width="95%",
                                             #            #tags$style(type='text/css','#table {background-color: transparent; border-color: red}'),
                                             #            tags$colgroup(
                                             #                # tags$col(width="5%"),
                                             #                # tags$col(width="5%"),
                                             #                # tags$col(width="10%"),
                                             #                # tags$col(width="10%"),
                                             #                # tags$col(width="10%"),
                                             #                # tags$col(width="10%"),
                                             #                # tags$col(width="10%"),
                                             #                # tags$col(width="10%"),
                                             #                # tags$col(width="10%"),
                                             #                # tags$col(width="10%"),
                                             #                # tags$col(width="10%"),
                                             #                tags$col(width="8%"),
                                             #                tags$col(width="92%")
                                             #                # tags$col(),
                                             #                # tags$col(),
                                             #                # tags$col(),
                                             #                # tags$col(),
                                             #                # tags$col(),
                                             #                # tags$col(),
                                             #                # tags$col(),
                                             #                # tags$col()
                                             #            ),
                                             #
                                             #            # tags$tr(cellspacing=0, cellpadding=0,
                                             #            #     tags$td(colspan = 4, valign="top", h3(textOutput("filename")))),
                                             #            #     #tags$td(colspan = 1, align="right", valign="bottom"),
                                             #                #tags$td(colspan = 1, align="left", valign="bottom", ))
                                             #                #tags$td(colspan = 1, ""),
                                             #                #tags$td(colspan = 1, align="right", valign="bottom", ),
                                             #                #tags$td(colspan = 1, align="left", ))#,
                                             #                #tags$td(colspan = 1, ""))
                                             #
                                             #            tags$tr(cellspacing=0, cellpadding=0,
                                             #                tags$td(colspan=1, valign="top",
                                             #                        tags$td(
                                             #                            actionButton(inputId = "accept", label="Accept"),
                                             #                            actionButton(inputId = "reject", label="Reject")),
                                             #                        #tags$br(),
                                             #                        tags$td(
                                             #                            disabled(actionButton(inputId = "previous", label ="<--")),
                                             #                            disabled(actionButton(inputId = "next1", label ="-->"))),
                                             #                        tags$td(
                                             #                            tags$td(align="center",
                                             #                                    valign="top",
                                             #                                    cellspacing=0,
                                             #                                    cellpadding=0,
                                             #                                    height = "100%",
                                             #                                    hidden(plotOutput("decision_indicator"))))),
                                             #                                                     #height = "5%",
                                             #                                                     #click = clickOpts(id="plot_click",
                                             #
                                             #                tags$td(colspan=1, valign="top", align="left",
                                             #                        plotOutput(outputId = "raw_output", width = "100%", height = "100%")))
                                             #                                   #
                                             #                                   #)))
                                             # )

                                         #)))),
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
