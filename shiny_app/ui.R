library(shiny)

fluidPage(
    titlePanel("RFret"),
    sidebarLayout(
        sidebarPanel(
            fileInput('input_files', 'Choose 1 or more files',
                      accept=c('text/csv',
                               'text/comma-separated-values,text/plain',
                               '.csv'),
                      multiple = TRUE
                      ),
            tags$hr(),
            checkboxInput('header', 'Header', TRUE),
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
            checkboxInput('view_quality_filter', 'Do you want to view the quality filter plots?', TRUE)
        ),
        mainPanel(
            tableOutput('contents')
        )
    )
)
