## SERVER.R
library(ggplot2)
library(shinyjs)
library(gridExtra)
library(magrittr)
library(shinythemes)
source("../R/inspect_raw_data.R")
source("../R/create_master_file.R")
source("../R/average_technical_replicates.R")
source("../R/correct_fret_signal.R")
source("../R/fit_binding_model.R")
source("../R/make_figure.R")
source("../R/guess_parameters.R")
source("graphics.R")

server <- function(input, output, session) {
    values <- reactiveValues()
    input_files <- reactive({
        if (is.null(input$data_file)) return(NULL)
        objectsLoaded <- list()
        for(i in 1:length(input$data_file$name)){
            df <- read.csv(input$data_file[i, 'datapath'],
                           header=TRUE,
                           sep=input$sep,
                           quote=input$quote,
                           skip = input$skip_rows)
            df <- list(input$data_file$name[i], df)
            objectsLoaded[[length(objectsLoaded)+1]] <- df
        }
        values$file_index <- 1
        values$number_of_files <- length(objectsLoaded)
        values$dataset_decisions <- rep(NA, length(objectsLoaded))
        return(objectsLoaded)
    })

    myData <- reactive({
        df<-input_files()
        if (is.null(df)) return(NULL)
        hideElement("splash_screen")
        showElement("raw_output")
        showElement("accept")
        values$number_of_files <- length(df)
        #if (input$skip_inspection == TRUE){
        #    values$dataset_decisions[1:length(values$dataset_decisions)] <- TRUE
        #    #input$process_all <- TRUE
        #}
        return(df)
    })
    output$filename <- renderText({
        if(is.null(input$data_file)) return(NULL)
        paste(values$file_index, "-    ", {myData()[[values$file_index]][[1]]})
    })

    output$raw_output <- renderPlot(height = 800, {
        if(is.null(input$data_file)) return(NULL)
        #showElement("accept")
        df <- data.frame(myData()[[values$file_index]][[2]])
        raw_data_plots <- inspect_raw_data(df)
        raw_data_grid <- grid.arrange(raw_data_plots$fret, raw_data_plots$donor, raw_data_plots$acceptor, nrow=3, ncol=1)
        return(raw_data_grid)
    })

    output$splash_screen <- renderPlot(once=TRUE, {
        if(is.null(input$data_file)){
            hideElement("raw_output")
            return(splash_screen())
        } else return(NULL)
    })

    observeEvent(input$accept, label = "Accept", {
        values$dataset_decisions[[values$file_index]] <- TRUE
        if(values$file_index < values$number_of_files) values$file_index <- values$file_index + 1
        View(values$dataset_decisions)
    })
    observeEvent(input$reject, label = "Reject", {
        values$dataset_decisions[[values$file_index]] <- FALSE
        if(values$file_index < values$number_of_files) values$file_index <- values$file_index + 1
        View(values$dataset_decisions)
    })
    observeEvent(input$previous, label = "Previous", {
        if(values$file_index > 1) values$file_index <- values$file_index - 1
    })
    observeEvent(input$next1, label = "Next", {
        if(values$file_index < values$number_of_files) values$file_index <- values$file_index + 1
    })
    observeEvent(input$accept_all, label = "Accept All", {
        values$dataset_decisions[1:values$number_of_files] <- TRUE
        View(values$dataset_decisions)
    })
    observeEvent(input$accept_all_subsequent, label = "Accept All Subsequent", {
        values$dataset_decisions[values$file_index:values$number_of_files] <- TRUE
        View(values$dataset_decisions)
    })

    observeEvent(input$algorithm, {toggleElement(id="algorithm_option", condition = (input$algorithm == "advanced") )})

    observeEvent(process_all_listener(), {
        print("processing")
        updateTabsetPanel(session = session,inputId = "main", selected = "full_analysis")
        print(list(input$data_file$datapath[values$dataset_decisions]))
        print(list(input$data_file$name[values$dataset_decisions]))
        print(input$sep)
        print(input$quote)
        print(TRUE)
        print(input$skip_rows)
        master_file <- makeMasterFile(input$data_file$datapath[values$dataset_decisions],
                       input$data_file$name[values$dataset_decisions],
                       input$sep,
                       input$quote,
                       TRUE,
                       input$skip_rows)

        atr <- average_technical_replicates(master_file, blanks = c("blank_1", "blank_2"), titrations = c("titration_1", "titration_2"))
        print("atr")
        cfs <- correct_fret_signal(atr)
        print("cfs")
        params <-  guess_parameters(cfs)
        #fbm <- fit_binding_model(cfs, binding_model = "quadratic", parameters = params, donor_concentration = 10) #, binding_model = "hyperbola", parameters = guess_parameters(cfs))
        #print("fbm")
        #fig <- make_figure(fbm)
        #print(atr)
        output$processed_output <- renderPlot({ return(splash_screen()) })
    })
    process_all_listener <- reactive({input$process_all})
    #process_all_listener <- reactive({isolate(input$skip_inspection==TRUE)})
    prelim_listener <- reactive({
        list(input$next1, input$previous, input$accept, input$reject, input$accept_all, input$accept_all_subsequent, input$data_file)
    })

    observeEvent(prelim_listener(), {
        if(!is.null(values$dataset_decisions)){
            toggleElement(id = "process_all", condition = ((!is.null(input$data_file))))
            toggleState(id = "process_all", condition = (sum(is.na(values$dataset_decisions)) == 0))
            toggleElement(id = "next1", condition = ((!is.null(input$data_file))))
            toggleElement(id = "previous", condition = ((!is.null(input$data_file))))
            toggleState(id = "next1", condition = ((values$file_index < values$number_of_files)))
            toggleState(id = "previous", condition = ((values$file_index > 1)))
            toggleElement(id = "accept", condition = (!is.null(input$data_file)))
            toggleElement(id = "reject", condition = (!is.null(input$data_file)))
            toggleElement(id = "accept_all_subsequent", condition = (!is.null(input$data_file)))
            toggleElement(id = "accept_all", condition = (!is.null(input$data_file)))
            if(!is.na(values$dataset_decisions[[values$file_index]])){
                if(values$dataset_decisions[[values$file_index]] == TRUE){
                    output$decision_image <- renderImage({list(src = "accept.png",
                                                               width = 50, height = 50, contentType = 'image/png',alt = "Accept")}, deleteFile = FALSE)
                }
                if(values$dataset_decisions[[values$file_index]] == FALSE){
                    output$decision_image <- renderImage({list(src = "reject.png",
                                                               width = 50, height = 50, contentType = 'image/png',alt = "Reject")}, deleteFile = FALSE)
                }
                showElement(id = "decision_image")
            } else hideElement(id = "decision_image")
        }
    })
    #shinyApp(ui, server)
}
## Accept and reject icons are from: http://www.iconsdb.com/green-icons/checked-checkbox-icon.html, http://www.iconsdb.com/red-icons/x-mark-4-icon.html
