## SERVER.R
library(ggplot2)
library(shinyjs)
library(gridExtra)
source("../R/inspect_raw_data.R")

function(input, output, session) {
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
        hideElement("splash_screen")
        showElement("raw_output")
        showElement("accept")
        values$number_of_files <- length(df)
        if (is.null(df)) return(NULL)
        return(df)
    })
    output$filename <- renderText({
        if(is.null(input$data_file)) return(NULL)
        paste(values$file_index, "-    ", {myData()[[values$file_index]][[1]]})
    })

    output$raw_output <- renderPlot(width = 750, height = 350, {
        if(is.null(input$data_file)) return(NULL)
        showElement("accept")
        df <- data.frame(myData()[[values$file_index]][[2]])
        raw_data_plots <- inspect_raw_data(df)
        raw_data_grid <- grid.arrange(raw_data_plots$donor, raw_data_plots$acceptor, raw_data_plots$fret, ncol=2, nrow=2)
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

    observeEvent(input$process_all, {
        print("processing")
        updateTabsetPanel(session, "main", selected = "Batch processing")
        #output$processed_output <- renderPlot({ Enter batch function here })
        print(list(input$data_file$datapath[values$dataset_decisions]))
        print(list(input$data_file$name[values$dataset_decisions]))
        print(input$sep)
        print(input$quote)
        print(TRUE)
        print(input$skip_rows)
    })

    prelim_listener <- reactive({
        list(input$next1, input$previous, input$accept, input$reject, input$accept_all, input$accept_all_subsequent, input$data_file)
    })

    observeEvent(prelim_listener(), {
        if(!is.null(values$dataset_decisions)){
            toggleElement(id = "process_all", condition = (sum(is.na(values$dataset_decisions)) == 0))
            toggleElement(id = "next1", condition = ((!is.null(input$data_file)) && values$file_index < values$number_of_files))
            toggleElement(id = "previous", condition = ((!is.null(input$data_file)) && values$file_index > 1))
            toggleElement(id = "accept", condition = (!is.null(input$data_file)))
            toggleElement(id = "reject", condition = (!is.null(input$data_file)))
            toggleElement(id = "accept_all_subsequent", condition = (!is.null(input$data_file)))
            toggleElement(id = "accept_all", condition = (!is.null(input$data_file)))
            if(!is.na(values$dataset_decisions[[values$file_index]])){
                if(values$dataset_decisions[[values$file_index]] == TRUE){
                    output$decision_image <- renderImage({list(src = "accept.png",
                                                               width = 50, height = 50, contentType = 'image/png',alt = "Accept")},
                                                         deleteFile = FALSE)
                    showElement(id = "decision_image")}
                if(values$dataset_decisions[[values$file_index]] == FALSE){
                    output$decision_image <- renderImage({list(src = "reject.png",
                                                               width = 50, height = 50, contentType = 'image/png',alt = "Reject")},
                                                         deleteFile = FALSE)
                    showElement(id = "decision_image")}
            } else hideElement(id = "decision_image")
        }
    })
}
## Accept and reject icons are from: http://www.iconsdb.com/green-icons/checked-checkbox-icon.html, http://www.iconsdb.com/red-icons/x-mark-4-icon.html
