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
                           header=input$header,
                           sep=input$sep,
                           quote=input$quote,
                           skip = input$skip_rows)
            df <- list(input$data_file$name[i], df)
            objectsLoaded[[length(objectsLoaded)+1]] <- df
        }
        print(input$data_file$datapath)
        print(input$data_file$name)
        print(input$sep)
        print(input$quote)
        print(input$header)
        print(input$skip_rows)
        values$file_index <- 1
        values$number_of_files <- length(objectsLoaded)
        values$dataset_decisions <- rep(NA, length(objectsLoaded))
        return(objectsLoaded)
    })

    myData <- reactive({
        df<-input_files()
        values$number_of_files <- length(df)
        if (is.null(df)) return(NULL)
        return(df)
    })
    output$filename <- renderText({
        if(is.null(input$data_file)) return(NULL)
        paste(values$file_index, "    ", {myData()[[values$file_index]][[1]]})
    })
    # output$decision_image <- renderImage({
    #     if(is.null(input$data_file)) return(NULL)
    #     if(values$dataset_decision[[values$file_index]] == TRUE){
    #         decision_image <- list(src = "accept.png",
    #              contentType = 'image/png',
    #              width = 40,
    #              height = 30,
    #              alt = "This file was accepted")
    #     } else if(values$dataset_decision[[values$file_index]] == FALSE){
    #         decision_image <- list(src = "remove.png",
    #              contentType = 'image/png',
    #              width = 40,
    #              height = 30,
    #              alt = "This file was removed")
    #     } else return(NULL)
    #     return(decision_image)
    # })

    output$raw_output <- renderPlot({
        if(is.null(input$data_file)) return(NULL)
        df <- data.frame(myData()[[values$file_index]][[2]])
        raw_data_plots <- inspect_raw_data(df)
        raw_data_grid <- grid.arrange(raw_data_plots$donor, raw_data_plots$acceptor, raw_data_plots$fret, ncol=2, nrow=3)
    })
    observeEvent(input$accept, label = "Accept", {
        values$dataset_decisions[[values$file_index]] <- TRUE
        if(values$file_index < values$number_of_files) values$file_index <- values$file_index + 1
        View(values$dataset_decisions)
    })
    observeEvent(input$remove, label = "Remove", {
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
        print(input$data_file$datapath[values$dataset_decisions])
        print(input$data_file$name[values$dataset_decisions])
        print(input$sep)
        print(input$quote)
        print(input$header)
        print(input$skip_rows)
    })

    prelim_listener <- reactive({
        list(input$next1, input$previous, input$accept, input$remove, input$accept_all, input$accept_all_subsequent, input$data_file)
    })

    observeEvent(prelim_listener(), {
        hideElement("process_all")
        if (is.null(input$data_file)){
            hideElement("process_all")
            hideElement("next1")
            hideElement("previous")
            hideElement("accept")
            hideElement("remove")
            hideElement("accept_all_subsequent")
            hideElement("accept_all")
            return(NULL)
        } else {
            showElement("accept")
            showElement("remove")
            showElement("accept_all_subsequent")
            showElement("accept_all")

            if (values$file_index >= values$number_of_files){
                hideElement("next1")
            } else {
                showElement("next1")
            }
            if (values$file_index == 1){
                hideElement("previous")
            } else {
                showElement("previous")
            }
            if (sum(is.na(values$dataset_decisions)) == 0){
                showElement("process_all")
            } else{
                hideElement("process_all")
            }
        }
    })
}
