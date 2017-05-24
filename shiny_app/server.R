## SERVER.R
library(ggplot2)
library(gridExtra)
source("/media/ab/HD4/rfret/R/inspect_raw_data.R")

function(input, output) {
    #file_index = 1
    values <- reactiveValues()
    values$file_index <- 1
    values$dataset_decisions <- list()
    input_files <- reactive({
        if (is.null(input$data_file)) {
            # User has not uploaded a file yet
            return(NULL)
        }
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
        return(objectsLoaded)
    })

    myData <- reactive({
        df<-input_files()
        if (is.null(df)) return(NULL)
        return(df)
    })
<<<<<<< HEAD
    #values$number_of_files <- length(myData())
=======
>>>>>>> 39011c707583c8b14a60d0c85fd03ca93b2fe268
    output$filename <-renderText({myData()[[values$file_index]][[1]]})
    output$raw_output <- renderPlot({
        values$number_of_files <- length(myData())
        if(is.null(input$data_file)) return(print("Please upload some FRET data"))
        df <- data.frame(myData()[[values$file_index]][[2]])
        raw_data_plots <- inspect_raw_data(df)
        raw_data_grid <- grid.arrange(raw_data_plots$donor, raw_data_plots$acceptor, raw_data_plots$fret, ncol=2, nrow=3)
    })
<<<<<<< HEAD
    observeEvent(input$accept, label = "Accept", {
        cat("accept ", values$file_index, "\n")
        values$dataset_decisions[[length(values$dataset_decisions) + 1]] <- TRUE
        values$file_index <- values$file_index + 1
        View(values$dataset_decisions)
    })
    observeEvent(input$remove, label = "Remove", {
        cat("remove ", values$file_index, "\n")
        values$dataset_decisions[[length(values$dataset_decisions) + 1]] <- FALSE
        values$file_index <- values$file_index + 1
        View(values$dataset_decisions)
    })
    observeEvent(input$accept_all, label = "Accept All", {
        cat("Accept All", values$file_index, "\n")
        values$dataset_decisions[values$file_index:values$number_of_files] <- TRUE
        values$file_index <- values$number_of_files
=======
    file_index <- observeEvent(input$accept, label = "Accept", {
        values$file_index <- values$file_index + 1
        cat("accept ", values$file_index, "\n")
        values$dataset_decisions[[length(values$dataset_decisions) + 1]] <- TRUE
        View(values$dataset_decisions)
    })

    file_index <- observeEvent(input$remove, label = "Remove", {
        values$file_index <- values$file_index + 1
        cat("remove ", values$file_index, "\n")
        values$dataset_decisions[[length(values$dataset_decisions) + 1]] <- FALSE
>>>>>>> 39011c707583c8b14a60d0c85fd03ca93b2fe268
        View(values$dataset_decisions)
    })
}

    #output$

    # output$plotgraph <- renderPlot({
    #     raw_plots <- inspect_raw_data(new_data$file_list[1])
    #     print("test")
    #     fig1 <- raw_plots$donor
    #     fig2 <- raw_plots$acceptor
    #     fig3 <- raw_plots$fret
    #     fig_list <- list(fig1,fig2,fig3)
    #     to_delete <- !sapply(fig_list,is.null)
    #     fig_list <- fig_list[to_delete]
    #     if (length(fig_list)==0){
    #         return(NULL)
    #     }
    #     print(fig1)
        #grid.arrange(grobs=fig_list,ncol=length(fig_list))
    #})
    #print("test444")

    # output$plotgraph <- renderPlot({
    #     raw_list <- reactive_data[1]
    #     raw_plots <- inspect_raw_data(raw_list)
    #     fig1 <- reactive({raw_plots$donor})
    #     fig2 <- reactive({raw_plots$acceptor})
    #     fig3 <- reactive({raw_plots$fret})
    #     fig_list <- list(fig1(),fig2(),fig3())
    #     to_delete <- !sapply(fig_list,is.null)
    #     fig_list <- fig_list[to_delete]
    #     if (length(ptlist)==0){
    #         return(NULL)
    #     }
    # grid.arrange(grobs=ptlist,ncol=length(ptlist))
