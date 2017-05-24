## SERVER.R
library(ggplot2)
library(gridExtra)
source("/media/ab/HD4/rfret/R/inspect_raw_data.R")

function(input, output) {
    infile <- reactive({
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
            df <- list(input$data_file$name, df)
            objectsLoaded[[length(objectsLoaded)+1]] <- df
        }
        return(objectsLoaded)
    })

    myData <- reactive({
        df<-infile()
        if (is.null(df)) return(NULL)
        return(df)
    })
    output$filename <-renderText({myData()[[1]][[1]]})
    output$raw_output <- renderPlot({
        if(is.null(input$data_file)) return(print("Please upload some FRET data"))
        df <- data.frame(myData()[[1]][[2]])
        raw_data_plots <- inspect_raw_data(df)
        raw_data_grid <- grid.arrange(raw_data_plots$donor, raw_data_plots$acceptor, raw_data_plots$fret, ncol=2, nrow=3)
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
