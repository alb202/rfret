library(shiny)
library(gridExtra)
library(ggplot2)
i = 1
function(input, output) {
    source("../R/inspect_raw_data.R")
    file_list <- c("")
    reactive_data <- renderText({
        for(i in 1:length(input$input_files[,1]))
        {
            new_dataframe <- read.csv(input$input_files[i, 'datapath'],
                                  header=input$header,
                                  sep=input$sep,
                                  quote=input$quote,
                                  skip = input$skip_rows)
            comment(new_dataframe) <- input$input_files$name[i]
            print(class(new_dataframe))
            if(input$header == FALSE){
                colnames(new_dataframe) <- c("Well Row","Well Col","Content",
                                          "fret_channel","donor_channel",
                                          "acceptor_channel","concentration")
            }
            file_list <- c(file_list, new_dataframe)
        }
        output$file_list <- file_list
    })

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

}


