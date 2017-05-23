library(shiny)

function(input, output) {
    output$contents <- renderTable({

        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.

        #inFile <- input$input_files

        if (is.null(input$input_files))
            return(NULL)
        file_list <- c("")
        for(i in 1:length(input$input_files[,1]))
        {
            #lst[[i]] <- read.csv(input$file1[[i, 'datapath']])
            #lst[i] <- read.csv(input$file1[[i, 'datapath']])
            #print(colnames(read.csv(input$file1[i, 'datapath'])))
            print(read.csv(input$input_files[i, 'datapath'],
                           header=input$header,
                           sep=input$sep,
                           quote=input$quote,
                           skip = 4)
                  )
            print(input$input_files$name[i])
            new_row <- c(input$input_files$name[i],
                         read.csv(input$input_files[i, 'datapath'],
                                  header=input$header,
                                  sep=input$sep,
                                  quote=input$quote,
                                  skip = 4))
            file_list <- c(file_list, new_row)
        }
    })
}
