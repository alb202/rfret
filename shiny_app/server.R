## SERVER.R
library(ggplot2)
library(shinyjs)
library(gridExtra)
library(magrittr)
library(readr)
library(shinythemes)
source("../R/inspect_raw_data.R")
source("../R/format_data.R")
source("../R/average_technical_replicates.R")
source("../R/correct_fret_signal.R")
source("../R/fit_binding_model.R")
source("../R/make_figure.R")
source("../R/guess_parameters.R")
source("../R/equations.R")
source("graphics.R")

server <- function(input, output, session) {
    values <- reactiveValues()
    input_files <- reactive({
        if (is.null(input$data_file)) return(NULL)
        objectsLoaded <- list()
        for(i in 1:length(input$data_file$name)){
            # print("add dataset1")
            # print(input$data_file[i, 'datapath'])
            # print(input$skip_rows)
            df <- read_delim(file = input$data_file[i, 'datapath'],
                            delim = input$sep,
                            skip = as.numeric(input$skip_rows),
                            quote = input$quote
                             )
            # df <- list(input$data_file$name[i], df)
            # print("add dataset2")
            # objectsLoaded[[length(objectsLoaded)+1]] <- df
            objectsLoaded[[ input$data_file$name[i] ]] <- df
        }
        values$file_index <- 1
        values$number_of_files <- length(objectsLoaded)
        values$dataset_decisions <- rep(NA, length(objectsLoaded))
        return(objectsLoaded)
    })

    myData <- reactive({
        df<-input_files()
        if (is.null(df)) return(NULL)
        #hideElement("splash_screen")
        #showElement("raw_output")
        #showElement("accept")
        values$number_of_files <- length(df)
        updateTabsetPanel(session = session, inputId = "sidebar", selected = "advanced")
        updateTabsetPanel(session = session, inputId = "main", selected = "inspect")
        #enable(id = "next1")
        #enable(id = "previous")
        #enable(id = "accept")
        #enable(id = "reject")
        #enable(id = "accept_all")
        #if (input$skip_inspection == TRUE){
        #    values$dataset_decisions[1:length(values$dataset_decisions)] <- TRUE
        #    #input$process_all <- TRUE
        #}
        return(df)
    })
    output$filename <- renderText({
        if(is.null(input$data_file)) return(NULL)
        paste(values$file_index, "-    ", {names(myData())[[values$file_index]]})
    })

    output$raw_output <- renderPlot(height = 800, {
        if(is.null(input$data_file)) return(NULL)
        #showElement("accept")
        df <- data.frame(myData()[[values$file_index]])
        raw_data_plots <- inspect_raw_data(df)
        raw_data_grid <- grid.arrange(raw_data_plots$fret, raw_data_plots$donor, raw_data_plots$acceptor, nrow=3, ncol=1)
        return(raw_data_grid)
    })

    output$splash_screen <- renderPlot(once=TRUE, {
        #if(is.null(input$data_file)){
            #hideElement("raw_output")
            return(splash_screen())
        #} #else return(NULL)
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
    # observeEvent(input$accept_all_subsequent, label = "Accept All Subsequent", {
    #     values$dataset_decisions[values$file_index:values$number_of_files] <- TRUE
    #     View(values$dataset_decisions)
    # })

    observeEvent(input$algorithm, {
        #if(input$algorithm == "quadratic") updateCheckboxInput(session, "hill_coefficient", value = FALSE)
        #if(input$algorithm == "hyperbolic") updateCheckboxInput(session, "donor_concentration", value = "")
        toggleState(id="donor_concentration", condition = (input$algorithm == "quadratic"))
        toggleState(id="hill_coefficient", condition = (input$algorithm == "hyperbolic"))})

    observeEvent(process_all_listener(), {
        print("processing")
        updateTabsetPanel(session = session, inputId = "main", selected = "fit")
        #print(class(lapply(myData()[values$dataset_decisions], "[[", 2)))
        #print(input$data_file$datapath[values$dataset_decisions])
        #print(list(input$data_file$datapath[values$dataset_decisions]))
        #print(list(input$data_file$name[values$dataset_decisions]))
        #print(input$sep)
        #print(input$quote)
        #print(lapply(myData()[values$dataset_decisions], "[[", 2))
        #print(unlist(myData()[values$dataset_decisions]))
        #print(input$skip_rows)
        #raw_data <- format_data(input = lapply(myData()[values$dataset_decisions], "[[", 2), skip_lines = 0)
        #df_list <- myData()[values$dataset_decisions]
        df_list <- myData()[values$dataset_decisions]
        #df_names <- names(myData())[values$dataset_decisions]
        #print(df_names)
        #names(df_list) <- df_names
        print(df_list)
        print(names(df_list))
        raw_data <- format_data(input = myData()[values$dataset_decisions], skip_lines = 0)
        print(raw_data)
        #print(class(df_list))
        #print(length(df_list))
        #names(df_list) <- input$data_file$name[values$dataset_decisions]
        #print(class(df_list))
        #print(class(df_list[1]))
        #print(class(df_list[1][1]))
        #print(class(df_list[1][1][1]))
        #print(names(df_list))
        #raw_data <- format_data(input = df_list, skip_lines = 0)
        #print(raw_data)
        atr <- average_technical_replicates(raw_data = raw_data)
        print(atr)
        cfs <- correct_fret_signal(atr)
        #print(cfs)
        #params <-  guess_parameters(cfs)
        #print(params)
        #print(input$algorithm)
        #print(input$donor_concentration)
        #print(input$hill_coefficient)
        if(input$algorithm == "hyperbolic"){
            donor_concentration <- input$donor_concentration
            hill_coefficient <- FALSE
        } else{
            donor_concentration <- NULL
            hill_coefficient <- input$hill_coefficient
        }
        fbm <- fit_binding_model(data_to_fit=cfs, model=input$algorithm, donor_concentration=donor_concentration, fit_Hill_coef=hill_coefficient)
        print("fbm$fit")
        print(fbm)
        print(names(fbm))
        print(cfs)
        print(names(cfs))
        #fig <- make_figure(corrected_data = cfs, fit = fbm)
        #output$processed_output <- renderPlot({ return(splash_screen()) })
    })
    process_all_listener <- reactive({input$process_all})

    prelim_listener <- reactive({
        list(input$next1, input$previous, input$accept, input$reject, input$accept_all, input$accept_all_subsequent, input$data_file)
    })

    observeEvent(prelim_listener(), {
        print("prelim listener")
        #if(!is.null(values$dataset_decisions)){
        if(!is.null(myData())){
            print("toggles")
            #toggleElement(id = "process_all", condition = ((!is.null(input$data_file))))
            toggleState(id = "process_all", condition = (sum(is.na(values$dataset_decisions)) == 0))
            #toggleElement(id = "next1", condition = ((!is.null(input$data_file))))
            #toggleElement(id = "previous", condition = ((!is.null(input$data_file))))
            toggleState(id = "next1", condition = (values$file_index < values$number_of_files))
            toggleState(id = "previous", condition = ((values$file_index > 1)))
            toggleState(id = "accept", condition = (!is.null(input$data_file)))
            toggleState(id = "reject", condition = (!is.null(input$data_file)))
            #toggleElement(id = "accept_all_subsequent", condition = (!is.null(input$data_file)))
            #toggleElement(id = "accept_all", condition = (!is.null(input$data_file)))
            if(!is.na(values$dataset_decisions[[values$file_index]])){
                if(values$dataset_decisions[[values$file_index]] == TRUE){
                    output$decision_image <- renderImage({list(src = "accept.png",
                                                               width = 30, height = 30, contentType = 'image/png',alt = "Accept")}, deleteFile = FALSE)
                }
                if(values$dataset_decisions[[values$file_index]] == FALSE){
                    output$decision_image <- renderImage({list(src = "reject.png",
                                                               width = 30, height = 30, contentType = 'image/png',alt = "Reject")}, deleteFile = FALSE)
                }
                showElement(id = "decision_image")
            } else hideElement(id = "decision_image")
        }
    })
    #shinyApp(ui, server)
}
## Accept and reject icons are from: http://www.iconsdb.com/green-icons/checked-checkbox-icon.html, http://www.iconsdb.com/red-icons/x-mark-4-icon.html
