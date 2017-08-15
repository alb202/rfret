## SERVER.R
library(ggplot2)
library(shinyjs)
library(gridExtra)
library(grid)
library(magrittr)
library(readr)
library(shinythemes)
library(shinyFiles)
library(cowplot)
source("../R/fret_inspect_raw_data.R")
source("../R/fret_format_data.R")
source("../R/fret_average_replicates.R")
source("../R/fret_correct_signal.R")
source("../R/fit_binding_model.R")
source("../R/make_figure.R")
source("../R/guess_parameters.R")
source("../R/equations.R")
source("../R/utilities.R")
source("graphics.R")

server <- function(input, output, session) {
    values <- reactiveValues()
    values$output_dir <- NULL
    input_files <- reactive({
        toggleState(id="find_dir", condition = (input$save == TRUE))
        if (is.null(input$data_file)) return(NULL)
        # objectsLoaded <- list()
        disable(id="save")
        # for(i in 1:length(input$data_file$name)){
        #     # print("add dataset1")
        #     # print(input$data_file[i, 'datapath'])
        #     # print(input$skip_rows)
        #     df <- read_delim(file = input$data_file[i, 'datapath'],
        #                      delim = ",",
        #                      skip = as.numeric(input$skip_rows)
        #                      )
        #     # df <- list(input$data_file$name[i], df)
        #     # print("add dataset2")
        #     # objectsLoaded[[length(objectsLoaded)+1]] <- df
        #     objectsLoaded[[ input$data_file$name[i] ]] <- df
        # }
        # print(class(objectsLoaded))
        # print(str(objectsLoaded))
        filenames <- input$data_file$datapath
        names(filenames) <- input$data_file$name
        print("show input data")
        print(input$data_file)
        print(filenames)
        ffd <- fret_format_data(input = filenames, skip_lines = 4)
        values$ffd <- ffd
        #print(abc)
        # print("input$data_file")
        # print(input$data_file)
        values$file_index <- 1
        values$number_of_files <- length(filenames)
        print("number of files: ")
        print(values$number_of_files)
        isolate(values$dataset_decisions <- rep(NA, values$number_of_files))
        # values$dataset_names <- sort(sub(names(objectsLoaded),
        #                                  pattern = ".csv",
        #                                  replacement = ""))
        print("dataset names")
        print(unique(x = ffd$Experiment))
        isolate(values$dataset_names <- sort(unique(x = ffd$Experiment)))
        #print(objectsLoaded[[1]])
        # print(length(objectsLoaded))
        # print(names(objectsLoaded))
        # print(class(objectsLoaded[1]))
        # print(class(objectsLoaded[[1]]))

        # If an output directory has not been set, make it null
        print("output dir")
        #print(values)
        print(is.null(isolate(values$output_dir)))
        # print(values[[1]])
        # print(class(values))
        # print(str(values))
        print(isolate(values$output_dir))
        # print(("output_dir" %in% values)==FALSE)
        # print(input$save==FALSE)
        # print(!("output_dir" %in% values) | input$save==FALSE)
        if(input$save==FALSE)
            isolate(values$output_dir <- NULL)
        if(input$save==TRUE & !is.null(isolate(values$output_dir))){
            if(!isTRUE(dir.exists(paths = isolate(values$output_dir))))
                dir.create(path = isolate(values$output_dir))
        }
        # Use the RFRET function to format the data frames
        #ffd <- fret_format_data(input = objectsLoaded)

        updateTabsetPanel(session = session, inputId = "sidebar", selected = "advanced")
        updateTabsetPanel(session = session, inputId = "main", selected = "inspect")
        showElement(id = "decision_indicator")
        showElement(id = "file_selector")
        if (is.null(ffd)) return(NULL)
        ird <- fret_inspect_raw_data(raw_data = ffd,
                                     plot_format = "png",
                                     output_directory = isolate(values$output_dir))

        #values$number_of_files <- length(ird)
        return(ird)
        # return(ffd)
    })

    myData <- reactive({
        # ffd <-input_files()
        # if (is.null(ffd)) return(NULL)
        # ird <- fret_inspect_raw_data(raw_data = ffd,
        #                              plot_format = "png",
        #                              output_directory = isolate(values$output_dir))
        ird <- input_files()
        print("myData")
        #hideElement("splash_screen")
        #showElement("raw_output")
        #showElement("accept")
        # values$number_of_files <- length(ird)
        # updateTabsetPanel(session = session, inputId = "sidebar", selected = "advanced")
        # updateTabsetPanel(session = session, inputId = "main", selected = "inspect")
        # showElement(id = "decision_indicator")
        # showElement(id = "file_selector")
        #enable(id = "next1")
        #enable(id = "previous")
        #enable(id = "accept")
        #enable(id = "reject")
        #enable(id = "accept_all")
        #if (input$skip_inspection == TRUE){
        #    values$dataset_decisions[1:length(values$dataset_decisions)] <- TRUE
        #    #input$process_all <- TRUE
        #}
        return(ird)
    })

    # output$filename <- renderText({
    #     if(is.null(input$data_file)) return(NULL)
    #         return(paste(values$file_index, " : ", values$dataset_names[[values$file_index]]))
    # })

    output$raw_output <- renderPlot(height = 800, {
        if(is.null(input$data_file)) return(NULL)
        print("render the inspection plots")
        #showElement("accept")
        #df <- data.frame(myData()[[values$file_index]])
        raw_data_plots <- myData()[[values$file_index]]
        raw_data_grid <- grid.arrange(raw_data_plots$fret, raw_data_plots$donor, raw_data_plots$acceptor, nrow=3, ncol=1)
        return(raw_data_grid)
    })

    output$splash_screen <- renderPlot(bg = "transparent",
                                       #once=TRUE,
                                       {
        return(splash_screen())
    })

    #output$decision_indicator <- renderPlot(once=FALSE, width=50, height=(length(isolate(values$dataset_decisions))*35), bg = "transparent", {      #width=50,
        #return(decision_indicator(decision_index = values$dataset_decisions, position_index = values$file_index))
        #if(is.null(input$data_file)) return(NULL)
    observeEvent(decision_listener(), {
        if(is.null(values$file_index)) return(NULL)
        print("decision indicator")
        dataset_decisions <- isolate(values$dataset_decisions)
        number_of_datasets <- isolate(values$number_of_files)
        #print(number_of_datasets*35)
        #print(dataset_decisions)
        #print(1:number_of_datasets)
        #print(class(isolate(values$dataset_decisions)))
        #print(class(1:length(isolate(values$dataset_decisions))))
        #print(isolate(values$file_index))

        # selected <- rep(FALSE, number_of_datasets)
        # selected[isolate(values$file_index)] <- TRUE
        # print(selected)
        p <- do.call(grid.arrange,
                     c(mapply(FUN = make_indicator,
                              dataset_decisions,
                              1:number_of_datasets,
                              #selected,
                              SIMPLIFY = FALSE,
                              USE.NAMES = FALSE),
                       ncol = 1))
        output$decision_indicator <- renderPlot(
            #once=FALSE,
            width=30,
            height=(length(isolate(values$dataset_decisions))*30),
            bg = "transparent",
            {grid.draw(p)}
            )
        #return(decision_indicator(decision_index = values$dataset_decisions, position_index = values$file_index))
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

    observeEvent(input$save, label = "saveFiles", {
        toggleState(id="find_dir", condition = (input$save == TRUE))
    })

    observeEvent(input$algorithm, {
        #if(input$algorithm == "quadratic") updateCheckboxInput(session, "hill_coefficient", value = FALSE)
        #if(input$algorithm == "hyperbolic") updateCheckboxInput(session, "donor_concentration", value = "")
        toggleState(id="donor_concentration", condition = (input$algorithm == "quadratic"))
        toggleState(id="hill_coefficient", condition = (input$algorithm == "hyperbolic"))})

    output$save_dir <- renderText({
        #volumes <- c("UserFolder"="/home/ab")
        #saveDir <- parseDirPath(volumes, input$find_dir)
        return(values$save_dir)
    })

    observeEvent(input$file_index, {
        print(input$file_index)
        values$file_index <- as.numeric(input$file_index)
    })

    output$file_selector <- renderUI({
        options <- 1:values$number_of_files
        radioButtons(width = "10%",
                     inputId = 'file_index',
                     label = NULL,
                     choiceValues = options,
                     choiceNames = rep("", values$number_of_files),
                     selected = values$file_index)
    })

    observe({
        volumes <- c("Home"="~/", "Root"="/")
        shinyDirChoose(input, "find_dir", roots=volumes, session=session)
        save_dir <- parseDirPath(volumes, input$find_dir)
        values$save_dir <- save_dir
        timestamp <- strsplit(x = as.character(Sys.time()), split = " ")
        output_dir <- paste(save_dir, "/", timestamp[[1]][1], "_", timestamp[[1]][2], sep = "")
        # if(!isTRUE(dir.exists(paths = output_dir)))
        #     dir.create(path = output_dir)
        isolate(values$output_dir <- output_dir)
        print(isolate(values$save_dir))
        print(isolate(values$output_dir))
        #output$dots <- renderImage(deleteFile = FALSE, {src = list("accept.png", contentType = 'image/png')})
        #output$dots <- renderImage({list(src = "red-dot.svg",
        #                                           width = 30, height = 30, contentType = 'image/svg',alt = "Accept")}, deleteFile = FALSE)
        #output$dots <- renderImage({ grid.arrange("red-dot.svg")})
    })

observeEvent(process_all_listener(), {
        print("processing")
        if(input$algorithm=="quadratic")
            req(input$donor_concentration)
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
        print("input_files")
        print(input$data_file[,"name"])
        #ffd <- input_files()
        ffd <- values$ffd
        print(values$dataset_names)
        print(values$dataset_decisions)
        print(values$dataset_names[values$dataset_decisions])
        print("ffd 1")
        print(isolate(values$dataset_names)[isolate(values$dataset_decisions)])
        print(ffd)
        #print(input$data_file[,"name"][values$dataset_decisions])
        #ffd <- ffd[ffd$Experiment %in% values$dataset_names[values$dataset_decisions], ]
        #ffd <- ffd[ffd$Experiment %in% input$data_file[,"name"][values$dataset_decisions], ]
        ffd <- ffd[ffd$Experiment %in% isolate(values$dataset_names)[values$dataset_decisions], ]
        print("ffd 2")
        print(ffd)
        print(class(input$donor_concentration))
        if(isTRUE(input$hill_coefficient) && input$algorithm!="quadratic"){
            binding_model <- "hill"
            print("binding is hill")
        } else{
            binding_model <- input$algorithm
            print("binding is not hill")
        }
        figures <- ffd %>%
            fret_average_replicates() %>%
            fret_correct_signal(output_directory = isolate(values$output_dir)) %>%
            fit_binding_model(binding_model = binding_model,
                              probe_concentration = as.numeric(input$donor_concentration),
                              output_directory = isolate(values$output_dir)) %>%
            make_figure(probe_concentration = as.numeric(input$donor_concentration),
                        output_directory = isolate(values$output_dir),
                        plot_format = "png")

        # print(class(figure))
        # print(class(figure[[1]]))
        # print(class(figure[1]))
        # print(length(figure))
        # #df_names <- names(myData())[values$dataset_decisions]
        #print(df_names)
        #names(df_list) <- df_names
        #print(ffd)
        #print(names(ffd))
        print("figures")
        print(class(figures))
        print(length(figures))
        print(figures[[1]])
        print(class(figures[[1]]))
        figures <- lapply(X = figures,
                          FUN = function(X) X +
                              theme(plot.margin=unit(c(.5,.05,.5,.05),"cm"),
                                    panel.border = element_rect(fill = NA,
                                                                colour = "gray40",
                                                                linetype = 1,
                                                                size = 1)))
        figure_grid <- do.call(grid.arrange, c(figures, ncol=1, nrow=length(figures)))
        output$processed_output <- renderPlot(height = 400*length(figures), {grid.draw(figure_grid)})

    })
    process_all_listener <- reactive({input$process_all})

    decision_listener <- reactive({list(values$dataset_decisions, values$file_index)})

    prelim_listener <- reactive({
        list(input$next1, input$previous, input$accept, input$reject, input$accept_all, input$accept_all_subsequent, input$data_file)
    })

    observeEvent(prelim_listener(), {
        print("prelim listener")
        #if(!is.null(values$dataset_decisions)){
        if(!is.null(myData())){
            print("toggles")
            #toggleElement(id = "process_all", condition = ((!is.null(input$data_file))))
            toggleState(id = "process_all", condition = (sum(is.na(values$dataset_decisions)) == 0 &
                                                             (sum(values$dataset_decisions==TRUE)>0)))
            #toggleElement(id = "next1", condition = ((!is.null(input$data_file))))
            #toggleElement(id = "previous", condition = ((!is.null(input$data_file))))
            toggleState(id = "next1", condition = (values$file_index < values$number_of_files))
            toggleState(id = "previous", condition = ((values$file_index > 1)))
            toggleState(id = "accept", condition = (!is.null(input$data_file)))
            toggleState(id = "reject", condition = (!is.null(input$data_file)))
            toggleState(id = "accept_all", condition = (!is.null(input$data_file)))
            #toggleElement(id = "accept_all", condition = (!is.null(input$data_file)))
            # if(!is.na(values$dataset_decisions[[values$file_index]])){
            #     if(values$dataset_decisions[[values$file_index]] == TRUE){
            #         output$decision_image <- renderImage({list(src = "green_check.png",
            #                                                    width = 30, height = 30, contentType = 'image/png',alt = "Accept")}, deleteFile = FALSE)
            #     }
            #     if(values$dataset_decisions[[values$file_index]] == FALSE){
            #         output$decision_image <- renderImage({list(src = "red_x.png",
            #                                                    width = 30, height = 30, contentType = 'image/png',alt = "Reject")}, deleteFile = FALSE)
            #     }
            # } else {
            #     output$decision_image <- renderImage({list(src = "yellow_question.png",
            #                                                width = 30, height = 30, contentType = 'image/png',alt = "Uninspected")}, deleteFile = FALSE)
            # }
        }
        # showElement(id = "decision_image")
    })
}
## Icons from Font-Awesome (http://fontawesome.io/icons/) and Bootstrap/Glyphicon (https://getbootstrap.com/docs/3.3/components/#glyphicons)
