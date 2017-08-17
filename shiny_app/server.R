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

    # Initialize Values properties
    values <- reactiveValues()
    values$output_dir <- NULL
    values$ffd <- NULL
    values$inspect_index <- NULL
    values$number_of_files <- NULL
    values$dataset_decisions <- NULL
    values$dataset_names <- NULL
    values$save_dir <- NULL
    values$results_index <- NULL
    values$volumes <- c("Home"="~/", "Root"="/")

    input_files <- reactive({
        # If the "Save..." option is checked, make the directory button active
        toggleState(id="find_dir", condition = (input$save == TRUE))
        # If no files have been selected, end here
        if (is.null(input$data_file)) return(NULL)

        ## After files have been selected, run these
        # Disable the "Save ... " option
        disable(id="save")

        # Start the progress bar here
        withProgress(message = 'Working: ', value = 0, detail = "Formatting data ...", {
            # Get the original names of the files
            filenames <- input$data_file$datapath
            names(filenames) <- input$data_file$name

            # Format the data of the opened files and save as "values$ffd"
            ffd <- fret_format_data(input = filenames, skip_lines = 4)
            values$ffd <- ffd

            # Set the index for inspecting the files and the count of files
            values$inspect_index <- 1
            values$number_of_files <- length(filenames)

            # Print some info
            print("number of files: ")
            print(isolate(values$number_of_files))

            # Set the decision for each file to NA
            isolate(values$dataset_decisions <- rep(NA, values$number_of_files))

            # Print the dataset names from ffd
            print("dataset names")
            print(unique(x = ffd$Experiment))

            # Get the unique sorted list of dataset names from ffd
            isolate(values$dataset_names <- sort(unique(x = ffd$Experiment)))

            # If an output directory has been set, create it
            print("output dir")
            print(isolate(values$output_dir))
            if(isTRUE(input$save) && !is.null((values$output_dir))){
                print("creating new directory")
                if(!isTRUE(dir.exists(paths = isolate(values$output_dir))))
                    dir.create(path = isolate(values$output_dir))
            }
            # Update progress
            incProgress(.5, detail = "Generating figures ...")

            # If for any reason ffd does not exist, stop
            if (is.null(ffd)) return(NULL)

            # Inspect the raw data with rfret
            ird <- fret_inspect_raw_data(raw_data = ffd,
                                         plot_format = "png",
                                         output_directory = isolate(values$output_dir))

            # Update progress
            incProgress(1, detail = "Displaying figures ...")

            # Swith the tabs and enable the accept/reject buttons
            updateTabsetPanel(session = session, inputId = "sidebar", selected = "advanced")
            updateTabsetPanel(session = session, inputId = "main", selected = "inspect")
            showElement(id = "decision_indicator")
            showElement(id = "file_selector")
        })

        # Return the ird object as input_files()
        return(ird)
    })

    inspection_data <- reactive({
        ## This places the formatted inspection data into an object so you don't rerun the input function every time it needs to be accessed
        print("inspection_data")
        # Return the input_files() data, which is just a copy of the ird object
        return(input_files())
    })

    results_data <- reactive({
        ## This places the formatted results data into an object so you don't rerun the processing function every time it needs to be accessed
        print("results_data")
        return(results_data())
    })

    output$splash_screen <- renderPlot(bg = "transparent",{
        # Run the splach_screen function and render the plot
        return(splash_screen())
    })

    observeEvent(decision_listener(), {
        # If an index hasn't been set, stop here
        if(is.null(values$inspect_index)) return(NULL)
        print("decision indicator")

        # Generate the decision indicator by arranging the figures in a grid
        p <- do.call(grid.arrange,
                     c(mapply(FUN = make_indicator,
                              isolate(values$dataset_decisions),
                              1:isolate(values$number_of_files),
                              SIMPLIFY = FALSE,
                              USE.NAMES = FALSE),
                       ncol = 1))

        # Render the decision indicator
        output$decision_indicator <- renderPlot(
            width=30,
            height=(isolate(values$number_of_files)*30),
            bg = "transparent",
            {grid.draw(p)})
    })

    ## This is for the left Data tab
    observeEvent(input$save, label = "saveFiles", {
        # If the "Save files" button is checked, enable the directory finder
        toggleState(id="find_dir", condition = (input$save == TRUE))
    })

    ## These two are for the inspection page
    observeEvent(input$accept, label = "Accept", {
        # Set the decision as Accept, then increment if not at the end of files
        values$dataset_decisions[[values$inspect_index]] <- TRUE
        if(values$inspect_index < values$number_of_files) values$inspect_index <- values$inspect_index + 1
    })
    observeEvent(input$reject, label = "Reject", {
        # Set the decision as Reject, then increment if not at the end of files
        values$dataset_decisions[[values$inspect_index]] <- FALSE
        if(values$inspect_index < values$number_of_files) values$inspect_index <- values$inspect_index + 1
    })
    output$file_selector <- renderUI({
        # Create the radio buttons for selecting a dataset to inspect
        radioButtons(width = "10%",
                     inputId = 'inspect_index',
                     label = NULL,
                     choiceValues = 1:isolate(values$number_of_files),
                     choiceNames = rep("", isolate(values$number_of_files)),
                     selected = values$inspect_index)
    })

    ## These two are for the results page
    observeEvent(input$previous, label = "Previous", {
        # Increment the results page if not at the end of files
        if(values$results_index > 1) values$results_index <- values$results_index - 1
    })
    observeEvent(input$next1, label = "Next", {
        # Increment the results page if not at the end of files
        if(values$results_index < values$number_of_files) values$results_index <- values$results_index + 1
    })
    ## These are for the left "inspect" tab
    observeEvent(input$accept_all, label = "Accept All", {
        # If the "accept all" button is clicked, set the decision for all files to TRUE
        values$dataset_decisions[1:values$number_of_files] <- TRUE
    })
    observeEvent(input$algorithm, {
        # Enable the hill coefficient or donor concentration option depending on the algorithm
        toggleState(id="donor_concentration", condition = (input$algorithm == "quadratic"))
        toggleState(id="hill_coefficient", condition = (input$algorithm == "hyperbolic"))})

    ## These are background processes
    output$save_dir <- renderText({
        # If a save directory is selected, display the name of the save directory
        return(values$save_dir)
    })
    observeEvent(input$inspect_index, {
        # Set the inspection index to the number of the radio button selected
        values$inspect_index <- as.numeric(input$inspect_index)
    })

    ## Save the output files to a directory
    observe({
        print("getting directory")

        # If the save checkbox isn't selected, stop here
        if(!isTRUE(input$save)) return(NULL)

        # When the directory button is pressed, these run
        shinyDirChoose(input, "find_dir", roots=isolate(values$volumes), session=session)

        # Parse the result and save the output directory
        values$save_dir <- parseDirPath(values$volumes, input$find_dir)

        # Get the current time
        timestamp <- strsplit(x = as.character(Sys.time()), split = " ")

        # Make the full output directory with timestamp
        values$output_dir <- paste(values$save_dir, "/", timestamp[[1]][1], "_", timestamp[[1]][2], sep = "")

        # Print the output directories
        print(isolate(values$save_dir))
        print(isolate(values$output_dir))
    })

    # Update the inspection tab when a change is made
    observeEvent(prelim_listener(), {
        print("prelim listener")
        # If there is data to inspect, enable to correct buttons on the inspection tab and display the graph
        if(!is.null(inspection_data())){
            toggleState(id = "process_all", condition = (sum(is.na(values$dataset_decisions)) == 0 &
                                                             (sum(values$dataset_decisions==TRUE)>0)))
            toggleState(id = "accept", condition = (!is.null(input$data_file)))
            toggleState(id = "reject", condition = (!is.null(input$data_file)))
            toggleState(id = "accept_all", condition = (!is.null(input$data_file)))

            # Render the inspection figure
            output$raw_output <- renderPlot(height = 800, {
                if(is.null(input$data_file)) return(NULL)
                print("render the inspection plots")
                # Select the current data set and arrange the plots in a grid
                inspection_plots <- inspection_data()[[values$inspect_index]]
                return(grid.arrange(inspection_plots$fret,
                                    inspection_plots$donor,
                                    inspection_plots$acceptor,
                                    nrow=3,
                                    ncol=1))
            })
        }
    })



    ## Actions that occur when processing selected files
    observeEvent(process_all_listener(), {
        print("processing")

        # If the algorithm is quadratic, the donor concentration must be set. If it is NA, a warning will display and processing will not occur
        if(input$algorithm=="quadratic")
            if(is.na(input$donor_concentration)){
                # Send the warning
                output$conc_required <- renderText({"Enter a donor concentration or change the algorithm!"})
                toggle(id = "conc_required")
            }
            # Do not allow anything to continue
            req(input$donor_concentration)
        updateTabsetPanel(session = session, inputId = "main", selected = "fit")
        #print(class(lapply(inspection_data()[values$dataset_decisions], "[[", 2)))
        #print(input$data_file$datapath[values$dataset_decisions])
        #print(list(input$data_file$datapath[values$dataset_decisions]))
        #print(list(input$data_file$name[values$dataset_decisions]))
        #print(input$sep)
        #print(input$quote)
        #print(lapply(inspection_data()[values$dataset_decisions], "[[", 2))
        #print(unlist(inspection_data()[values$dataset_decisions]))
        #print(input$skip_rows)
        #raw_data <- format_data(input = lapply(inspection_data()[values$dataset_decisions], "[[", 2), skip_lines = 0)
        #df_list <- inspection_data()[values$dataset_decisions]
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

        withProgress(message = 'Working: ', value = 0, min = 0, max = 1, detail = "Fitting binding model ...", {
            #incProgress(0, detail = )
            fbm <- ffd %>%
                fret_average_replicates() %>%
                fret_correct_signal(output_directory = isolate(values$output_dir)) %>%
                fit_binding_model(binding_model = binding_model,
                                  probe_concentration = as.numeric(input$donor_concentration),
                                  output_directory = isolate(values$output_dir))
            figures <- fbm %>% make_figure(probe_concentration = as.numeric(input$donor_concentration),
                                           output_directory = isolate(values$output_dir),
                                           plot_format = "png")
            incProgress(.3, detail = "Calculating kd values ...")

            # print(class(figure))
            # print(class(figure[[1]]))
            # print(class(figure[1]))
            # print(length(figure))
            print("fit_summaries")
            #z <- data.frame(summary(fbm[[1]])[["coefficients"]])
            #print(z)
            y <- lapply(X = fbm, FUN = function(x) summary(x)[["coefficients"]])
            print(melt(y))

            # #df_names <- names(inspection_data())[values$dataset_decisions]
            #print(df_names)
            #names(df_list) <- df_names
            #print(ffd)
            #print(names(ffd))
            print("figures")
            # print(class(figures))
            # print(length(figures))
            # print(figures[[1]])
            # print(class(figures[[1]]))
            values$results_index <- 1
            incProgress(.6, detail = "Generating figures ...")
            figures <- lapply(X = figures,
                              FUN = function(X) X +
                                  theme(plot.margin=unit(c(0,0,0,0),"cm"),
                                        panel.border = element_rect(fill = NA,
                                                                    colour = "gray40",
                                                                    linetype = 1,
                                                                    size = 0)))
            #figure_grid <- do.call(grid.arrange, c(figures, ncol=1, nrow=length(figures)))
            output$processed_output <- renderPlot(height = 400*length(1), {grid.draw(figures[[1]])})
            incProgress(1, detail = "Displaying figures ...")
            #return(output$processed_output)
        })
    })
    process_all_listener <- reactive({input$process_all})
    decision_listener <- reactive({list(values$dataset_decisions, values$inspect_index)})
    prelim_listener <- reactive({list(input$accept, input$reject, input$accept_all, input$data_file)})
    results_listener <- reactive({list(input$next1, input$previous)})

    observeEvent(results_listener(), {
        print("results listener")
        if(!is.null(input$data_file)){
            toggleState(id = "next1", condition = (values$results_index < values$number_of_files))
            toggleState(id = "previous", condition = ((values$results_index > 1)))
        }
    })

}
## Icons from Font-Awesome (http://fontawesome.io/icons/) and Bootstrap/Glyphicon (https://getbootstrap.com/docs/3.3/components/#glyphicons)
