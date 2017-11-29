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
library(rlang)
library(jsonlite)
library(devtools)
#library(DT)
source("../R/rfret-package.R")
source("../R/fret_inspect_raw_data.R")
source("../R/fret_format_data.R")
source("../R/fret_average_replicates.R")
source("../R/fret_correct_signal.R")
source("../R/fit_binding_model.R")
source("../R/make_figure.R")
source("../R/guess_parameters.R")
source("../R/equations.R")
#source("../R/utilities.R")
source("../R/load_data.R")
source("../R/plan_experiment.R")
source("graphics.R")
source("../R/fret_job_plot.R")
source("../R/on_attach.R")
source("../R/get_user_metadata.R")
source("environment.R")
#source("../data-raw/make_default_metadata.R")
#source("R/equations.R")
#source("R/rfret-package.R")
#source("R/datasets.R")
#source("R/make_figure.R")
#source("R/fret_average_replicates.R")
#source("R/fret_correct_signal.R")
#source("R/fret_format_data.R")
#source("R/fret_inspect_raw_data.R")
#source("R/fret_job_plot.R")
#source("R/fit_binding_model.R")
#source("R/guess_parameters.R")
#source("R/load_data.R")
#source("R/on_attach.R")
#source("R/plan_experiment.R")

server <- function(input, output, session) {

    # Initialize Values properties
    values <- reactiveValues()
    values$output_dir <- NULL
    values$ffd <- NULL
    values$inspect_index <- NULL
    values$number_of_files <- NULL
    values$dataset_decisions <- NULL
    values$dataset_names <- NULL
    values$raw_output <- list()
    values$save_dir <- NULL
    values$custom_headers <- NULL
    values$results_index <- NULL
    values$volumes <- c("Home"="~/", "Root"="/")

    input_files <- reactive({
        # If the "Save..." option is checked, make the directory button active
        toggleState(id="find_dir", condition = (input$save == TRUE))
        toggleState(id="find_headers", condition = (input$headers == TRUE))
        # If no files have been selected, end here
        if (is.null(input$data_file)) return(NULL)

        ## After files have been selected, run these
        # Disable the "Save ... "  and "Custom headers" options
        disable(id="save")
        disable(id="headers")

        # Create the metadata environment
        print("enviro class")
        .rfret <- create_environment()
        print(class(.rfret))
            #metadata_json = "../data-raw/default_metadata.json")
       # if(input$headers == TRUE)
    #        get_user_metadata(user_json_file = isolate(values$custom_headers))
        #print(.rfret$metadata)
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
            isolate(values$raw_output <- as.list(rep(NA, values$number_of_files)))

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

            # If for any reason ffd does not exist, stop
            if (is.null(ffd)) return(NULL)

            # Inspect the raw data with rfret
            ird <- fret_inspect_raw_data(raw_data = ffd,
                                         plot_format = "png",
                                         output_directory = isolate(values$output_dir))



            # Swith the tabs and enable the accept/reject buttons
            updateTabsetPanel(session = session, inputId = "sidebar", selected = "advanced")
            updateTabsetPanel(session = session, inputId = "main", selected = "inspect")
            showElement(id = "decision_indicator")
            showElement(id = "file_selector")

            # Update progress
            incProgress(.5, detail = "Generating figures ...")

            # Generating inspection plots and saving them for viewing
            inspection_plots <- lapply(X = ird,
                                       FUN = function(i) {
                                           grid.arrange(i$fret,
                                                        i$donor,
                                                        i$acceptor,
                                                        nrow=3,
                                                        ncol=1)})
            # Update progress
            incProgress(1, detail = "Displaying figures ...")
        })

        return(inspection_plots)
    })

    inspection_data <- reactive({
        ## This places the formatted inspection data into an object so you don't rerun the input function every time it needs to be accessed
        print("inspection_data")
        # Return the input_files() data, which is all the inspection plots as a grid
        return(input_files())
    })

    results_figures <- reactive({
        ## This places the formatted results data into an object so you don't rerun the processing function every time it needs to be accessed
        print("results_data")
        return(values$results_data)
    })

    output$splash_screen <- renderPlot(bg = "transparent",{
        # Run the splach_screen function and render the plot
        return(splash_screen())
    })

    observeEvent(decision_listener(), {
        # If an index hasn't been set, stop here
        if(is.null(values$inspect_index)) return(NULL)
        print("decision indicator")
        #if(TRUE) return(NULL)
        # Generate the decision indicator by arranging the figures in a grid
        # p <- do.call(grid.arrange,
        #              c(mapply(FUN = make_indicator,
        #                       isolate(values$dataset_decisions),
        #                       1:isolate(values$number_of_files),
        #                       SIMPLIFY = FALSE,
        #                       USE.NAMES = FALSE),
        #                ncol = 1))

        # Render the decision indicator
        # output$decision_indicator <- renderPlot(
        #     width=30,
        #     height=(isolate(values$number_of_files)*30),
        #     bg = "transparent",
        #     {grid.draw(p)})
    })

    ## This is for the Save Folder button
    observeEvent(input$save, label = "saveFiles", {
        # If the "Save files" button is checked, enable the directory finder
        toggleState(id="find_dir", condition = (input$save == TRUE))
    })

    ## This is for the Custom Headers button
    observeEvent(input$headers, label = "customHeaders", {
        # If the "Save files" button is checked, enable the directory finder
        toggleState(id="find_headers", condition = (input$headers == TRUE))
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
        if(values$results_index < values$number_of_positive_decisions) values$results_index <- values$results_index + 1
    })

    output$results_selector <- renderUI({
        # Create the radio buttons for selecting results
        print("run results selector")
        radioButtons(width = "10%",
                     inputId = 'results_index',
                     label = NULL,
                     choiceValues = 1:isolate(values$number_of_positive_decisions),
                     choiceNames = rep("", isolate(values$number_of_positive_decisions)),
                     selected = values$results_index)
    })

    ## These are for the left "inspect" tab
    observeEvent(input$accept_all, label = "Accept All", {
        # If the "accept all" button is clicked, set the decision for all files to TRUE
        values$dataset_decisions[1:values$number_of_files] <- TRUE
    })
    observeEvent(input$algorithm, {
        # Enable the hill coefficient or donor concentration option depending on the algorithm
        toggleState(id="donor_concentration", condition = (input$algorithm == "quadratic"))
        #toggleState(id="hill_coefficient", condition = (input$algorithm == "hyperbolic"))
    })

    ## These are background processes
    output$save_dir <- renderText({
        # If a save directory is selected, display the name of the save directory
        print("render save dir")
        print(values$save_dir)
        return(values$save_dir)
    })

    output$header_file <- renderText({
        # If a custom header is selected, display the name of the header file
        print("custom headers print")
        print(values$custom_headers)
        return(values$custom_headers)
    })

    observeEvent(input$inspect_index, {
        # Set the inspection index to the number of the radio button selected
        values$inspect_index <- as.numeric(input$inspect_index)
    })
    observeEvent(input$results_index, {
        # Set the inspection index to the number of the radio button selected
        values$results_index <- as.numeric(input$results_index)
    })

    ## Save the output files to a directory
    observe({
        print("getting directory")

        # If the save checkbox isn't selected, stop here
        if(!isTRUE(input$save)) return(NULL)

        # When the directory button is pressed, these run
        shinyDirChoose(input, "find_dir", roots=isolate(values$volumes), session=session)

        # Parse the result and save the output directory
        values$save_dir <- parseDirPath(roots = values$volumes, selection = input$find_dir)

        # Get the current time
        timestamp <- strsplit(x = as.character(Sys.time()), split = " ")

        # Make the full output directory with timestamp
        values$output_dir <- paste(values$save_dir, "/", timestamp[[1]][1], "_", timestamp[[1]][2], sep = "")

        # Print the output directories
        print(isolate(values$save_dir))
        print(isolate(values$output_dir))
    })

    ## Get the location of custom headers
    observe({
        print("getting custom headers")

        # If the save checkbox isn't selected, stop here
        if(!isTRUE(input$headers)) return(NULL)
        #
        # # When the directory button is pressed, these run
        # shinyDirChoose(input, "find_dir", roots=isolate(values$volumes), session=session)
        #
        # When the custom headers button is pressed, these run
        shinyFileChoose(input = input, id = "find_headers", roots=isolate(values$volumes))
        #shinyFileChoose(input, "find_headers", roots=isolate(values$volumes), session=session)
        print("custom headers")
        print(input$find_headers)
        # Parse the result and save the output directory
        #values$custom_headers <- parseFilePaths(roots = isolate(values$volumes), selection = input$custom_headers)
        values$custom_headers <- as.character(parseFilePaths(roots = values$volumes, selection = input$find_headers)$datapath)
        print(isolate(values$custom_headers))
        # Get the current time
        #timestamp <- strsplit(x = as.character(Sys.time()), split = " ")

        # Make the full output directory with timestamp
        #values$output_dir <- paste(values$save_dir, "/", timestamp[[1]][1], "_", timestamp[[1]][2], sep = "")

        # Print the output directories
        #print(isolate(values$custom_headers))
        #print(isolate(values$output_dir))
    })


    ## Actions that occur when processing selected files
    observeEvent(process_all_listener(), {
        print("processing")
        if (is.null(input$data_file)) return(NULL)
        # If the algorithm is quadratic, the donor concentration must be set. If it is NA, a warning will display and processing will not occur
        if(input$algorithm=="quadratic"){
            if(is.na(input$donor_concentration)){
                # Send the warning
                output$conc_required <- renderText({"Enter a donor concentration or change the algorithm to Hyperbolic"})
                toggle(id = "conc_required")
            }
            # Do not allow anything to continue
            req(input$donor_concentration)
        }

        ## Start the progress indicator
        withProgress(message = 'Working: ', value = 0, min = 0, max = 1, detail = "Fitting binding model ...", {

            # Get the formatted data from the earlier step
            ffd <- values$ffd

            # Filter the formatted data using the accepted datasets
            ffd <- ffd[ffd$Experiment %in% isolate(values$dataset_names)[values$dataset_decisions], ]

            # Number of positive decisions
            values$number_of_positive_decisions <- sum(isolate(values$dataset_decisions))

            # Fit the binding model
            corrected_data <- ffd %>%
                fret_average_replicates() %>%
                fret_correct_signal(output_directory = isolate(values$output_dir))

            output$corrected_data_output <- renderUI({
                tagList(lapply(X = split(corrected_data , f = corrected_data$Experiment), FUN = function(i) {
                    renderTable(expr = {i}, striped = TRUE, rownames = FALSE, colnames = TRUE, digits = 5)
                }))
            })

            fbm <- corrected_data %>% fit_binding_model(binding_model = input$algorithm,
                                                        probe_concentration = as.numeric(input$donor_concentration),
                                                        output_directory = isolate(values$output_dir))

            # Update the progress indicator
            incProgress(.5, detail = "Generate the figures ...")

            # Generate figures with binding model data)
            figures <- fbm %>% make_figure(probe_concentration = as.numeric(input$donor_concentration),
                                           output_directory = isolate(values$output_dir),
                                           plot_format = "png")

            # Update the progress indicator
            incProgress(.75, detail = "Getting kd values ...")

            # Extract the fit coefficients from the binding model data and add them to the list of plots
            for(i in 1:length(figures)){
                print(i)
                figures[[i]][["coefficients"]] <- summary(fbm[[i]])[["coefficients"]]
            }
        })
        # Move the data to a reactive element
        #values$results_data <- reactive({figures})
        values$results_data <- figures

        # Set the results index to 1
        values$results_index <- 1

        # Switch to the results tab
        updateTabsetPanel(session = session, inputId = "main", selected = "fit_new")
        showElement(id = "results_indicator")
        showElement(id = "results_selector")

    })

    process_all_listener <- reactive({input$process_all})
    decision_listener <- reactive({list(values$dataset_decisions, values$inspect_index)})
    prelim_listener <- reactive({list(input$accept, input$reject, input$accept_all, input$data_file)})
    results_listener <- reactive({list(input$next1, input$previous, values$results_index)})

    observeEvent(results_listener(), {
        print("results listener")
        if(is.null(results_figures())) return(NULL)
        print("starting display of results")
        withProgress(message = 'Working: ', value = 0, min = 0, max = 1, detail = "Displaying results ...", {
            print("results index and number of files")
            print(values$results_index)
            print(values$number_of_positive_decisions)
            # Activate the increment buttons
            toggleState(id = "next1", condition = (values$results_index < values$number_of_positive_decisions))
            toggleState(id = "previous", condition = ((values$results_index > 1)))

            output$results_indicator <- renderText({ html_indicator(rep(TRUE, values$number_of_positive_decisions)) })

            # # Generate the results indicator by arranging the figures in a grid as all TRUE
            # p <- do.call(grid.arrange,
            #              c(mapply(FUN = make_indicator,
            #                       rep(TRUE, values$number_of_positive_decisions),
            #                       1:isolate(values$number_of_positive_decisions),
            #                       SIMPLIFY = FALSE,
            #                       USE.NAMES = FALSE),
            #                ncol = 1))
            #
            # # Render the decision indicator
            # output$results_indicator <- renderPlot(
            #     width=30,
            #     height=(isolate(values$number_of_positive_decisions)*30),
            #     bg = "transparent",
            #     {grid.draw(p)})

            print("toggled the buttons")
            # Load the figure data
            figures <- values$results_data
            print("get the results data")
            # Display the plot
            output$results_output <- renderPlot(height = 400, {figures[[values$results_index]]})
            print("render the results table")
            # Create the data table
            output$results_table <- renderTable(striped = TRUE,
                                                rownames = TRUE,
                                                colnames = TRUE, digits = 2,
                                                expr = {figures[[values$results_index]][["coefficients"]][,1:2]})
        })
    })

    # Update the results tab when a change is made
    observeEvent(prelim_listener(), {
        print("observe prelim listener")
        # If there is data to inspect, enable to correct buttons on the inspection tab and display the graph
        if(!is.null(inspection_data())){
            toggleState(id = "process_all", condition = (sum(is.na(values$dataset_decisions)) == 0 &
                                                             (sum(values$dataset_decisions==TRUE)>0)))
            toggleState(id = "accept", condition = (!is.null(input$data_file)))
            toggleState(id = "reject", condition = (!is.null(input$data_file)))
            toggleState(id = "accept_all", condition = (!is.null(input$data_file)))

            output$decision_indicator <- renderText({ html_indicator(values$dataset_decisions) })
            output$raw_output <- renderPlot(height = 800, {grid.draw(inspection_data()[[values$inspect_index]])})
        }
    })

    output$plan_download <- downloadHandler(filename = "experiment_plan.png", content = function(file){
        png(file)
        print(values$plan_output)
        dev.off()
    }, contentType = "image/png")

    output$plan_output <- renderPlot({
        toggleState(id="plan_donor_concentration", condition = (input$plan_algorithm == "quadratic"))
        toggleState(id="plan_hill_coefficient", condition = (input$plan_algorithm == "hill"))
        plan_parameters <- list(kd = input$plan_kd)
        if(!is.na(input$plan_min_concentration)) plan_parameters <- c(plan_parameters, min_concentration = input$plan_min_concentration)
        if(!is.na(input$plan_max_concentration)) plan_parameters <- c(plan_parameters, max_concentration = input$plan_max_concentration)
        if(!is.na(input$plan_algorithm)) plan_parameters <- c(plan_parameters, binding_model = input$plan_algorithm)
        if(!is.na(input$plan_donor_concentration)) plan_parameters <- c(plan_parameters,probe_conc = input$plan_donor_concentration)
        if(!is.na(input$plan_hill_coefficient)) plan_parameters <- c(plan_parameters, hill_coef = input$plan_hill_coefficient )
        print(plan_parameters)
        values$plan_output <- do.call(plan_experiment, plan_parameters)
        return(values$plan_output)
    })

}
## Icons from Font-Awesome (http://fontawesome.io/icons/) and Bootstrap/Glyphicon (https://getbootstrap.com/docs/3.3/components/#glyphicons)
