# This file, create_master_file.R, contains 2 functions:
# 1) makeMasterFile: this is the main function.
# 2) cleanData: this is an internal function that makeMasterFile calls.

#' @title Clean data from single csv file
#'
#' @description This is an internal function that processes a single csv file and generates the
#'      \code{Experiment}, \code{Type}, \code{Replicate}, and \code{Observation} columns.  One data frame is
#'      returned with the rows of data generated from a single csv file.
#' @param dataTmpFile Location of single csv file to process.
#' @param inputFileName csv file name.
#' @param csvSeparator csv separator.
#' @param quoteVersion Version of quotes used.
#' @param headerOption Header option.
#' @param numberOfLinesToSkip Number of lines to skip when reading the csv file, usually containing header information.
#' @return One dataframe is returned, containing 11 columns with the input data from a single file: \code{Experiment},
#' \code{Type}, \code{Replicate}, \code{Observation}, \code{Well.Row}, \code{Well.Col}, \code{Content},
#' \code{fret_channel}, \code{donor_channel}, \code{acceptor_channel}, \code{concentration}.
#'
#' @examples
#' \dontrun{
#'
#'  cleanData(files[1], files[1], ",", "\"", TRUE, 4)
#'
#'  }

cleanData <- function(dataTmpFile, inputFileName, csvSeparator, quoteVersion, headerOption, numberOfLinesToSkip){

    raw_data = read.csv(dataTmpFile,
                               header=headerOption,
                               sep=csvSeparator,
                               quote=quoteVersion,
                               skip=numberOfLinesToSkip,
                               as.is=TRUE)

    raw_data$Experiment = strsplit(inputFileName, "[.]")[[1]][1]

    raw_data =
        raw_data %>%
        dplyr::rowwise() %>%
        dplyr::mutate(intermediate = strsplit(Content, "_"),
                      Type = intermediate[1],
                      Replicate = intermediate[2]) %>%
        dplyr::select(-intermediate)

    raw_data = raw_data %>%
        dplyr::group_by(Experiment, Type, Replicate) %>%
        dplyr::mutate(Observation = row_number())

    raw_data <- raw_data[c("Experiment", "Type", "Replicate", "Observation", "Well.Row", "Well.Col", "Content",
                           "fret_channel", "donor_channel", "acceptor_channel", "concentration")]
    return(raw_data)
}


#' @title Create master data frame from input csv file(s)
#'
#' @description This function creates one 'master' data frame by combining all the input csv files. The
#'      input variables are 1) list of data files to process, 2) list of corresponding file names,
#'      3) csv separator value, 4) quote version, 5) header option, and 6) number of lines in the csv file to skip.
#'      For each input file, this function creates the \code{Experiment} column based on the \code{file_name}
#'      for each file_name.csv provided as input.  The \code{Type} and \code{Replicate} are extrapolated
#'      from the \code{Content} column in the original data.  \code{Type} is either 'blank' or 'titration'.
#'      \code{Replicate} is the number of technical replicate.  For example, \code{Type} = 'blank' and
#'      \code{Replicate} = '1' if \code{Content} = "blank_1".  The data is then grouped by \code{Experiment},
#'      \code{Type}, and \code{Replicate} to determine the \code{Observation} number.  \code{Observation}
#'      number should match the \code{Well.Col} number. \code{dplyr::full_join} is used to combine the rows
#'      from each input file.
#' @param listOfDataTmpFiles A list of input csv files to be processed.
#' @param listOfInputFileNames A list of corresponding csv file names.
#' @param csvSeparator csv separator.
#' @param quoteVersion Version of quotes used.
#' @param headerOption Header option.
#' @param numberOfLinesToSkip The number of lines to skip in the csv file, usually containing header information.
#' @return One dataframe is returned, containing 11 columns with the combined input data: \code{Experiment},
#' \code{Type}, \code{Replicate}, \code{Observation}, \code{Well.Row}, \code{Well.Col}, \code{Content},
#' \code{fret_channel}, \code{donor_channel}, \code{acceptor_channel}, \code{concentration}.
#' @examples
#' \dontrun{
#'
#' makeMasterFile(c(files[1],files[2]), c(files[1],files[2]), ",", "\"", TRUE, 4)
#'
#' }
#' @export

makeMasterFile <- function(listOfDataTmpFiles, listOfInputFileNames, csvSeparator, quoteVersion, headerOption, numberOfLinesToSkip){

    masterFile <- cleanData(listOfDataTmpFiles[1], listOfInputFileNames[1], csvSeparator, quoteVersion, headerOption, numberOfLinesToSkip)

    for(i in 2:length(listOfDataTmpFiles)){

        nextData <- cleanData(listOfDataTmpFiles[i], listOfInputFileNames[i], csvSeparator, quoteVersion, headerOption, numberOfLinesToSkip)

        masterFile = dplyr::full_join(masterFile, nextData)
    }

    return(masterFile)
}
