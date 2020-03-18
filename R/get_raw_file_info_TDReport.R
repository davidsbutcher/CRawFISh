
#' Get raw file info, TDReport
#'
#' @name get_raw_file_info_TDReport
#' @param rawFileDir Full path to the directory containing relevant raw files.
#' @param tdReportDir Full path to the directory containing the tdReport.
#' @param tdReportName Name of the tdReport file.
#' @return
#' Dataframe containing all raw info from all raw files in the tdReport.
#' @examples
#' Fix this later
#' @import rawDiag
#' @importFrom fs dir_ls
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom purrr as_vector
#' @importFrom purrr reduce
#' @importFrom furrr future_map
#' @importFrom stringr str_detect
#' @importFrom stringr str_subset
#' @importFrom dplyr union_all
#' @importFrom tibble as_tibble

get_raw_file_info_TDReport <-
   function(
      rawFileDir,
      tdReportDir,
      tdReportName
   ) {

      library(rawDiag)

      # Load Data ---------------------------------------------------------------

      filesindir <-
         dir_ls(
            tdReportDir,
            recurse = TRUE,
            type = "file",
            regexp = c("[.]tdReport$")
         ) %>%
         as_vector()

      if (
         map(
            tdReportName,
            ~str_detect(filesindir, .x)
         ) %>%
         map(any) %>%
         as_vector() %>%
         all() == FALSE
      ) stop("One or more tdReports not found")

      tdReportList <-
         tdReportName %>%
         map_chr(~str_subset(filesindir, .x)) %>%
         as.list()

      ##

      rawFilesInTDreport <-
         tdReportList %>%
         future_map(read_tdreport_filenames) %>%
         reduce(unlist)

      rawFilesInTDreportList <-
         rawFilesInTDreport %>%
         as.list()

      rawFilesInDir <-
         dir_ls(
            rawFileDir,
            recurse = TRUE,
            type = "file",
            regexp = c("[.]raw$")
         )

      ## Check for missing files

      if (any(!rawFilesInTDreport %in% basename(rawFilesInDir)) == TRUE) {

         missingFiles <-
            rawFilesInTDreport[!rawFilesInTDreport %in% basename(rawFilesInDir)]

         for (i in seq_along(missingFiles)) {

            paste("File not found in rawFileDir:", missingFiles[i]) %>%
               message()

         }

         stop("One or more raw files from TDreport not found in raw file directory")

      }

      rawFileList <-
         map(
            rawFilesInTDreport,
            ~str_subset(rawFilesInDir, .x)
         ) %>%
         as.list()

      message("\n\nReading .raw files\n")


      rawFileInfo <-
         suppressMessages(
            suppressWarnings(
               future_map(
                  rawFileList,
                  read.raw,
                  rawDiag = FALSE,
                  .progress = TRUE
               )
            )
         ) %>%
         map(as_tibble) %>%
         reduce(union_all)

      message("DONE reading .raw files!\n\n")

      return(rawFileInfo)

   }
