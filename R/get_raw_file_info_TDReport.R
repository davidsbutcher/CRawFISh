
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
         map(read_tdreport_filenames) %>%
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

      ## Check for duplicate files

      if (any(length(rawFileList) > 1)) {

         message("\nRaw files from TDReport found in multiple locations. Using first location found\n")

         rawFileList <-
            rawFileList %>%
            map(
               ~(magrittr::extract2(.x, 1))
            )

      }

      ## Read raw files

      message("\n\nReading .raw files\n")

      rawFileInfo <-
         suppressMessages(
            suppressWarnings(
               map(
                  rawFileList,
                  read.raw,
                  rawDiag = FALSE
               )
            )
         ) %>%
         map(as_tibble) %>%
         reduce(union_all) %>%
         mutate(
            MSOrder = case_when(
               MSOrder == "Ms" ~ 1,
               MSOrder == "Ms2" ~ 2,
               MSOrder == "Ms3" ~ 3,
               MSOrder == "Ms4" ~ 4
            ) %>% as.integer
         ) %>%
         mutate(
            polarity = if_else(
               str_detect(ScanType, fixed("+")), "pos", "neg"
            )
         ) %>%
         mutate(
            CtrapFill = if_else(
               str_detect(
                  FTAnalyzerMessage, fixed("CTrap=")
               ),
               str_extract(
                  FTAnalyzerMessage, "(?<=CTrap=)\\d{1,2}"
               ),
               "1"
            ) %>% as.integer()
         ) %>%
         mutate(
            Ufill = if_else(
               str_detect(
                  FTAnalyzerMessage, fixed("Ufill=")
               ),
               str_extract(
                  FTAnalyzerMessage, "(?<=Ufill=)0\\.\\d{1,3}"
               ),
               "1"
            ) %>% as.double()
         )

      message("DONE reading .raw files!\n\n")

      return(rawFileInfo)

   }
