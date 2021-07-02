
#' Get raw file info, Directory
#'
#' @name get_raw_file_info_Directory
#' @param rawFileDir Full path to the directory containing relevant raw files.
#' @return
#' Dataframe containing all raw info from all raw files in the input directory.
#' @examples
#' Fix this later
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
#' @importFrom rawDiag read.raw

get_raw_file_info_Directory <-
   function(
      rawFileDir
   ) {

      library(rawDiag)

      # Load Data ---------------------------------------------------------------


      rawFilesInDir <-
         dir_ls(
            rawFileDir,
            recurse = TRUE,
            type = "file",
            regexp = c("[.]raw$")
         )

      if (length(rawFilesInDir) == 0) {
         stop("No .raw files found in raw file directory")
      }

      rawFileList <-
         rawFilesInDir %>%
         as.list()

      message("\n\nReading .raw files...\n")

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

