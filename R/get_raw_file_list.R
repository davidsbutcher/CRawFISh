
#' get_raw_file_list
#'
#' @name get_raw_file_list
#' @param rawFileDir Full path to the directory containing relevant raw files.
#' @param tdReportDir Full path to the directory containing the tdReport.
#' @param tdReportName Name of the tdReport file.
#' @param use_tdreport Boolean value. Should raw file names be retrieved from a tdReport?
#' @importFrom fs dir_ls
#' @importFrom purrr as_vector
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom stringr str_subset
#' @importFrom purrr reduce
#' @importFrom furrr future_map
#' @return
#' List containing full paths of raw files
#' @examples
#' Fix this later

get_raw_file_list <-
   function(
      rawFileDir = NULL,
      tdReportDir = NULL,
      tdReportName = NULL,
      use_tdreport = NULL
   ) {

      if (use_tdreport == TRUE) {

         # TDreport ----------------------------------------------------------------

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

      } else {

         # Directory ---------------------------------------------------------------

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

      }

      return(rawFileList)

   }
