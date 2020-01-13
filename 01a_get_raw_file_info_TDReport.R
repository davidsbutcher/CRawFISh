
# Functions ---------------------------------------------------------------

source("extras/01x_functions.R")

# Load Data ---------------------------------------------------------------

tic()

filesindir <- 
   fs::dir_ls(tdReportDir, recurse = TRUE, type = "file", 
              regexp = c("[.]tdReport$")) %>% 
   purrr::as_vector()

if (map(tdReportName, 
        ~str_detect(filesindir, .x)) %>% 
    map(any) %>%
    as_vector() %>% 
    all() == FALSE) stop("One or more tdReports not found")

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
   fs::dir_ls(rawFileDir, recurse = TRUE, type = "file", 
              regexp = c("[.]raw$"))

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
   map(rawFilesInTDreport,
       ~str_subset(rawFilesInDir, .x)) %>% 
   as.list()

message("\n\nReading .raw files\n")


rawFileInfo <- 
   suppressMessages(
      suppressWarnings(
         future_map(rawFileList,
                    read.raw,
                    rawDiag = FALSE,
                    .progress = TRUE)
      )
   ) %>% 
   map(as_tibble) %>% 
   reduce(union_all)


message("DONE reading .raw files!\n\n")
