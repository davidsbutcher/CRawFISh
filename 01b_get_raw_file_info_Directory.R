
# Functions ---------------------------------------------------------------

source("extras/01x_functions.R")

# Load Data ---------------------------------------------------------------

tic()

rawFilesInDir <- 
   fs::dir_ls(rawFileDir, recurse = TRUE, type = "file", 
              regexp = c("[.]raw$"))

if (length(rawFilesInDir) == 0) {
   stop("No .raw files found in raw file directory")
}

rawFileList <- 
   rawFilesInDir %>% 
   as.list()

if (length(rawFileList) >= 8) {
   
   plan(multisession(workers = 8))
   
} else {
   
   plan(multisession(workers = length(rawFileList)))
   
}

message("\n\nReading .raw files...\n")

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
