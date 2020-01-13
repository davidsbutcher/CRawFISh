# Packages ------------------------------------------------------------------------------------

library(here)
library(tictoc)
library(rawDiag)
library(magrittr)
library(tictoc)
library(glue)
library(fs)
library(tibble)
library(glue)
library(RSQLite)
library(purrr)
library(furrr)
library(readr)
library(openxlsx)
library(fs)
library(ggplot2)
library(stringr)
library(dplyr)

# Functions ---------------------------------------------------------------

source("extras/01x_functions.R")

# Initialize Parameters -----------------------------------------------------------------------

# TDReport Directory

tdReportDir <- 
   c("E:/Z Drive Backup 20191213/TDReports/")

# TDReport Name

tdReportName <- 
   # c("20191125_EcoliMG1655_GF_LB-D-20190515_F01-09_2run_CAMsearch.tdReport")
   # c("20190626-28_PEPPI_F01-F06_1mMDTT_2mMIAA_CAMsearch.tdReport")
   # c("20190916_EcoliMG1655_GF_M9-OL-20190515_2runs_CAMSEARCH.tdReport")
   # ("20191105_EcoliMG1655_GF_M9-OL-20190515_F01-09_2run_CAMsearch.tdReport")
   # ("20191109_EcoliMG1655_PEPPI_LB-B-20190515_F01-09_2run_CAMsearch_B.tdReport")
   # "20191125_EcoliMG1655_GF_LB-D-20190515_F01-09_2run_nomods.tdReport"

# Raw File Directory

rawFileDir <- 
   "E:/Z Drive Backup 20191213/21T data/"

# Percentage of max injection time to use for determination
# of max injects. 99% recommended

maxinjectcutoff <- 0.99

# Make workers for future_map

plan(multisession(workers = 8))

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

# rawFileInfo <- 
#    suppressMessages(
#       suppressWarnings(
#          future_map(rawFileList,
#                     read.raw,
#                     .progress = TRUE)
#       )
#    ) %>% 
#    map(as_tibble) %>% 
#    reduce(union_all)

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

# rawFileMetadataFULL <- 
#    suppressMessages(
#       suppressWarnings(
#          future_map(rawFileList,
#                     read.raw.info,
#                     .progress = TRUE)
#       )
#    )

message("DONE reading .raw files!\n\n")

source("02_analyze_raw_file_info.R")