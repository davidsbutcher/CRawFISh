# Packages ------------------------------------------------------------------------------------

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
library(dplyr)
library(stringr)
library(tidylog)


# Functions ---------------------------------------------------------------

read_tdreport_filenames <- function(tdreport) {
      
      # Uses dbplyr instead of dplyr. Not much faster than read_tdreport2, possibly
      # has less memory usage
      
      message(glue("\nEstablishing connection to {basename(tdreport)}..."))
      
      # Establish database connection. Keep trying until it works!
      
      safe_dbConnect <- safely(dbConnect)
      
      safecon <- safe_dbConnect(RSQLite::SQLite(), ":memory:", dbname = tdreport)
      
      if (is.null(safecon[["result"]]) == TRUE) message("Connection failed, trying again!")
      
      iteration_num <- 1
      
      while (is.null(safecon[["result"]]) == TRUE & iteration_num < 500) {
         
         iteration_num <- iteration_num + 1
         
         message(glue("\nTrying to establish database connection, attempt {iteration_num}"))
         safecon <- safe_dbConnect(RSQLite::SQLite(), ":memory:",
                                   dbname = tdreport,
                                   synchronous = NULL)
         
      }
      
      if (is.null(safecon[["result"]]) == TRUE) {
         
         stop("read_tdreport_protein could not connect to TDreport")
         
      } else {
         
         message(glue("\nConnection to {basename(tdreport)} succeeded"))
         con <- safecon[["result"]]
         
      }
      
      output <- 
         tbl(con, "DataFile") %>% 
         select(Name) %>% 
         collect() %>% 
         pull()
      
      # Close database connection and return output table
      
      dbDisconnect(con)
      
      message("read_tdreport_filenames finished")
      
      return(output)
      
   }

# Initialize Parameters -----------------------------------------------------------------------

# TDReport Directory

tdReportDir <- 
   c("Z:/ICR/David Butcher/TDReports/EcoliMG1655/")

# TDReport Name

tdReportName <- 
   c("20190916_EcoliMG1655_GF_M9-OL-20190515_2runs_CAMSEARCH.tdReport")

# Raw File Directory

rawFileDir <- 
   "Z:/ICR/David Butcher/21T data/"

# Percentage of max injection time to use for determination
# of max injects. 99% recommended

maxinjectcutoff <- 0.99

# Make workers for future_map

plan(multisession(workers = 4))

# Load Data ---------------------------------------------------------------

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

rawFilesInDir <- 
   fs::dir_ls(rawFileDir, recurse = TRUE, type = "file", 
              regexp = c("[.]raw$"))

if (any(rawFilesInTDreport %in% basename(rawFilesInDir)) == FALSE) {
   stop("One or more raw files from TDreport not found in raw file directory")
}

rawFileList <- 
   map_chr(rawFilesInTDreport,
           ~str_subset(rawFilesInDir, .x)) %>% 
   as.list()

rawFileInfo <- 
   suppressMessages(
      future_map(rawFileList, read.raw, .progress = TRUE)
   ) %>% 
   map(as_tibble) %>% 
   reduce(union_all)

# Analysis 1 ----------------------------------------------------------------------------------

injectionTimesAnalysis <- 
   rawFileInfo %>%
   mutate(polarity = if_else(
      str_detect(ScanType, fixed("+")), "pos", "neg")
      ) %>%
   group_by(filename, MSOrder, polarity) %>%
   summarize(acquisitionCount = n(),
             maxInjectTime = max(IonInjectionTimems),
             maxInjections = 
                sum(IonInjectionTimems >= maxInjectTime * maxinjectcutoff),
             `maxInjections %` =
                (sum(IonInjectionTimems >= maxInjectTime * maxinjectcutoff)/
                    acquisitionCount) * 100)

# rawFileInfo %>%
#    mutate(polarity = if_else(
#       str_detect(ScanType, fixed("+")), "pos", "neg")
#    ) %>%
#    filter(filename == "DSB-LCA_20191121_M9-M-20190515_PEPPI01B_01.raw" &
#              MSOrder == "Ms2" & polarity == "pos") %>% 
#    select(scanNumber, IonInjectionTimems) %>% 
#    View()