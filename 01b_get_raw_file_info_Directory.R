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

# Raw File Directory

rawFileDir <- 
   "Z:/ICR/David Butcher/21T data/20191121_EcoliMG1655_PEPPI_M9/"

# Percentage of max injection time to use for determination
# of max injects. 99% recommended

maxinjectcutoff <- 0.99

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


source("02_analyze_raw_file_info.R")