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

# Initialize Parameters ---------------------------------------------------

# Search tdreport?

use_tdreport <- F

# TDReport Directory

tdReportDir <- 
   c("Z:/ICR/David Butcher/TDReports/")

# TDReport Name

tdReportName <- 
   c("20200106_EcoliMG1655_GF_LB-D-20190515_F01-09_2run_CAMsearch.tdReport")

# Raw File Directory

rawFileDir <- 
   "Z:/ICR/David Butcher/21T data/20200202_EcoliMG1655_WCL_columnheater/60C"


# Percentage of max injection time to use for determination
# of max injects. 99% recommended

maxinjectcutoff <- 0.99

# Make workers for future_map

plan(multisession(workers = 8))


# Run Scripts -------------------------------------------------------------

if (use_tdreport == TRUE) {
   
   source("01a_get_raw_file_info_TDReport.R")
   
} else if (use_tdreport == FALSE) {
   
   source("01b_get_raw_file_info_Directory.R")
   
} else if (exists("use_tdreport") == FALSE) {
   
   stop("Need to specify use_tdreport as TRUE or FALSE")
      
}

source("02_analyze_raw_file_info.R")

# Output report -----------------------------------------------------------

if (use_tdreport == TRUE) {
   
   rmarkdown::render(
      "03a_generate_report_parent.Rmd",
      output_file = 
         glue("output/{path_ext_remove(tdReportName)}_CRawFISh_report.html")
   )
   
} else {
   
   rmarkdown::render(
      "03a_generate_report_parent.Rmd",
      output_file = glue("output/{systime}_CRawFISh_report.html")
   )
   
}

message(glue("\n\nScript finished, {capture.output(toc())}"))

plan(multisession(workers = 1))


