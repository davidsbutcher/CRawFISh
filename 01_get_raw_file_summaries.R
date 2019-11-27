# Packages ------------------------------------------------------------------------------------

library(rawDiag)
library(magrittr)
library(tictoc)
library(glue)
library(fs)
library(tibble)
library(purrr)
library(furrr)
library(dplyr)
library(tidylog)


# Functions ---------------------------------------------------------------



# Initialize Parameters -----------------------------------------------------------------------

# Directory containing raw files

filedir <- 
   "Z:/ICR/David Butcher/21T data/20190725_EcoliMG1655_WCL_M9-J-20190404"

# Percentage of max injection time to use for determination
# of max injects. 99% recommended

maxinjectcutoff <- 0.99

# Make workers for future_map

plan(multisession(workers = 4))

# Load Data ---------------------------------------------------------------

filelist <- 
   filedir %>%
   dir_ls(glob = "*.raw") %>% 
   as.list()

rawfileinfolist <- 
   future_map(filelist, read.raw, .progress = TRUE) %>% 
   map(as_tibble)

rawfileinfo <- 
   future_map2(rawfileinfolist, filelist,
               ~dplyr::mutate(.x, filename = basename(.y))) %>% 
   reduce(union_all) %>%
   filter(MassAnalyzer == "MassAnalyzerFTMS")

# Analysis 1 ----------------------------------------------------------------------------------

injectiontimesanalysis <- 
   rawfileinfo %>%
   group_by(filename, MSOrder) %>%
   summarize(acquisitionCount = n(),
             maxInjectTime = max(IonInjectionTimems),
             maxInjections = 
                sum(IonInjectionTimems >= maxInjectTime * maxinjectcutoff),
             `maxInjections %` =
                (sum(IonInjectionTimems >= maxInjectTime * maxinjectcutoff)/
                    acquisitionCount) * 100)
