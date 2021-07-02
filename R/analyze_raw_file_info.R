
#' Analyze raw file info
#'
#' @name analyze_raw_file_info
#' @param rawFileInfo Dataframe containing all raw file data.
#' @param maxinjectcutoff Fraction of max injection time that should be used as cutoff for max injections.
#' @return
#' Dataframe containing analysis of raw file info.
#' @importFrom magrittr %>%
#' @importFrom fs dir_exists
#' @importFrom fs dir_create
#' @importFrom stringr str_detect
#' @importFrom stringr str_match
#' @importFrom stringr str_remove
#' @importFrom stringr str_extract
#' @importFrom stringr fixed
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr summarize
#' @importFrom dplyr case_when
#' @importFrom dplyr if_else

analyze_raw_file_info <-
   function(
      rawFileInfo,
      maxinjectcutoff
   ) {

      # Analysis 1 ----------------------------------------------------------------------------------

      message("\n\nAnalyzing .raw file info...")

      basicAnalysis <-
         rawFileInfo %>%
         group_by(filename, ScanSegment, ScanEvent, polarity, MSOrder, MassAnalyzer) %>%
         summarize(
            scanHeader = if_else(MSOrder == 1, ScanType, "N/A")[1],
            FTResolution = FTResolution[1],
            MicroScanCount = MicroScanCount[1],
            CtrapFill = CtrapFill[1],
            meanUfill = mean(Ufill),
            APISourceCIDEnergy = APISourceCIDEnergy[1],
            CollisionEnergy = CollisionEnergy[1],
            MS2IsolationWidth = MS2IsolationWidth[1],
            averageScanTime = mean(ElapsedScanTimesec),
            stdDevScanTime = sd(ElapsedScanTimesec),
            acquisitionCount = dplyr::n(),
            maxTIC = max(TIC),
            meanTIC = mean(TIC),
            stdDevTIC = sd(TIC),
            maxInjectTime = max(IonInjectionTimems),
            meanInjectTime = mean(IonInjectionTimems),
            stdDevInjectTime = sd(IonInjectionTimems),
            maxInjectTime = max(IonInjectionTimems),
            maxInjections =
               sum(IonInjectionTimems >= maxInjectTime * maxinjectcutoff),
            `maxInjections %` =
               (sum(IonInjectionTimems >= maxInjectTime * maxinjectcutoff)/
                   acquisitionCount) * 100
         ) %>%
         ungroup() %>%
         mutate(MassAnalyzer = str_remove(MassAnalyzer, fixed("MassAnalyzer")))

      message("Done summarizing raw file info\n\n")

      return(basicAnalysis)

   }
