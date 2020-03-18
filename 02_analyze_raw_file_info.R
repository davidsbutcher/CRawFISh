
# Analysis 1 ----------------------------------------------------------------------------------

message("\n\nAnalyzing .raw file info...")

basicAnalysis <- 
   rawFileInfo %>%
   mutate(MSOrder = case_when(MSOrder == "Ms" ~ 1,
                              MSOrder == "Ms2" ~ 2,
                              MSOrder == "Ms3" ~ 3,
                              MSOrder == "Ms4" ~ 4) %>% as.integer) %>% 
   mutate(polarity = if_else(
      str_detect(ScanType, fixed("+")), "pos", "neg")
   ) %>%
   mutate(
      CtrapFill = if_else(str_detect(FTAnalyzerMessage, fixed("CTrap=")),
                          str_match(FTAnalyzerMessage, "\\d{1,2}"),
                          "1") %>% as.integer
   ) %>%  
   group_by(filename, ScanSegment, ScanEvent, polarity, MSOrder, MassAnalyzer) %>%
   summarize(scanHeader = if_else(MSOrder == 1, ScanType, "N/A")[1],
             FTResolution = FTResolution[1],
             MicroScanCount = MicroScanCount[1],
             CtrapFill = CtrapFill[1],
             APISourceCIDEnergy = APISourceCIDEnergy[1],
             CollisionEnergy = CollisionEnergy[1],
             MS2IsolationWidth = MS2IsolationWidth[1],
             averageScanTime = mean(ElapsedScanTimesec),
             stdDevScanTime = sd(ElapsedScanTimesec),
             acquisitionCount = n(),
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
                    acquisitionCount) * 100) %>% 
   ungroup() %>% 
   mutate(MassAnalyzer = str_remove(MassAnalyzer, fixed("MassAnalyzer")))

setwd(here())

message("DONE!\n\n")

# Output data -------------------------------------------------------------

if (dir_exists("output/") == FALSE) dir_create("output/")

systime <- format(Sys.time(), "%Y%m%d_%H%M%S")

# write_csv(basicAnalysis, glue("output/{systime}_CRawFISh_results.csv"))

## Make Excel workbook

xl_workbook <- 
   createWorkbook()

xl_workbook %>% 
   addWorksheet(glue("{systime}"))

writeData(xl_workbook, 1, basicAnalysis, withFilter = TRUE)
freezePane(xl_workbook, 1, firstRow = TRUE)
setColWidths(xl_workbook, 1, cols = 1:length(basicAnalysis), widths = "auto")
setRowHeights(xl_workbook, 1, rows = 1, heights = 30)

# Create and set styles

toprow <- 
   createStyle(fontSize = 11,
               border = "bottom",
               textDecoration = "bold",
               halign = "center",
               valign = "top")

addStyle(xl_workbook, 1, toprow,
         rows = 1, cols = 1:length(basicAnalysis))

if (use_tdreport == TRUE) {
   
   saveWorkbook(
      xl_workbook,
      file = glue("output/{path_ext_remove(tdReportName)}_CRawFISh_report.xlsx"),
      overwrite = TRUE
   )
   
} else {
   
   saveWorkbook(
      xl_workbook,
      file = glue("output/{systime}_CRawFISh_results.xlsx"),
      overwrite = TRUE
   )
   
}

# ggplot Themes -----------------------------------------------------------

suppressMessages(
   extrafont::loadfonts(device = "win")
)

TICtheme <- 
   list(
      geom_label(
         data = ~filter(.x, TIC == max(TIC)),
         aes(x = StartTime, y = TIC,
             label = glue("Max TIC: {format(TIC,
                          scientific = TRUE, nsmall = 4, digits = 4)}")),
         nudge_x = 25
      ),
      ggthemes::theme_clean(),
      theme(
         text = element_text(size = 16, family = "Arial"),
         axis.text = element_text(size = 14),
         axis.title = element_text(size = 14)
      ),
      labs(x = "Retention Time (min)",
           y = "Total Ion Current")
   )

BPCtheme <- 
   list(
      geom_label(
         data = ~filter(.x, BasePeakIntensity == max(BasePeakIntensity)),
         aes(x = StartTime, y = BasePeakIntensity,
             label = glue("Max BPI: {format(BasePeakIntensity,
                          scientific = TRUE, nsmall = 4, digits = 4)}")),
         nudge_x = 25
      ),
      ggthemes::theme_clean(),
      theme(
         text = element_text(size = 16, family = "Arial"),
         axis.text = element_text(size = 14),
         axis.title = element_text(size = 14)
      ),
      labs(x = "Retention Time (min)",
           y = "Base Peak Intensity")
   )

scatterPlotTheme <- 
   list(
      geom_label(
         data = ~filter(.x, IonInjectionTimems == max(IonInjectionTimems)),
         aes(x = StartTime, y = IonInjectionTimems,
             label = glue("Max InjTime: {IonInjectionTimems}")),
         nudge_x = 25
      ),
      ggthemes::theme_clean(),
      theme(
         text = element_text(size = 16, family = "Arial"),
         axis.text = element_text(size = 14),
         axis.title = element_text(size = 14)
      ),
      labs(x = "Retention Time (min)",
           y = "Ion Injection Time (ms)")
   )

# Make Plots --------------------------------------------------------------

message("\n\nCreating plots from .raw file summary...\n")

TIC_ms1 <- 
   future_map(rawFileList %>% map(basename),
              make_TIC_ms1,
              rawfiledata = rawFileInfo,
              plot_theme = TICtheme,
              .progress = TRUE)

set_names(TIC_ms1, rawFileList %>% unlist() %>% basename())

TIC_ms2 <- 
   future_map(rawFileList %>% map(basename),
              make_TIC_ms2,
              rawfiledata = rawFileInfo,
              plot_theme = TICtheme,
              .progress = TRUE)

set_names(TIC_ms2, rawFileList %>% unlist() %>% basename())

BPC_ms1 <- 
   future_map(rawFileList %>% map(basename),
              make_BPC_ms1,
              rawfiledata = rawFileInfo,
              plot_theme = BPCtheme,
              .progress = TRUE)

set_names(BPC_ms1, rawFileList %>% unlist() %>% basename())

BPC_ms2 <- 
   future_map(rawFileList %>% map(basename),
              make_BPC_ms2,
              rawfiledata = rawFileInfo,
              plot_theme = BPCtheme,
              .progress = TRUE)

set_names(BPC_ms2, rawFileList %>% unlist() %>% basename())

injectionTime_ms1_plot <- 
   future_map(rawFileList %>% map(basename),
              make_injTime_plot_ms1,
              rawfiledata = rawFileInfo,
              plot_theme = scatterPlotTheme,
              .progress = TRUE)

set_names(injectionTime_ms1_plot, rawFileList %>% unlist() %>% basename())

injectionTime_ms2_plot <- 
   future_map(
      rawFileList %>% map(basename),
      make_injTime_plot_ms2,
      rawfiledata = rawFileInfo,
      plot_theme = scatterPlotTheme,
      .progress = TRUE
   )

set_names(injectionTime_ms2_plot, rawFileList %>% unlist() %>% basename())

message("DONE!\n\n")



# make_BPC_ms2(
#    rawFileList[[4]] %>% basename(),
#    rawfiledata = rawFileInfo,
#    plot_theme = BPCtheme
# )