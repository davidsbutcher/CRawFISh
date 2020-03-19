#' Create Raw File Information Sheet
#'
#' @name crawfish
#' @param rawFileDir Full path to the directory containing relevant raw files.
#' @param use_tdreport Boolean value. Should raw file names be retrieved from a tdReport?
#' @param tdReportDir Full path to the directory containing the tdReport.
#' @param tdReportName Name of the tdReport file.
#' @param maxinjectcutoff Fraction of max injection time that should be used as cutoff for max injections.
#' @param make_report Boolean value. Create an HTML report in the output directory?
#' @return
#' This function saves a table of summary data (.xlsx) and an html report for the relevant raw files to the specified output directory.
#' @examples
#' IOU some examples. -David
#' @import assertthat
#' @import ggplot2
#' @import tictoc
#' @importFrom fs path_ext_remove
#' @importFrom rmarkdown render
#' @importFrom rlang set_names
#' @export

crawfish <-
   function(
      rawFileDir = NULL,
      use_tdreport = FALSE,
      tdReportDir = NULL,
      tdReportName = NULL,
      outputDir = NULL,
      maxinjectcutoff = 0.99,
      make_report = TRUE
   ) {

      # Assertions --------------------------------------------------------------

      assert_that(
         is.dir(rawFileDir),
         msg = "rawFileDir is not a recognized path"
      )

      assert_that(
         is.flag(use_tdreport),
         msg = "use_tdreport is not a Boolean value"
      )

      if (use_tdreport == TRUE) {

         assert_that(
            is.dir(tdReportDir),
            msg = "tdReportDir is not a recognized path"
         )

         assert_that(
            has_extension(tdReportName, "tdReport"),
            msg = "tdReportName does not have the right extension"
         )

      }

      assert_that(
         is.dir(outputDir),
         msg = "outputDir is not a recognized path"
      )

      assert_that(
         is.numeric(maxinjectcutoff),
         maxinjectcutoff > 0,
         maxinjectcutoff <= 1,
         msg = "maxinjectcutoff should be between 0 and 1"
      )



      # Choose subfunction ------------------------------------------------------

      tic()

      if (use_tdreport == TRUE) {

         rawFileInfo <-
            get_raw_file_info_TDReport(
               rawFileDir,
               tdReportDir,
               tdReportName
            )

      } else {

         rawFileInfo <-
            get_raw_file_info_Directory(
               rawFileDir
            )

      }

      rawFileInfoSum <-
         analyze_raw_file_info(rawFileInfo, maxinjectcutoff)

      ## Save summary results to an XLSX

      save_raw_file_info_sum(
         rawFileInfoSum,
         outputDir,
         use_tdreport,
         tdReportName
      )


      # Load ggplot themes ------------------------------------------------------

      suppressMessages(
         extrafont::loadfonts(device = "win")
      )

      TICtheme <-
         list(
            geom_label(
               data = ~filter(.x, TIC == max(TIC)),
               aes(
                  x = StartTime,
                  y = TIC,
                  label = paste0(
                     "Max TIC: ",
                     format(
                        TIC,
                        scientific = TRUE,
                        nsmall = 4,
                        digits = 4
                     )
                  )
               ),
               nudge_x = 25
            ),
            ggthemes::theme_clean(),
            theme(
               text = element_text(size = 16, family = "Arial"),
               axis.text = element_text(size = 14),
               axis.title = element_text(size = 14)
            ),
            labs(
               x = "Retention Time (min)", y = "Total Ion Current"
            )
         )

      BPCtheme <-
         list(
            geom_label(
               data = ~filter(.x, BasePeakIntensity == max(BasePeakIntensity)),
               aes(
                  x = StartTime,
                  y = BasePeakIntensity,
                  label = paste0(
                     "Max BPI: ",
                     format(
                        BasePeakIntensity,
                        scientific = TRUE,
                        nsmall = 4,
                        digits = 4
                     )
                  )
               ),
               nudge_x = 25
            ),
            ggthemes::theme_clean(),
            theme(
               text = element_text(size = 16, family = "Arial"),
               axis.text = element_text(size = 14),
               axis.title = element_text(size = 14)
            ),
            labs(
               x = "Retention Time (min)",
               y = "Base Peak Intensity"
            )
         )

      scatterPlotTheme <-
         list(
            geom_label(
               data = ~filter(.x, IonInjectionTimems == max(IonInjectionTimems)),
               aes(
                  x = StartTime, y = IonInjectionTimems,
                  label = paste0("Max InjTime: ", IonInjectionTimems)
               ),
               nudge_x = 25
            ),
            ggthemes::theme_clean(),
            theme(
               text = element_text(size = 16, family = "Arial"),
               axis.text = element_text(size = 14),
               axis.title = element_text(size = 14)
            ),
            labs(
               x = "Retention Time (min)",
               y = "Ion Injection Time (ms)"
            )
         )





      # Make Plots --------------------------------------------------------------

      rawFileList <-
         get_raw_file_list(
            rawFileDir = rawFileDir,
            tdReportDir = tdReportDir,
            tdReportName = tdReportName,
            use_tdreport = use_tdreport
         )

      message("\n\nCreating plots from .raw file summary...\n")

      TIC_ms1 <-
         map(
            rawFileList %>% map(basename),
            make_TIC_ms1,
            rawfiledata = rawFileInfo,
            plot_theme = TICtheme
         )

      set_names(TIC_ms1, rawFileList %>% unlist() %>% basename())

      TIC_ms2 <-
         map(
            rawFileList %>% map(basename),
            make_TIC_ms2,
            rawfiledata = rawFileInfo,
            plot_theme = TICtheme
         )

      set_names(TIC_ms2, rawFileList %>% unlist() %>% basename())

      BPC_ms1 <-
         map(
            rawFileList %>% map(basename),
            make_BPC_ms1,
            rawfiledata = rawFileInfo,
            plot_theme = BPCtheme
         )

      set_names(BPC_ms1, rawFileList %>% unlist() %>% basename())

      BPC_ms2 <-
         map(
            rawFileList %>% map(basename),
            make_BPC_ms2,
            rawfiledata = rawFileInfo,
            plot_theme = BPCtheme
         )

      set_names(BPC_ms2, rawFileList %>% unlist() %>% basename())

      injectionTime_ms1_plot <-
         map(
            rawFileList %>% map(basename),
            make_injTime_plot_ms1,
            rawfiledata = rawFileInfo,
            plot_theme = scatterPlotTheme
         )

      set_names(injectionTime_ms1_plot, rawFileList %>% unlist() %>% basename())

      injectionTime_ms2_plot <-
         map(
            rawFileList %>% map(basename),
            make_injTime_plot_ms2,
            rawfiledata = rawFileInfo,
            plot_theme = scatterPlotTheme
         )

      set_names(injectionTime_ms2_plot, rawFileList %>% unlist() %>% basename())


      # Knit the Crawfish report ------------------------------------------------

      if (make_report == TRUE) {

         setwd(
            paste0(.libPaths(), "/CRawFISh/")
         )

         if (use_tdreport == TRUE) {

            render(
               "rmd/generate_report_parent.Rmd",
               output_file =
                  paste0(outputDir, "/", path_ext_remove(tdReportName), "_CRawFISh_report.html")
            )

         } else {

            systime <- format(Sys.time(), "%Y%m%d_%H%M%S")

            render(
               "rmd/generate_report_parent.Rmd",
               output_file = paste0(outputDir, "/", systime, "_CRawFISh_report.html")
            )

         }

      }

      message(
         paste0("\n\nCrawfish finished, ", capture.output(toc()))
      )

   }
