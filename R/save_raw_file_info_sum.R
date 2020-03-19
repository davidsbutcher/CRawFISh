#' Save raw file info sum
#'
#' @name analyze_raw_file_info
#' @param rawFileInfoSum Dataframe containing all raw file summary data.
#' @param outputDir Path to directory for output of summary info sheet.
#' @param use_tdreport Boolean value. Should raw file names be retrieved from a tdReport?
#' @param tdReportName Name of the tdReport file.
#' @return
#' Saves XLSX to output dir containing raw file info summary.
#' @examples
#' Fix this later
#' @importFrom magrittr %>%
#' @importFrom fs dir_exists
#' @importFrom fs dir_create
#' @importFrom fs path_ext_remove
#' @importFrom openxlsx createWorkbook
#' @importFrom openxlsx addWorksheet
#' @importFrom openxlsx writeData
#' @importFrom openxlsx freezePane
#' @importFrom openxlsx setColWidths
#' @importFrom openxlsx setRowHeights
#' @importFrom openxlsx createStyle
#' @importFrom openxlsx addStyle
#' @importFrom openxlsx saveWorkbook


save_raw_file_info_sum <-
   function(
      rawFileInfoSum,
      outputDir,
      use_tdreport,
      tdReportName
   ) {

      if (dir_exists(outputDir) == FALSE) dir_create(outputDir)

      systime <- format(Sys.time(), "%Y%m%d_%H%M%S")

      ## Make Excel workbook

      xl_workbook <-
         createWorkbook()

      xl_workbook %>%
         addWorksheet(paste(systime))

      writeData(xl_workbook, 1, rawFileInfoSum, withFilter = TRUE)
      freezePane(xl_workbook, 1, firstRow = TRUE)
      setColWidths(xl_workbook, 1, cols = 1:length(rawFileInfoSum), widths = "auto")
      setRowHeights(xl_workbook, 1, rows = 1, heights = 30)

      # Create and set styles

      toprow <-
         createStyle(
            fontSize = 11,
            border = "bottom",
            textDecoration = "bold",
            halign = "center",
            valign = "top"
         )

      addStyle(
         xl_workbook, 1, toprow,
         rows = 1, cols = 1:length(rawFileInfoSum)
      )

      if (use_tdreport == TRUE) {

         saveWorkbook(
            xl_workbook,
            file = paste0(outputDir, "/", path_ext_remove(tdReportName), "_CRawFISh_results.xlsx"),
            overwrite = TRUE
         )

      } else {

         saveWorkbook(
            xl_workbook,
            file = paste0(outputDir, "/", systime, "_CRawFISh_results.xlsx"),
            overwrite = TRUE
         )

      }

   }
