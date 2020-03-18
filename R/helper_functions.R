#' Read tdReport Filenames
#' @param tdreport Full path to a tdreport file.
#' @importFrom magrittr %>%
#' @importFrom purrr safely
#' @importFrom RSQLite dbConnect
#' @importFrom RSQLite dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom dplyr tbl
#' @importFrom dplyr select
#' @importFrom dplyr collect
#' @importFrom dplyr pull

read_tdreport_filenames <- function(tdreport) {

   # Uses dbplyr instead of dplyr. Not much faster than read_tdreport2, possibly
   # has less memory usage

   message(paste0("\nEstablishing connection to ", basename(tdreport)))

   # Establish database connection. Keep trying until it works!

   safe_dbConnect <- safely(dbConnect)

   safecon <- safe_dbConnect(RSQLite::SQLite(), ":memory:", dbname = tdreport)

   if (is.null(safecon[["result"]]) == TRUE) message("Connection failed, trying again!")

   iteration_num <- 1

   while (is.null(safecon[["result"]]) == TRUE & iteration_num < 500) {

      iteration_num <- iteration_num + 1

      message(
         paste0("\nTrying to establish database connection, attempt ", iteration_num)
      )

      safecon <-
         safe_dbConnect(
            RSQLite::SQLite(), ":memory:",
            dbname = tdreport,
            synchronous = NULL
         )

   }

   if (is.null(safecon[["result"]]) == TRUE) {

      stop("read_tdreport_protein could not connect to TDreport")

   } else {

      message(paste0("\nConnection to ", basename(tdreport), " succeeded"))
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

#' Make TIC MS1
#' @param rawfilename String containing the name of a single raw file.
#' @param rawfiledata A dataframe containing data extracted from the corresponding raw file.
#' @param plot_theme Optional list of ggplot protos to add to plots.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 aes

make_TIC_ms1 <- function(rawfilename, rawfiledata = NULL, plot_theme = NULL) {

   rawfiledata %>%
      filter(
         filename == rawfilename & MSOrder == "Ms"
      ) %>%
      ggplot(aes(x = StartTime, y = TIC)) +
      geom_line() +
      plot_theme

}

#' Make TIC MS2
#' @param rawfilename String containing the name of a single raw file.
#' @param rawfiledata A dataframe containing data extracted from the corresponding raw file.
#' @param plot_theme Optional list of ggplot protos to add to plots.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 aes

make_TIC_ms2 <- function(rawfilename, rawfiledata = NULL, plot_theme = NULL) {

   rawfiledata %>%
      filter(
         filename == rawfilename & MSOrder == "Ms2"
      ) %>%
      ggplot(aes(x = StartTime, y = TIC)) +
      geom_line() +
      plot_theme

}

#' Make BPC MS1
#' @param rawfilename String containing the name of a single raw file.
#' @param rawfiledata A dataframe containing data extracted from the corresponding raw file.
#' @param plot_theme Optional list of ggplot protos to add to plots.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 aes

make_BPC_ms1 <- function(rawfilename, rawfiledata = NULL, plot_theme = NULL) {

   rawfiledata %>%
      filter(
         filename == rawfilename & MSOrder == "Ms"
      ) %>%
      ggplot(aes(x = StartTime, y = BasePeakIntensity)) +
      geom_line() +
      plot_theme

}

#' Make BPC MS2
#' @param rawfilename String containing the name of a single raw file.
#' @param rawfiledata A dataframe containing data extracted from the corresponding raw file.
#' @param plot_theme Optional list of ggplot protos to add to plots.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 aes

make_BPC_ms2 <- function(rawfilename, rawfiledata = NULL, plot_theme = NULL) {

   rawfiledata %>%
      filter(
         filename == rawfilename & MSOrder == "Ms2"
      ) %>%
      ggplot(aes(x = StartTime, y = BasePeakIntensity)) +
      geom_line() +
      plot_theme

}

#' Make Injection Time Plot, MS1
#' @param rawfilename String containing the name of a single raw file.
#' @param rawfiledata A dataframe containing data extracted from the corresponding raw file.
#' @param plot_theme Optional list of ggplot protos to add to plots.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 aes

make_injTime_plot_ms1 <- function(rawfilename, rawfiledata = NULL, plot_theme = NULL) {

   rawfiledata %>%
      filter(filename == rawfilename & MSOrder == "Ms") %>%
      ggplot(aes(x = StartTime, y = IonInjectionTimems)) +
      geom_point() +
      plot_theme

}

#' Make Injection Time Plot, MS2
#' @param rawfilename String containing the name of a single raw file.
#' @param rawfiledata A dataframe containing data extracted from the corresponding raw file.
#' @param plot_theme Optional list of ggplot protos to add to plots.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 aes

make_injTime_plot_ms2 <- function(rawfilename, rawfiledata = NULL, plot_theme = NULL) {

   rawfiledata %>%
      filter(filename == rawfilename & MSOrder == "Ms2") %>%
      ggplot(aes(x = StartTime, y = IonInjectionTimems)) +
      geom_point() +
      plot_theme

}

#' Make Precursor Mass Plot
#' @param rawfilename String containing the name of a single raw file.
#' @param rawfiledata A dataframe containing data extracted from the corresponding raw file.
#' @param plot_theme Optional list of ggplot protos to add to plots.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 aes

make_precursorMass_plot <- function(rawfilename, rawfiledata = NULL, plot_theme = NULL) {

   rawfiledata %>%
      filter(filename == rawfilename & MSOrder == "Ms2") %>%
      ggplot(aes(x = StartTime, y = PrecursorMass)) +
      geom_point() +
      plot_theme

}

