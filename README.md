CRawFISh
================

Make a statistical summary sheet for any number of Thermo Raw files

## Installation

Install from GitHub:

``` r
remotes::install_github(davidsbutcher/crawfish)
```

## Usage

The exported function from this package, `crawfish`, can be used to get
summary information on raw files. Raw files can be specified by
providing a directory or a tdReport file.

### Directory

``` r
crawfish(
      rawFileDir = "C:/rawfiles/experiment1",
      outputDir = "C:/rawfiles/experiment1_output"
   )
```

All .raw files in `rawFileDir` and its subdirectories will be read and
summarized.

### tdReport

``` r
crawfish(
      rawFileDir = "C:/rawfiles/experiment1",
      use_tdreport = TRUE,
      tdReportDir = "C:/tdreports",
      tdReportName = "201912_experiment1.tdReport",
      outputDir = "C:/rawfiles/experiment1_output"
   )
```

`rawFileDir` will be searched for the file called `tdReportName`. If
found, names of all raw files which were searched to generate the report
are extracted. The `rawFileDir` is then searched for the extracted raw
files, which are read and summarized as above. Note that the names of
the raw files in the tdReport must match the names of the files in the
`rawFileDir`.

### Additional arguments

For all analyses, the following arguments can be specified:

  - maxinjectcutoff: Fraction of maximum injection time that is used to
    determine the cutoff for “max injects”. Defaults to 0.99.

  - make\_report: Boolean value (TRUE or FALSE). Determines whether an
    HTML report is generated in the `outputDir`. Defaults to TRUE.

## Dependencies

This package imports `assertthat`, `ggplot2`, `rawDiag` and selected
functions from `RSQLite`, `dplyr`, `fs`, `magrittr`, `openxlsx`,
`purrr`, `rmarkdown`, `stringr`, and `tibble`.

## License

Creative Commons CC0
