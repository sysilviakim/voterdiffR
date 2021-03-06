#' Importing Snapshots into List/Dataframe
#'
#' This function imports consecutive snapshots of the data into a list output.
#' If there is only one snapshot input requested, it will return a single
#' dataframe as is. Unless otherwise supplied, the output list will reference
#' the snapshots as dfA, dfB, dfC, and so on, to ensure that the output can be
#' supplied to other functions generically.
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate_if
#' @importFrom dplyr as_tibble
#' @importFrom readr read_csv
#' @importFrom readr read_delim
#' @importFrom readr locale
#' @importFrom readr cols
#' @importFrom data.table setDT
#' @importFrom data.table set
#' @importFrom utils read.delim
#'
#' @param path File path to target.
#' Defaults to current directory.
#' @param file_prefix File name prefix.
#' Defaults to empty string.
#' @param units Snapshot ID e.g. 20180426.
#' @param file_suffix File name suffix.
#' Defaults to empty string.
#' @param file_type Input file type. Currently takes in .txt and .csv.
#' Defaults to .txt.
#' @param col_classes A list of column classes for faster import, if available.
#' Defaults to NULL.
#' @param n_max Maximum number of records to import.
#' Defaults to Inf.
#' @param enc Encoding. Default is ISO-8859-1, to account for certain names.
#' @param del Delimiter. Default is tab.
#' @param quote Set of quoting characters. Default is an empty string.
#' @param col_names Whether column names exist in the dataframe.
#' Defaults to TRUE.
#' @param na NA characters to be interpreted.
#' Defaults to empty string.
#' @param ref Reference for snapshot imports when more than two are imported.
#' Defaults to English lowercase alphabets.
#' @param package Delimited file reading package. Defaults to `readr` read_delim
#' but can be `base` read.delim.
#' @param ... Other arguments to be passed to
#' readr::read:csv or readr::read_delim
#'
#' @return A list of dataframes of the desired data. If only a single unit is
#' input, a single dataframe.
#'
#' @export

snapshot_import <- function(path = ".",
                            file_prefix = "",
                            units,
                            file_suffix = "",
                            file_type = ".txt",
                            col_classes = NULL,
                            n_max = Inf,
                            enc = "ISO-8859-1",
                            del = "\t",
                            quote = "",
                            col_names = TRUE,
                            na = "",
                            ref = NULL,
                            package = "readr",
                            ...) {
  out <- list()
  if (!(file_type %in% c(".csv", ".txt"))) {
    stop("The input must be either .txt or .csv.")
  }
  if (length(units) == 0) {
    stop("The snapshot to be imported must be specified.")
  }
  if (is.null(col_classes) & package == "readr") {
    col_classes <- cols(.default = "c")
  }
  if (is.null(col_classes) & package == "base") {
    col_classes <- "character"
  }
  if (is.null(ref) & length(units) <= 26) {
    ## Lowercase alphabets.
    ## Snapshot {t-1} vs. {t} will be listed as `dfA` vs. `dfB`.
    ref <- letters
  } else if (
    (is.null(ref) & length(units) > 26) |
      length(units) > length(ref)
  ) {
    stop("Supply the function with longer generic references.")
  }
  for (unit in units) {
    print(paste0("Data import for ", unit, " will be executed."))
    file_path <- list.files(
      path,
      full.names = TRUE,
      pattern = paste0(
        "^", file_prefix, ".*", unit, ".*",
        file_suffix, ".*", file_type, "$"
      )
    )
    if (length(file_path) > 1) {
      stop("There are multiple files with the given prefix/suffix/unit.")
    } else if (length(file_path) == 0) {
      stop("There are no files with the given prefix/suffix/unit.")
    }
    if (package == "readr") {
      df <- read_delim(
        file = file_path,
        delim = del,
        col_names = col_names,
        col_types = col_classes,
        trim_ws = TRUE,
        quote = quote,
        locale = locale(encoding = enc),
        na = na,
        n_max = n_max,
        ...
      )
    } else if (package == "base") {
      df <- read.delim(
        file = file_path,
        header = FALSE,
        sep = del,
        quote = quote,
        fileEncoding = enc,
        skipNul = TRUE,
        col.names = col_names,
        colClasses = col_classes,
        comment.char = "",
        stringsAsFactors = FALSE,
        na.strings = na,
        fill = TRUE
      )
    }
    ## Trim the whitespace
    df %>% mutate_if(is.factor, as.character) -> df
    setDT(df)
    for (j in names(df)) {
      if (class(df[[j]]) == "character") {
        set(df, j = j, value = trimws(df[[j]]))
      }
    }
    df <- as_tibble(df)
    out[[paste0("df", toupper(ref)[which(unit == units)])]] <- df
    print(paste0("Data import for ", unit, " is finished."))
  }
  if (length(units) == 1) {
    return(df)
  } else {
    return(out)
  }
}
