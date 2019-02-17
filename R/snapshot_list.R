#' List the Available Snapshots
#'
#' This function lists currently available snapshots, assuming a common
#' file pattern and snapshot IDs by date. The current default values are
#' tailored to the Orange County setup.
#'
#' @import dplyr
#' @importFrom lubridate wday
#'
#' @param start The start date of the first snapshot.
#' Defaults to April 26, 2018.
#' @param end The end date of the last snapshot.
#' Defaults to Jan 1, 2021.
#' @param path Path where all snapshots are stored.
#' Defaults to subfolder 7z.
#' @param pattern Regular expression of the file pattern to find.
#' Defaults to a particular pattern of OCROV files.
#' @param enc File encoding.
#' Defaults to .txt.
#' @param id How the snapshot files are formatted/labelled for their IDs.
#' Defaults to mdy.
#' @param rec Whether to find files recursively.
#' Defaults to FALSE.
#' @param per Period of each snapshot---whether daily, weekly, and so on.
#' Defaults to 1 (equivalent to "day"). Any valid input for base seq.Date
#' by argument is allowed.
#' @param prefix File name prefix.
#' Defaults to Cntywd_.
#'
#' @return A dataframe that contains available snapshots.
#'
#' @export

snapshot_list <- function(start = "2018-04-26",
                          end = "2021-01-01",
                          path = "7z",
                          pattern = "^(?=.*Cntywd_)(?!.*Hist)",
                          enc = ".txt",
                          id = "%m%d%y",
                          rec = FALSE,
                          per = 1,
                          prefix = "Cntywd_") {
  date_label <- NULL
  date_df <-
    data.frame(
      date = seq(as.Date(start), as.Date(end), by = per)
    ) %>%
    dplyr::mutate(
      weekday = wday(date, label = TRUE),
      date_label = format(date, id)
    ) %>%
    dplyr::filter(
      date_label %in% substr(
        list.files(file.path(path), pattern = enc, recursive = rec)[
          grepl(
            list.files(file.path(path), pattern = enc, recursive = rec),
            pattern = pattern,
            perl = TRUE
          )
        ],
        nchar(prefix) + 1,
        nchar(prefix) + nchar(id)
      )
    )
  return(date_df)
}
