#' Clean Snapshots and Store Them
#'
#' This function takes requested snapshots, import them, clean them, and
#' export them into Rda and/or fst objects for future calls by vrmatch function.
#'
#' @importFrom dplyr "%>%"
#' @importFrom Kmisc clean_vars
#' @importFrom fst write.fst
#'
#' @param date_df List of snapshots. Defaults to NULL,
#' in which case the function will detect all snapshots available.
#' @param start The start date of the first snapshot.
#' Defaults to April 26, 2018.
#' @param end The end date of the last snapshot.
#' Defaults to Jan 1, 2021.
#' @param path Path where all snapshots are stored.
#' Defaults to subfolder 7z.
#' @param pattern Regular expression of the file pattern to find.
#' Defaults to a particular pattern of OCROV files.
#' @param file_type File type.
#' Defaults to .txt.
#' @param path_clean Path where cleaned snapshots would be stored.
#' Defaults to "clean_df".
#' @param clean_prefix File prefixes for cleaned snapshots.
#' This replaces the existing file prefix. Defaults to "df_cleaned_".
#' @param clean_suffix File suffixes for cleaned snapshots.
#' Defaults to empty string.
#' @param save_type How to export the cleaned dataframe.
#' Defaults to Rda and fst.
#' @param id How the snapshot files are formatted/labelled for their IDs.
#' Defaults to mdy.
#' @param rec Whether to find files recursively.
#' Defaults to FALSE.
#' @param per Period of each snapshot---whether daily, weekly, and so on.
#' Defaults to 1 (equivalent to "day"). Any valid input for base seq.Date
#' by argument is allowed.
#' @param prefix File name prefix.
#' Defaults to Cntywd_.
#' @param varnames All variables to be cleaned.
#' Defaults to NULL.
#' @param varnames_date Date variables.
#' Defaults to NULL.
#' @param date_order Order of the date variable, if string format.
#' @param varnames_num Numeric variables.
#' Defaults to NULL.
#' @param varnames_first Variable containing first names.
#' Defaults to "szNameFirst".
#' @param varname_prefix Variable containing self-reported personal prefixes.
#' Defaults to "sVoterTitle".
#' @param varname_gender Variable containing original gender entry.
#' Defaults to "sGender".
#' @param varname_email Name of the email address field.
#' Defaults to "szEmailAddress".
#' @param email_exc Emails that are to be cleaned.
#' Defaults to a single vector of abc at example.com
#' @param varname_phone Name of the phone number field.
#' Defaults to "szPhone".
#' @param phone_exc Phone numbers that are to be cleaned.
#' Defaults to "___-____".
#' @param ... Other arguments to be passed to snapshot_import.
#'
#' @return Output dataframe with cleaned contacts.
#'
#' @export

clean_snapshot <- function(date_df = NULL,
                           start = "2018-04-26",
                           end = "2021-01-01",
                           path = "7z",
                           pattern = "^(?=.*Cntywd_)(?!.*Hist)",
                           file_type = ".txt",
                           path_clean = "clean_df",
                           clean_prefix = "df_cleaned_",
                           clean_suffix = "",
                           save_type = c("rda", "fst"),
                           id = "%m%d%y",
                           rec = FALSE,
                           per = 1,
                           prefix = "Cntywd_",
                           varnames = NULL,
                           varnames_date = NULL,
                           date_order = "mdy",
                           varnames_num = NULL,
                           varnames_first = "szNameFirst",
                           varname_prefix = "sVoterTitle",
                           varname_gender = "sGender",
                           varname_email = "szEmailAddress",
                           email_exc = c("abc@example.com"),
                           varname_phone = "szPhone",
                           phone_exc = "___-____",
                           ...) {
  . <- NULL
  if (is.null(date_df)) {
    print("Clean all snapshots.")
    date_df <- snapshot_list(
      start = start,
      end = end,
      path = path,
      pattern = pattern,
      file_type = file_type,
      id = id,
      rec = rec,
      per = per,
      prefix = prefix
    )
  }
  for (i in seq(nrow(date_df))) {
    df <-
      snapshot_import(
        path = path,
        prefix = prefix,
        units = date_df$date_label[i],
        file_type = file_type,
        ...
      ) %>%
      clean_vars(
        df = .,
        varnames = varnames,
        varnames_date = varnames_date,
        date_order = date_order,
        varnames_num = varnames_num,
        firstname = varnames_first,
        prefix = varname_prefix,
        gender_original = varname_gender
      ) %>%
      clean_contact(
        df = .,
        email = varname_email,
        email_exc = email_exc,
        phone = varname_phone,
        phone_exc = phone_exc
      )
    if ("rda" %in% tolower(save_type)) {
      save(
        df, file = file.path(
          path_clean,
          paste0(clean_prefix, date_df$date_label[i], clean_suffix, ".Rda")
        )
      )
    }
    if ("fst" %in% tolower(save_type)) {
      write.fst(
        df, path = file.path(
          path_clean,
          paste0(clean_prefix, date_df$date_label[i], clean_suffix, ".fst")
        )
      )
    }
    print(paste("Cleaning for", date_df$date_label[i], "is complete."))
  }
}
