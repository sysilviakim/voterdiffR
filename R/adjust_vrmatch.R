#' Adjust for Duplicates and False Negatives for All Snapshots
#'
#' This function applies and `adjust_fn` and `adjust_dups` to all snapshots
#' specified, and exports the adjusted match, corresponding changes, and
#' summaries of these changes.
#'
#' The function should not be pre-applied if the user intends to do a
#' performance evaluation.
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_indices
#' @importFrom dplyr n
#' @importFrom dplyr everything
#' @importFrom dplyr tibble
#' @importFrom assertthat assert_that
#'
#' @param dedup_ids The ID variables to correct for duplicates.
#' Default is c("lVoterUniqueID", "sAffNumber").
#' @param fn_ids The ID variables to correct for false negatives.
#' Default is c("lVoterUniqueID", "sAffNumber").
#' @param adj_prefix File prefix for saving deduped objects for all matches,
#' changes, and reports. Defaults to "dedup_". If set to empty string as well
#' as adj_suffix, this will overwrite the existing pre-deduplication outputs,
#' and this must be done with caution.
#' @param adj_suffix File suffix for saving deduped objects.
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
#' @param file_type_snapshot File type for `snapshot_list()`.
#' Defaults to .txt.
#' @param file_type_cleaned File type for `clean_import()`.
#' Defaults to .Rda.
#' @param id How the snapshot files are formatted/labelled for their IDs.
#' Defaults to mdy.
#' @param rec Whether to find files recursively.
#' Defaults to FALSE.
#' @param per Period of each snapshot---whether daily, weekly, and so on.
#' Defaults to 1 (equivalent to "day"). Any valid input for base seq.Date
#' by argument is allowed.
#' @param prefix File name prefix.
#' Defaults to Cntywd_.
#' @param path_changes Path where the extracted changes are output to.
#' Defaults to "changes".
#' @param path_reports Path where the summarized changes are output to.
#' Defaults to "reports".
#' @param path_matches Path where the match outcomes are output to.
#' Defaults to "matches".
#' @param vars_change Variables to track changes of.
#' Defaults to NULL, which will then track all variables.
#' @param date_label Labels for dates (i.e., snapshot IDs), in `date_df`.
#' Defaults to "date_label".
#' @param nrow Name of list element which will contain the number of rows
#' of the input list dataframes.
#' @param path_clean Path to the cleaned snapshots.
#' Defaults to "clean_df".
#' @param clean_prefix File prefixes for cleaned snapshots.
#' Defaults to "df_cleaned_".
#' @param clean_suffix File suffixes for cleaned snapshots.
#' Defaults to empty string.
#'
#' @return A named list of dataframes similar to vrmatch output but with
#' perfect duplicates by the matching variables cleaned.
#'
#' @export

adjust_vrmatch <- function(dedup_ids = c("lVoterUniqueID", "sAffNumber"),
                           fn_ids = c("lVoterUniqueID", "sAffNumber"),
                           adj_prefix = "adj_",
                           adj_suffix = "",
                           date_df = NULL,
                           start = "2018-04-26",
                           end = "2021-01-01",
                           path = "7z",
                           pattern = "^(?=.*Cntywd_)(?!.*Hist)",
                           file_type_snapshot = ".txt",
                           file_type_cleaned = ".Rda",
                           id = "%m%d%y",
                           rec = FALSE,
                           per = 1,
                           prefix = "Cntywd_",
                           path_changes = "changes",
                           path_reports = "reports",
                           path_matches = "matches",
                           vars_change = NULL,
                           date_label = "date_label",
                           nrow = "nrow",
                           path_clean = "clean_df",
                           clean_prefix = "df_cleaned_",
                           clean_suffix = "") {
  . <- NULL
  if (adj_prefix != "" & !grepl("_$", adj_prefix)) {
    adj_prefix <- paste0(adj_prefix, "_")
  }
  if (adj_suffix != "" & !grepl("^_", adj_suffix)) {
    adj_suffix <- paste0("_", adj_suffix)
  }
  if (is.null(date_df)) {
    print("Dedup all snapshot matches.")
    date_df <- snapshot_list(
      start = start, end = end, path = path, pattern = pattern, prefix = prefix,
      file_type = file_type_snapshot, id = id, rec = rec, per = per
    )
  }
  final_report <- list()
  for (i in seq(nrow(date_df) - 1)) {
    day1 <- date_df[[date_label]][i]
    day2 <- date_df[[date_label]][i + 1]
    orig <- clean_import(
      path_clean, clean_prefix, clean_suffix, day1, day2, file_type_cleaned
    )
    load(file.path(path_matches, paste0("match_", day1, "_", day2, ".Rda")))
    adj_match <- adjust_fn(match = match, fn_ids = fn_ids)
    adj_match <- adjust_dups(match = adj_match, dedup_ids = dedup_ids)
    lapply(dedup_ids, function(x) assert_adj_match(adj_match, orig, x))
    save(
      adj_match,
      file = file.path(
        path_matches,
        paste0(adj_prefix, "match_", day1, "_", day2, adj_suffix, ".Rda")
      )
    )
    ## changes_prev <-
    ##   changes_extract(match, varnames = vars_change, nrow = nrow)
    adj_changes <- changes_extract(
      adj_match,
      varnames = vars_change, nrow = nrow
    )
    save(
      adj_changes,
      file = file.path(
        path_changes,
        paste0(adj_prefix, "change_", day1, "_", day2, ".Rda")
      )
    )
    ## report_prev <- changes_report(changes_prev, vars_change, nrow = nrow)
    adj_report <- changes_report(adj_changes, vars_change, nrow = nrow)
    ## print(paste0("Change summaries for ", day1, " and ", day2, ":"))
    save(
      adj_report,
      file = file.path(
        path_reports,
        paste0(adj_prefix, "table_", day1, "_", day2, ".Rda")
      )
    )
    final_report[[day2]] <- adj_report
    gc(reset = TRUE)
    print(
      paste0(
        "vrmatch adjusted for snapshot matching between ",
        day1, " and ", day2, "."
      )
    )
  }
  final_report <- final_report %>%
    bind_rows(.id = "date_origin")
  return(final_report)
}

assert_adj_match <- function(adj_match, orig, id) {
  ## Validate the number of rows.
  assert_that(
    nrow(adj_match$data$changed_A) == nrow(adj_match$data$changed_B)
  )
  assert_that(
    nrow(adj_match$data$exact_match) +
      nrow(adj_match$data$id_match_A) +
      nrow(adj_match$data$changed_A) +
      nrow(adj_match$data$only_A) ==
      nrow(orig$dfA)
  )
  assert_that(
    nrow(adj_match$data$exact_match) +
      nrow(adj_match$data$id_match_B) +
      nrow(adj_match$data$changed_B) +
      nrow(adj_match$data$only_B) ==
      nrow(orig$dfB)
  )
  assert_that(
    length(setdiff(
      orig$dfA[, id],
      c(
        adj_match$data$exact_match[, id], adj_match$data$id_match_A[, id],
        adj_match$data$changed_A[, id], adj_match$data$only_A[, id]
      )
    )) == 0
  )
  assert_that(
    length(setdiff(
      orig$dfB[, id],
      c(
        adj_match$data$exact_match[, id], adj_match$data$id_match_B[, id],
        adj_match$data$changed_B[, id], adj_match$data$only_B[, id]
      )
    )) == 0
  )
}
