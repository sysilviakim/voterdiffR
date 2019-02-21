#' Dedupe Ties in vrmatch Output for All Snapshots
#'
#' This function takes the `vrmatch` output and deduplicates the probabilistic
#' record linkage output for all user-requested snapshots.
#'
#' This happens because (1) the snapshot A was not deduplicated, but (2) when
#' snapshot B was. Hence, the old duplicates in A that are exact matches
#' with records in B (in terms of matching variables, not necessarily all)
#' force the remaining records in B to duplicated to be matched
#' to all the duplicates in A.
#'
#' For instance, if you asked `dfA[c(1, 1, 2), ]` and `dfA[c(1, 2)]` to be
#' matched, it will give you three matched outcomes: `dfA[c(1, 1, 2), ]`,
#' even when `fastLink::dedupeMatches` has been called for.
#' It should be stressed that this is not a bug of fastLink, as it has
#' no means to distinguish these perfect ties.
#'
#' In this function, we use the internal voter ID to correct for these
#' duplicates. Suppose that in snapshot A there are records with IDs a1 and a2
#' the same name, address, and date of birth, and in snapshot B, the duplicate
#' a2 has been deleted by the Registrar of Voters. Between two matches
#' (1) a1-a1 and (2) a2-a1, we drop (2), and classify a2 as "record only in A".
#'
#' Note that this function is only relevant when using `vrmatch`
#' when the reference ID was not used to exclude exact matches. In addition,
#' after the deduplication, the EM object and et cetera from fastLink is
#' pre-deduplication, and hence cannot be used with the deduped `vrmatch`
#' object.
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
#' @param dedup_id The internal ID to correct for duplicates.
#' Default is "lVoterUniqueID".
#' @param dedup_prefix File prefix for saving deduped objects for all matches,
#' changes, and reports. Defaults to "dedup_". If set to empty string as well
#' as dedup_suffix, this will overwrite the existing pre-deduplication outputs,
#' and this must be done with caution.
#' @param dedup_suffix File suffix for saving deduped objects.
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
#'
#' @return A named list of dataframes similar to vrmatch output but with
#' perfect duplicates by the matching variables cleaned.
#'
#' @export

adjust_match_dedup <- function(dedup_id = "lVoterUniqueID",
                               dedup_prefix = "dedup_",
                               dedup_suffix = "",
                               date_df = NULL,
                               start = "2018-04-26",
                               end = "2021-01-01",
                               path = "7z",
                               pattern = "^(?=.*Cntywd_)(?!.*Hist)",
                               file_type = ".txt",
                               id = "%m%d%y",
                               rec = FALSE,
                               per = 1,
                               prefix = "Cntywd_",
                               path_changes = "changes",
                               path_reports = "reports",
                               path_matches = "matches",
                               vars_change = NULL,
                               date_label = "date_label",
                               nrow = "nrow") {
  . <- NULL
  if (dedup_prefix != "" & grepl("_$", dedup_prefix)) {
    dedup_prefix <- paste0(dedup_prefix, "_")
  }
  if (dedup_suffix != "" & grepl("^_", dedup_suffix)) {
    dedup_suffix <- paste0("_", dedup_suffix)
  }
  if (is.null(date_df)) {
    print("Dedup all snapshot matches.")
    date_df <- snapshot_list(
      start = start, end = end, path = path, pattern = pattern,
      file_type = file_type, id = id, rec = rec, per = per, prefix = prefix
    )
  }
  final_report <- list()
  for (i in seq(nrow(date_df) - 1)) {
    day1 <- date_df[[date_label]][i]
    day2 <- date_df[[date_label]][i + 1]
    load(file.path(path_matches, paste0("match_", day1, "_", day2, ".Rda")))
    deduped <- dedup_vrmatch(match = match, dedup_id = dedup_id)
    save(
      deduped,
      file = file.path(
        path_matches,
        paste0(dedup_prefix, "match_", day1, "_", day2, dedup_suffix, ".Rda")
      )
    )
    changes_prev <- changes_extract(match, varnames = vars_change, nrow = nrow)
    changes <- changes_extract(deduped, varnames = vars_change, nrow = nrow)
    save(
      changes,
      file = file.path(
        path_changes,
        paste0(dedup_prefix, "change_", day1, "_", day2, ".Rda")
      )
    )
    report_prev <- changes_report(changes_prev, vars_change, nrow = nrow)
    report <- changes_report(changes, vars_change, nrow = nrow)
    print(paste0("Change summaries for ", day1, " and ", day2, ":"))
    print(report)
    save(
      report,
      file = file.path(
        path_reports,
        paste0(dedup_prefix, "table_", day1, "_", day2, ".Rda")
      )
    )
    final_report[[day2]] <- report
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

dedup_vrmatch <- function(match, dedup_id = "lVoterUniqueID") {
  . <- group_id <- NULL
  print(unlist(lapply(match$data, nrow)))
  vars_all <- match$args$vars_all
  x <- (
    match$data$changed_B %>%
      ungroup() %>%
      mutate(row = row_number()) %>%
      group_by(!!as.name(dedup_id)) %>%
      filter(n() > 1)
  )$row
  out <- match
  if (length(x) > 0) {
    tempA <- match$data$changed_A[x, ] %>%
      mutate(
        group_id = group_indices(., !!as.name(vars_all))
      ) %>%
      select(
        group_id, everything()
      )
    tempB <- match$data$changed_B[x, ] %>%
      mutate(
        group_id = group_indices(., !!as.name(vars_all))
      ) %>%
      select(
        group_id, everything()
      )
    ## Is there, for groups in tempA, ID that does not match in corresponding B?
    ## i.e., a2-a1 vs. a1-a1 ---> find the a2.
    y <- (
      tempA %>%
        ungroup() %>%
        mutate(row = row_number()) %>%
        filter(!(!!as.name(dedup_id) %in% tempB[[dedup_id]]))
    )$row
    ## So that we do not have completely different cases matched:
    ## a3-a4 vs. a1-a1.
    assert_that(
      sum(!(unique(tempB[[dedup_id]]) %in% tempA[[dedup_id]])) == 0
    )
    ## Correct A
    out$data$only_A <- bind_rows(
      out$data$only_A, out$data$changed_A[x[y], ]
    )
    out$data$changed_A <- out$data$changed_A[-x[y], ]
    ## Correct B
    out$data$changed_B <- out$data$changed_B[-x[y], ]
    ## Validate the changed results
    assert_that(
      sum(duplicated(out$data$changed_B[[dedup_id]])) == 0
    )
    assert_that(
      nrow(match$data$changed_A) + nrow(match$data$only_A) ==
        nrow(out$data$changed_A) + nrow(out$data$only_A)
    )
    print(paste0(length(x[y]), " cases cleaned."))
  }
  return(out)
}
