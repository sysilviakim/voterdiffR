#' Voter Registration Database Snapshot Matching
#'
#' This function performs probabilistic record linkage between all user-supplied
#' consecutive snapshots of the voter file. Note that the default option is to
#' exclude exact matches of all fields between two snapshots when performing
#' the record linkage, for computational reasons.
#'
#' @import dplyr
#' @importFrom parallel detectCores
#' @importFrom assertthat assert_that
#' @importFrom fastLink fastLink
#' @importFrom lubridate is.Date
#' @importFrom lubridate is.POSIXt
#' @importFrom fst read.fst
#'
#' @param date_df Dataframe of list of snapshots.
#' @param exact_exclude Whether to exclude full exact matches between snapshots
#' when doing probabilistic record linkage. Defaults to TRUE.
#' @param sample Whether to add random samples of full exact matches or ID
#' matches to correct for underlying population's value distributions for
#' each field. Defaults to FALSE.
#' @param block Whether to employ blocking.
#' Defaults to FALSE.
#'
#' @param path_clean Path to the cleaned snapshots.
#' Defaults to "clean_df".
#' @param path_changes Path where the extracted changes are output to.
#' Defaults to "changes".
#' @param path_reports Path where the summarized changes are output to.
#' Defaults to "reports".
#' @param path_matches Path where the match outcomes are output to.
#' Defaults to "matches".
#'
#' @param clean_prefix File prefixes for cleaned snapshots.
#' Defaults to "df_cleaned_".
#' @param clean_suffix File suffixes for cleaned snapshots.
#' Defaults to empty string.
#' @param exist_files Whether previously performed match outcomes exist.
#' Defaults to FALSE.
#'
#' @param varnames Variables to perform probabilistic record linkage.
#' @param varnames_str String variables for matching.
#' @param varnames_num Numeric variables for matching.
#' Defaults to NULL, in which case it will be setdiff(varnames, varnames_str).
#' @param varnames_id Voter IDs variables, if any exists, and is to be excluded
#' from PRL when IDs match.
#' @param partial.match Variables to be partially matched.
#' Defaults to all varnames_str.
#' @param varnames_block Nested list of variables or their combinations
#' for blocking passes.
#' @param vars_change Variables to track changes of.
#' Defaults to NULL, which will then track all variables.
#'
#' @param n.cores Number of cores to parallelize the matching.
#' Defaults to half the existing threads.
#' @param file_type Input file types.
#' Defulats to .Rda.
#' @param date_label Labels for dates (i.e., snapshot IDs), in `date_df`.
#' Defaults to "date_label".
#' @param nrow Name of list element which will contain the number of rows
#' of the input list dataframes.
#' @param seed Seed to set. Defaults to 123.
#' @param ... Other parameters for fastLink.
#'
#' @return A nested list of matched dataframes, fastLink output, and arguments.
#'
#' @export

vrmatch <- function(date_df,
                    exact_exclude = TRUE,
                    sample = FALSE,
                    block = FALSE,
                    path_clean = "clean_df",
                    path_changes = "changes",
                    path_reports = "reports",
                    path_matches = "matches",
                    clean_prefix = "df_cleaned_",
                    clean_suffix = "",
                    exist_files = FALSE,
                    varnames,
                    varnames_str,
                    varnames_num = NULL,
                    varnames_id = NULL,
                    partial.match = NULL,
                    varnames_block = NULL,
                    vars_change = NULL,
                    n.cores = NULL,
                    file_type = ".Rda",
                    date_label = "date_label",
                    nrow = "nrow",
                    seed = 123,
                    ...) {
  set.seed(seed)
  if (!is.null(varnames_str) &
    sum(!(varnames_str %in% varnames)) != 0) {
    stop("String variables list is not a subset of the variable list.")
  }
  if (!is.null(partial.match) &
    sum(!(partial.match %in% varnames_str)) != 0) {
    stop("Partial match list is not a subset of the string variables list.")
  }
  if (is.null(partial.match)) {
    partial.match <- varnames_str
  }
  if (is.null(varnames_num)) {
    varnames_num <- setdiff(varnames, varnames_str)
  }
  if (is.null(n.cores)) {
    n.cores <- detectCores() / 2
    if (is.na(n.cores)) {
      n.cores <- 2
    }
  }
  for (p in c(path_changes, path_reports, path_matches)) {
    if (!dir.exists(file.path(p))) {
      dir.create(p)
    }
  }
  final_report <- tibble()
  for (i in 1:(nrow(date_df) - 1)) {
    day1 <- date_df[[date_label]][i]
    day2 <- date_df[[date_label]][i + 1]
    if (
      exist_files == TRUE &
        file.exists(file.path(
          path_matches, paste0("match_", day1, "_", day2, ".Rda")
        ))
    ) {
      ## If there is already a match output, load it
      load(file.path(path_matches, paste0("match_", day1, "_", day2, ".Rda")))
      print(paste0("Matched dataframes loaded for ", day1, " and ", day2, "."))
    } else {
      ## If this is a new match to be, load the cleaned dataframes.
      orig <- clean_import(
        path_clean, clean_prefix, clean_suffix, day1, day2, file_type
      )
      ## Perform full exact matching if requested to exclude them from PRL.
      inter <- exact_match(orig, "dfA", "dfB", exact_exclude)
      if (!is.null(varnames_id)) {
        inter <- id_match(inter, ids = varnames_id)
      } else {
        inter$id_match_A <- inter$id_match_B <- inter$exact_match[0, ]
      }
      assert_inter(inter, orig)
      inter <- lapply(
        inter, function(x) x %>% dplyr::mutate(row_id = dplyr::row_number())
      )
      print("The interim list has the following number of rows: ")
      print(unlist(lapply(inter, nrow)))
      ## If there are less than three rows in each database, don't match.
      ## Regard them as nonmatches. This is especially because in tableCounts,
      ## if these few obs have many NA in fields or
      ## there are no underlying true matches, fastLink breaks.
      ## In these extreme small obs cases, makes less sense to do PRL as well.
      runtime <- f.out <- NULL
      if (nrow(inter$mismatch_A) > 2 & nrow(inter$mismatch_B) > 2) {
        tryCatch({
          runtime <- system.time({
            f.out <-
              fastLink(
                dfA = inter$mismatch_A,
                dfB = inter$mismatch_B,
                varnames = varnames,
                stringdist.match = varnames_str,
                numeric.match = varnames_num,
                partial.match = partial.match,
                n.cores = n.cores,
                ...
              )
          })
        }, error = function(e) {
          message(paste0(e, "\n"))
        })
        print("fastLink running is complete.")
        match <- match_out(inter, f.out)
      } else {
        print("There are too few obs. in records to match. Abort matching.")
        match <- match_none(inter)
      }
      ## Delete mismatches, which are interim objects.
      match$data$mismatch_A <- match$data$mismatch_B <- NULL
      match$args <- list(
        vars_all = varnames, vars_str = varnames_str, vars_num = varnames_num,
        vars_id = varnames_id, vars_partial = partial.match,
        vars_block = varnames_block, vars_change = vars_change,
        path_matches = path_matches, path_changes = path_changes,
        path_reports = path_reports, seed = seed
      )
      match$runtime <- runtime
      save(
        match,
        file = file.path(
          path_matches, paste0("match_", day1, "_", day2, ".Rda")
        )
      )
    }
    ## Track changes and summarize them.
    changes <- changes_extract(match, varnames = vars_change, nrow = nrow)
    print("Changes are extracted.")
    save(
      changes,
      file = file.path(
        path_changes, paste0("change_", day1, "_", day2, ".Rda")
      )
    )
    tbl <- changes_report(changes, vars_change, nrow = nrow)
    print(paste0("Change summaries for ", day1, " and ", day2, ":"))
    print(tbl)
    save(
      tbl,
      file = file.path(
        path_reports, paste0("table_", day1, "_", day2, ".Rda")
      )
    )
    ## Report is appended with each snapshot matching.
    final_report <- dplyr::bind_rows(
      final_report,
      tbl %>% dplyr::mutate(date_origin = day2)
    )
  }
  gc(reset = TRUE)
  return(final_report)
}





## Internal helper functions ===================================================
clean_import <- function(path_clean,
                         clean_prefix,
                         clean_suffix,
                         day1,
                         day2,
                         file_type = ".Rda") {
  orig <- list()
  if (tolower(file_type) == ".rda") {
    load(
      file.path(path_clean, paste0(clean_prefix, day1, clean_suffix, ".Rda"))
    )
  } else if (tolower(file_type) == ".fst") {
    df <- read.fst(
      file.path(path_clean, paste0(clean_prefix, day1, clean_suffix, ".fst"))
    )
  }
  ## Results differ when variable is Date or POSIXt.
  orig[["dfA"]] <- df %>%
    dplyr::mutate_if(is.Date, as.numeric) %>%
    dplyr::mutate_if(is.POSIXt, function(x) as.numeric(as.Date(x)))
  if (tolower(file_type) == ".rda") {
    load(
      file.path(path_clean, paste0(clean_prefix, day2, clean_suffix, ".Rda"))
    )
  } else if (tolower(file_type) == ".fst") {
    df <- read.fst(
      file.path(path_clean, paste0(clean_prefix, day2, clean_suffix, ".fst"))
    )
  }
  orig[["dfB"]] <- df %>%
    dplyr::mutate_if(is.Date, as.numeric) %>%
    dplyr::mutate_if(is.POSIXt, function(x) as.numeric(as.Date(x)))
  print(paste0("Cleaned dataframes loaded for ", day1, " and ", day2, "."))
  return(orig)
}

assert_inter <- function(inter, orig) {
  ## Validate the number of rows.
  assert_that(
    nrow(inter$exact_match) + nrow(inter$mismatch_A) +
      nrow(inter$id_match_A) == nrow(orig$dfA)
  )
  assert_that(
    nrow(inter$exact_match) + nrow(inter$mismatch_B) +
      nrow(inter$id_match_B) == nrow(orig$dfB)
  )
}

match_out <- function(inter, f.out) {
  match <- list(data = inter, matches.out = f.out)
  match$data$changed_A <-
    match$data$mismatch_A[f.out$matches$inds.a, ]
  match$data$only_A <-
    match$data$mismatch_A[-f.out$matches$inds.a, ]
  match$data$changed_B <-
    match$data$mismatch_B[f.out$matches$inds.b, ]
  match$data$only_B <-
    match$data$mismatch_B[-f.out$matches$inds.b, ]
  return(match)
}

match_none <- function(inter) {
  match <- list(data = inter, f.out = NULL)
  match$data$changed_A <- match$data$changed_B <- inter$exact_match[0, ]
  match$data$only_A <- match$data$mismatch_A
  match$data$only_B <- match$data$mismatch_B
  return(match)
}