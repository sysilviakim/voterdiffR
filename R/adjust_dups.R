#' Dedupe Ties in a vrmatch Output
#'
#' This function takes the `vrmatch` output and deduplicates the probabilistic
#' record linkage output.
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
#' However, for practical applications, we must sometimes correct
#' for these duplicates, to see which observations have truly changed.
#'
#' In this function, we use the internal voter ID to correct for these
#' duplicates. Suppose that in snapshot A there are records with IDs a1 and a2
#' the same name, address, and date of birth, and in snapshot B, the duplicate
#' a2 has been deleted by the Registrar of Voters. Between two matches
#' (1) a1-a1 and (2) a2-a1, we drop (2), and classify a2 as "record only in A".
#'
#' Sometimes, the following cases happen: a2-a3 vs. a1-a1.
#' But in fact, there is a2 in `only_B`: it was just pushed aside by a1 because
#' it had the same values in the fields called for matching.
#' Hence we recommend calling this function after correcting for nonmatches
#' that in fact have the same internal IDs (i.e., false negatives).
#'
#' Note that this function is only relevant when using `vrmatch`
#' when the reference ID was not used to exclude exact matches. In addition,
#' after the correction, the EM object and et cetera from fastLink has
#' not been corrected accordingly and should not be used in inference.
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
#' @importFrom assertthat assert_that
#'
#' @param match The vrmatch output to correct for false negatives.
#' @param dedup_ids Voter IDs used in detecting and correcting false negatives.
#'
#' @return Corrected vrmatch output.
#'
#' @export

adjust_dups <- function(match, dedup_ids = "lVoterUniqueID") {
  . <- group_id <- NULL
  vars_all <- match$args$vars_all
  orig_match <- match
  for (dedup_id in dedup_ids) {
    x <- (
      match$data$changed_B %>%
        ungroup() %>%
        mutate(row = row_number()) %>%
        group_by(!!as.name(dedup_id)) %>%
        filter(n() > 1)
    )$row
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
      ## Is there, for groups in tempA, ID that does not match in corresp. B?
      ## i.e., a2-a1 vs. a1-a1 ---> find the a2.
      y <- (
        tempA %>%
          ungroup() %>%
          mutate(row = row_number()) %>%
          filter(!(!!as.name(dedup_id) %in% tempB[[dedup_id]]))
      )$row
      ## If adjust_fn has been applied the following holds true.
      assert_that(
        sum(!(unique(tempB[[dedup_id]]) %in% tempA[[dedup_id]])) == 0
      )
      ## Correct A
      match$data$only_A <- bind_rows(
        match$data$only_A, match$data$changed_A[x[y], ]
      )
      match$data$changed_A <- match$data$changed_A[-x[y], ]
      ## Correct B
      match$data$changed_B <- match$data$changed_B[-x[y], ]
      ## Validate the changed results
      assert_that(
        sum(duplicated(match$data$changed_B[[dedup_id]])) == 0
      )
      print(paste0(length(x[y]), " cases of duplicates adjusted."))
    }
  }
  assert_that(
    nrow(match$data$changed_A) + nrow(match$data$only_A) ==
      nrow(orig_match$data$changed_A) + nrow(orig_match$data$only_A)
  )
  return(match)
}
