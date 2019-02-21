#' Correct False Negatives in a vrmatch Output for All Snapshots
#'
#' This function takes the `vrmatch` output and using a voter ID variable,
#' re-matches the nonmatches that have the same ID back as a match---i.e.,
#' correcting the false negatives. This is sometimes necessary for real
#' applications, especially when you do not wish to exclude ID matches
#' from probabilistic record linkage.
#'
#' Note that this function is only relevant when using `vrmatch`
#' when the reference ID was not used to exclude exact matches. In addition,
#' after the correction, the EM object and et cetera from fastLink has
#' not been corrected accordingly and should not be used in inference.
#'
#' Also note that this assumes that if a voter ID matches, it is never a false
#' positive.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr row_number
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom assertthat assert_that
#'
#' @param match The vrmatch output to correct for false negatives.
#' @param fn_ids Voter IDs used in detecting and correcting false negatives.
#'
#' @return Corrected vrmatch output.
#'
#' @export

adjust_fn <- function(match, fn_ids = c("lVoterUniqueID", "sAffNumber")) {
  for (id in fn_ids) {
    ## Case 1 (false positives involved)
    x <- c(
      intersect(match$data$changed_A[[id]], match$data$only_B[[id]]),
      intersect(match$data$only_A[[id]], match$data$changed_B[[id]])
    )
    if (length(x) > 0) {
      y <- (
        match$data$changed_A %>% ungroup() %>% mutate(rownames = row_number())
      )[na.omit(match(x, match$data$changed_A[[id]])), ]$rownames
      ## Correct A
      match$data$only_A <- bind_rows(
        match$data$only_A, match$data$changed_A[y, ]
      )
      match$data$changed_A <- match$data$changed_A[-y, ]
      ## Correct B
      match$data$only_B <- bind_rows(
        match$data$only_B, match$data$changed_B[y, ]
      )
      match$data$changed_B <- match$data$changed_B[-y, ]
    }
    ## Case 2
    z <- intersect(match$data$only_A[[id]], match$data$only_B[[id]])
    if (length(z) > 0) {
      ## Correct A
      match$data$changed_A <- bind_rows(
        match$data$changed_A,
        match$data$only_A[na.omit(match(z, match$data$only_A$lVoterUniqueID)), ]
      )
      match$data$only_A <-
        match$data$only_A[-na.omit(match(z, match$data$only_A$lVoterUniqueID)),]
      ## Correct B
      match$data$changed_B <- bind_rows(
        match$data$changed_B,
        match$data$only_B[na.omit(match(z, match$data$only_B$lVoterUniqueID)), ]
      )
      match$data$only_B <-
        match$data$only_B[-na.omit(match(z, match$data$only_B$lVoterUniqueID)),]
    }
    ## Validate the changed results
    assert_that(
      length(intersect(match$data$only_A[[id]], match$data$only_B[[id]])) == 0
    )
    assert_that(
      nrow(match$data$changed_A) + nrow(match$data$only_A) ==
        nrow(match$data$changed_A) + nrow(match$data$only_A)
    )
    assert_that(
      nrow(match$data$changed_B) + nrow(match$data$only_B) ==
        nrow(match$data$changed_B) + nrow(match$data$only_B)
    )
    print(paste0(
      length(c(x, z)), " cases of false negatives re-matched with ", id, "."
    ))
  }
  return(match)
}
