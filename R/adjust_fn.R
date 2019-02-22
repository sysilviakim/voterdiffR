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
#' positive, and that there are no duplicates by `fn_ids`.
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
    match <- fn1(match, id)
    match <- fn2(match, id)
    match <- fn3(match, id)
    ## Validate the changed results
    assert_that(length(
      intersect(match$data$only_A[[id]], match$data$only_B[[id]])
    ) == 0)
    assert_that(length(
      intersect(match$data$changed_A[[id]], match$data$only_A[[id]])
    ) == 0)
    assert_that(length(
      intersect(match$data$changed_B[[id]], match$data$only_B[[id]])
    ) == 0)
    assert_that(length(
      intersect(match$data$changed_B[[id]], match$data$only_B[[id]])
    ) == 0)
    assert_that(nrow(match$data$changed_A) == nrow(match$data$changed_B))
  }
  return(match)
}

fn1 <- function(match, id) {
  ## Case 1-1 (false positives involved)
  x <- intersect(match$data$changed_A[[id]], match$data$only_B[[id]])
  if (length(x) > 0) {
    y <- (
      match$data$changed_A %>% ungroup() %>% mutate(rownames = row_number())
    )[which(match(match$data$changed_A[[id]], x) > 0), ]$rownames
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
  print(paste0(length(x), " cases re-matched with ", id, "."))
  ## Case 1-2
  x <- intersect(match$data$only_A[[id]], match$data$changed_B[[id]])
  if (length(x) > 0) {
    y <- (
      match$data$changed_B %>% ungroup() %>% mutate(rownames = row_number())
    )[which(match(match$data$changed_B[[id]], x) > 0), ]$rownames
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
  return(match)
}

fn2 <- function(match, id) {
  ## Case 2
  match$data$only_A <- match$data$only_A[!duplicated(match$data$only_A), ]
  match$data$only_B <- match$data$only_B[!duplicated(match$data$only_B), ]
  x <- intersect(match$data$only_A[[id]], match$data$only_B[[id]])
  if (length(x) > 0) {
    ## Correct A
    match$data$changed_A <- bind_rows(
      match$data$changed_A,
      match$data$only_A[which(match(match$data$only_A[[id]], x) > 0), ]
    )
    match$data$only_A <-
      match$data$only_A[-which(match(match$data$only_A[[id]], x) > 0),]
    ## Correct B
    match$data$changed_B <- bind_rows(
      match$data$changed_B,
      match$data$only_B[which(match(match$data$only_B[[id]], x) > 0), ]
    )
    match$data$only_B <-
      match$data$only_B[-which(match(match$data$only_B[[id]], x) > 0),]
  }
  print(paste0(length(x), " cases re-matched with ", id, "."))
  return(match)
}

fn3 <- function(match, id) {
  ## Case 3
  x <- intersect(match$data$changed_B[[id]], match$data$only_B[[id]])
  if (length(x) > 0) {
    match$data$only_B <-
      match$data$only_B[-which(match(match$data$only_B[[id]], x) > 0), ]
  }
  print(paste0(length(x), " cases re-matched with ", id, "."))
  return(match)
}

