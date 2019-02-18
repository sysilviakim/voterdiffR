#' Changes by Field
#'
#' This function takes a matched output and outputs dataframes that have
#' changed values in user-specified fields.
#'
#' @import dplyr
#' @importFrom assertthat assert_that
#'
#' @param match List of matched output from vrmatch.
#' @param varnames Variables to track changes.
#' @param nrow Name of list element which will contain the number of rows
#' of the input list dataframes.
#'
#' @return List of dataframes that contain changes in each specified field.
#'
#' @export

changes_extract <- function(match, varnames, nrow = "nrow") {
  if (nrow %in% varnames) {
    stop("Specify another name for nrow. Currently it is a variable name.")
  }
  out <- list()
  dfA <- bind_rows(
    match$data$changed_A,
    match$data$id_match_A
  ) %>%
    mutate_if(is.factor, as.character)
  dfB <- bind_rows(
    match$data$changed_B,
    match$data$id_match_B
  ) %>%
    dplyr::mutate_if(is.factor, as.character)
  assert_that(nrow(dfA) == nrow(dfB))
  out[[nrow]] <- list(
    exact_match = nrow(match$data$exact_match),
    changed = nrow(dfA),
    only_A = nrow(match$data$only_A),
    only_B = nrow(match$data$only_B)
  )
  for (v in varnames) {
    out[[paste(v, "A", sep = "_")]] <- dfA[dfA[[v]] != dfB[[v]], ]
    out[[paste(v, "B", sep = "_")]] <- dfB[dfA[[v]] != dfB[[v]], ]
  }
  return(out)
}
