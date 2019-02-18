#' Report Changes Between Snapshots
#'
#' This function takes the extracted matches from changes_extract,
#' and then summarizes what has changed between the snapshots compared.
#'
#' @import dplyr
#'
#' @param changes Named list outputed from change_extract.
#' @param varnames Variables to track changes.
#' @param nrow Name of list element which will contain the number of rows
#' of the input list dataframes.
#'
#' @return Dataframe with changes' summary statistics.
#'
#' @export

changes_report <- function(changes, varnames, nrow = "nrow") {
  change_prop <- change_n <- totalA <- totalB <- NULL
  out <- data.frame(
    labels = c(
      c("Exact Matches", "Changes", "Dropped", "Added"), varnames
    ),
    totalA = rep(
      sum(
        changes[[nrow]][["exact_match"]],
        changes[[nrow]][["changed"]],
        changes[[nrow]][["only_A"]]
      ),
      length(varnames) + 4
    ),
    totalB = rep(
      sum(
        changes[[nrow]][["exact_match"]],
        changes[[nrow]][["changed"]],
        changes[[nrow]][["only_B"]]
      ),
      length(varnames) + 4
    ),
    change_n = c(
      changes[[nrow]][["exact_match"]],
      changes[[nrow]][["changed"]],
      changes[[nrow]][["only_A"]],
      changes[[nrow]][["only_B"]],
      unlist(
        lapply(varnames, function(v) nrow(changes[[paste(v, "A", sep = "_")]]))
      )
    )
  ) %>%
    dplyr::mutate(
      change_prop = change_n / totalB
    )
  return(out)
}
