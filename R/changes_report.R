#' Report Changes Between Snapshots
#'
#' This function takes the extracted matches from changes_extract,
#' and then summarizes what has changed between the snapshots compared.
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom purrr set_names
#'
#' @param changes Named list outputed from change_extract.
#' @param varnames Variables to track changes.
#' Defaults to NULL.
#' @param nrow Name of list element which will contain the number of rows
#' of the input list dataframes.
#' @param group Grouping variable, such as jurisdiction,
#' to differentiate changes in variables by the group.
#'
#' @return Dataframe with changes' summary statistics.
#'
#' @export

changes_report <- function(changes,
                           varnames = NULL,
                           nrow = "nrow",
                           group = NULL) {
  . <- .x <- change_prop <- change_n <- totalA <- totalB <- NULL
  if (is.null(group)) {
    out <- data.frame(
      labels = c(
        c("Exact Matches", "Changed", "Dropped", "Added"), varnames
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
        varnames %>%
          map(nrow(changes[[paste(.x, "A", sep = "_")]])) %>%
          unlist()
      )
    ) %>%
      mutate(change_prop = change_n / totalB)
  } else {
    names(out[[nrow]]) %>%
      set_names(., .) %>%
      map(
        ~ data.frame(
          labels = c(
            c("Exact Matches", "Changed", "Dropped", "Added"), varnames
          ),
          group = .x,
          totalA = rep(
            sum(
              changes[[nrow]][[.x]][["exact_match"]],
              changes[[nrow]][[.x]][["changed"]],
              changes[[nrow]][[.x]][["only_A"]]
            ),
            length(varnames) + 4
          ),
          totalB = rep(
            sum(
              changes[[nrow]][[.x]][["exact_match"]],
              changes[[nrow]][[.x]][["changed"]],
              changes[[nrow]][[.x]][["only_B"]]
            ),
            length(varnames) + 4
          ),
          change_n = c(
            changes[[nrow]][[.x]][["exact_match"]],
            changes[[nrow]][[.x]][["changed"]],
            changes[[nrow]][[.x]][["only_A"]],
            changes[[nrow]][[.x]][["only_B"]],
            varnames %>%
              map(nrow(changes[[paste(.x, "A", sep = "_")]])) %>%
              unlist()
          )
        ) %>%
          mutate(change_prop = change_n / totalB)
      ) %>%
      bind_rows()
  }
  return(out)
}
