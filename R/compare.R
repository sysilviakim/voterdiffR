#' Comparing vrmatch Outputs
#'
#' This function takes two `vrmatch` outputs and compares their differences
#' by returning the set difference of matched outputs between them excluding
#' ID matches. The assumption is that these are matches on the same datasets
#' with different parameter selection, match-variable selection, or random
#' sample selection.
#'
#' @importFrom dplyr inner_join
#'
#' @param m1 The first vrmatch output.
#' @param m2 The second vrmatch output.
#' @param row Temporary row field name.
#' @param id Internal ID variable.
#' @param vars Variables to show in set difference dataframes.
#' Defaults to all common variable names.
#'
#' @return List of set difference dataframes and their number of rows.
#'
#' @export

compare <- function(m1, m2, row = "row", id = "lVoterUniqueID", vars = NULL) {
  if (row %in% names(m1$data$exact_match)) {
    stop("Choose another temporary row field name.")
  }
  m1$data$changed_A[row] <- m1$data$changed_B[row] <-
    seq(nrow(m1$data$changed_A))
  m2$data$changed_A[row] <- m2$data$changed_B[row] <-
    seq(nrow(m2$data$changed_A))
  suppressMessages({
    df_names <- intersect(names(m1$data$changed_A), names(m2$data$changed_A))
    if (nrow(m1$data$id_match_A) == 0) m1 <- id_return(m1, row, id)
    if (nrow(m2$data$id_match_A) == 0) m2 <- id_return(m2, row, id)
    m1$data$changed_A[row] <- m1$data$changed_B[row] <-
      seq(nrow(m1$data$changed_A))
    m2$data$changed_A[row] <- m2$data$changed_B[row] <-
      seq(nrow(m2$data$changed_A))
    x1 <- inner_join(
      m1$data$changed_A, m2$data$changed_A, by = setdiff(df_names, row)
    )
    x2 <- inner_join(
      m1$data$changed_B, m2$data$changed_B, by = setdiff(df_names, row)
    )
    ind1 <- intersect(x1[, paste0(row, ".x")], x2[, paste0(row, ".x")])
    ind2 <- intersect(x1[, paste0(row, ".y")], x2[, paste0(row, ".y")])
  })
  if (is.null(vars)) vars <- df_names
  m1_changed_A <- m1$data$changed_A[-ind1, vars]
  m1_changed_B <- m1$data$changed_B[-ind1, vars]
  m2_changed_A <- m2$data$changed_A[-ind2, vars]
  m2_changed_B <- m2$data$changed_B[-ind2, vars]
  m1_nrow <- nrow(m1_changed_A)
  m2_nrow <- nrow(m2_changed_A)
  print(paste0("From the first vrmatch, ", m1_nrow, " rows are added."))
  print(paste0("From the second match,  ", m2_nrow, " rows are added."))
  return(
    list(
      setdiff = list(
        m1_changed_A = m1_changed_A, m1_changed_B = m1_changed_B,
        m2_changed_A = m2_changed_A, m2_changed_B = m2_changed_B
      ),
      union = list(
        changed_A_union = bind_rows(
          m1_changed_A, m1$data$changed_A[ind1, vars], m2_changed_A
        ),
        changed_B_union = bind_rows(
          m1_changed_B, m1$data$changed_B[ind1, vars], m2_changed_B
        )
      ),
      nrow = list(m1_setdiff_nrow = m1_nrow, m2_setdiff_nrow = m2_nrow)
    )
  )
}

id_return <- function(match, row = "row", id = "lVoterUniqueID") {
  x <- inner_join(
    match$data$changed_A[, c(row, id)], match$data$changed_B[, c(row, id)]
  )[, row]
  match$data$id_match_A <- match$data$changed_A[x, ]
  match$data$id_match_B <- match$data$changed_B[x, ]
  match$data$changed_A <- match$data$changed_A[-x, ]
  match$data$changed_B <- match$data$changed_B[-x, ]
  return(match)
}
