#' Extract Exact Matches by All Fields Between Two Snapshots
#'
#' This function takes a named list of two snapshots, performs exact matching
#' by dplyr::inner_join, and outputs a list of three dataframes:
#' the first dataframe contains records that are exact matches between the
#' two dataframes. The second dataframe contains records that are only in the
#' first snapshot. The third dataframe contains records that are only in the
#' second snapshot. This is to take the load off record linkage when the
#' snapshots are granular, such as daily snapshots.
#'
#' @import dplyr
#' @param df_list A named list that contains two consecutive snapshots.
#' @param dfA Name of the first snapshot in df_list for exact matching.
#' Defaults to "dfA".
#' @param dfB Name of the second snapshot in df_list for exact matching.
#' Defaults to "dfB".
#' @param exact_exclude Whether to actually perform this extraction.
#' Defaults to TRUE.
#'
#' @return A list of three dataframes: exact_match, mismatch_A, and mismatch_B.
#'
#' @export

exact_match <- function(df_list,
                        dfA = "dfA",
                        dfB = "dfB",
                        exact_exclude = TRUE) {
  out <- list()
  df_list[[dfA]] <- df_list[[dfA]] %>% dplyr::mutate_if(is.factor, as.character)
  df_list[[dfB]] <- df_list[[dfB]] %>% dplyr::mutate_if(is.factor, as.character)
  if (exact_exclude == TRUE) {
    out[["exact_match"]] <-
      dplyr::inner_join(df_list[[dfA]], df_list[[dfB]])
    out[["mismatch_A"]] <-
      dplyr::anti_join(df_list[[dfA]], out[["exact_match"]])
    out[["mismatch_B"]] <-
      dplyr::anti_join(df_list[[dfB]], out[["exact_match"]])
  } else {
    out[["exact_match"]] <- df_list[[dfA]][0, ]
    out[["mismatch_A"]] <- df_list[[dfA]]
    out[["mismatch_B"]] <- df_list[[dfB]]
  }
  print("Full matches are extracted.")
  return(out)
}
