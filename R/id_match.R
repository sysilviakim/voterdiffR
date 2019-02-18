#' Extract Exact Matches by ID Fields Between Two Dataframes
#'
#' This function takes a named list of two dataframes, performs exact matching
#' by specified ID field(s), and outputs a list of four dataframes:
#' the original dataframes are split into ID matches and non-matches.
#' The original dataframes would be rid of records that match in IDs.
#' The additional dataframes of "id_match_A" and "id_match_B" would contain
#' records with matches in ID in the respective input dataframes.
#'
#' @import dplyr
#'
#' @param df_list A named list that contains two dataframes
#' to check for ID matches.
#' @param ids A vector of internal IDs to iterate for exact match extracts.
#' Defaults to "lVoterUniqueID".
#' @param dfA Name of the first snapshot in df_list for exact matching in IDs.
#' Defaults to "mismatch_A".
#' @param dfB Name of the second snapshot in df_list for exact matching in IDs.
#' Defaults to "mismatch_B".
#'
#' @return the data frame with IDs corrected for
#' @export

id_match <- function(df_list,
                     ids = "lVoterUniqueID",
                     dfA = "mismatch_A",
                     dfB = "mismatch_B") {
  for (id in ids) {
    dfA2 <- df_list[[dfA]] %>% dplyr::arrange(!!as.name(id))
    dfB2 <- df_list[[dfB]] %>% dplyr::arrange(!!as.name(id))
    indexA <- which(dfA2[[id]] %in% dfB2[[id]])
    indexB <- which(dfB2[[id]] %in% dfA2[[id]])

    if ("id_match_A" %in% names(df_list)) {
      df_list$id_match_A <-
        dplyr::bind_rows(df_list$id_match_A, dfA2[indexA, ])
      df_list$id_match_B <-
        dplyr::bind_rows(df_list$id_match_B, dfB2[indexB, ])
    } else {
      df_list$id_match_A <- dfA2[indexA, ]
      df_list$id_match_B <- dfB2[indexB, ]
    }
    if (length(indexA) > 0) {
      df_list[[dfA]] <- dfA2[-indexA, ]
    }
    if (length(indexB) > 0) {
      df_list[[dfB]] <- dfB2[-indexB, ]
    }
    print(paste("Exact matches for ID variable", id, "are extracted."))
    print(unlist(lapply(df_list, nrow)))
  }
  return(df_list)
}

