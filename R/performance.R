#' Assessing the Performance of Record Linkage with a True Match Variable
#'
#' This function takes the match snapshot(s) that are outputs from vrmatch,
#' and using a variable that shows the true match status, i.e., for instance
#' an internally validated voter ID field, assesses the performance of the
#' match. We can use multiple true match variables---for instance,
#' a combination of the internal voter ID variable (field A) and
#' the registration affidavit number variable (field B).
#' The records will be true matches if either A or B matches. Vice versa,
#' we can also garner true matches based on all true match variables being
#' matched.
#'
#' Note that this differs from fastLink::summary in that we are utilizing
#' an externally supplied true match variable.
#'
#' @importFrom stats na.omit
#' @importFrom dplyr "%>%"
#' @importFrom dplyr tibble
#' @importFrom dplyr bind_rows
#' @importFrom purrr set_names
#' @importFrom purrr map
#'
#' @param date_df Dataframe of list of snapshots.
#' @param path_matches Path where the match outcomes are output to.
#' Defaults to "matches".
#' @param date_label Labels for dates (i.e., snapshot IDs), in `date_df`.
#' Defaults to "date_label".
#' @param ids A vector of true match fields.
#' Defaults to c("lVoterUniqueID", "sAffNumber").
#' @param cond Whether the supplied true match fields should be used with a
#' OR condition or AND. Defaults to "or".
#' @param inter_id Interm ID for dplyr::bind_row for purrr::output.
#' Defaults to "inter_id".
#'
#' @return A summary dataframe of performance assessment of all user-specified
#' snapshot matches.
#'
#' @export

performance <- function(date_df,
                        path_matches = "matches",
                        date_label = "date_label",
                        ids = c("lVoterUniqueID", "sAffNumber"),
                        cond = "or",
                        inter_id = "inter_id") {
  if (length(ids) > 1 & !(tolower(cond) %in% c("or", "and"))) {
    stop("Specify a valid condition to handle multiple true match IDs.")
  }
  error_report <- tibble()
  for (i in 1:(nrow(date_df) - 1)) {
    day1 <- date_df[[date_label]][i]
    day2 <- date_df[[date_label]][i + 1]
    load(file.path(path_matches, paste0("match_", day1, "_", day2, ".Rda")))
    if (inter_id %in% names(match$data$exact_match)) {
      stop("Specify a different label for an interim ID for purrr.")
    }
    summ <- list()
    for (id in ids) {
      summ[[id]]$pr <- match$data$changed_A[[id]]
      ## Pairwise truths (exist in both datasets)
      summ[[id]]$pt <- intersect(
        c(match$data$changed_A[[id]], match$data$only_A[[id]]),
        c(match$data$changed_B[[id]], match$data$only_B[[id]])
      )
      ## True positives: same IDs, classified as match.
      summ[[id]]$tp <-
        intersect(match$data$changed_A[[id]], match$data$changed_B[[id]])
      ## False negatives: same IDs, classified as non-match.
      summ[[id]]$fn <-
        intersect(match$data$only_A[[id]], match$data$only_B[[id]])
    }

    temp <- bind_rows(match$data$changed_A, match$data$only_A)
    df_list <- map(
      set_names(names(summ[[1]])), function(x) {
        map(
          set_names(ids), function(y) {
            temp[na.omit(match(summ[[y]][[x]], temp[[y]])), ]
          }
        ) %>%
          bind_rows(.id = inter_id)
      }
    )

    if (tolower(cond) == "or") {
      df_list <- map(
        df_list, function(x) {
          x[!duplicated(x %>% dplyr::select(-!!as.name(inter_id))), ]
        }
      )
    } else if (tolower(cond) == "and"){
      df_list <- map(
        df_list, function(x) {
          x[duplicated(x %>% dplyr::select(-!!as.name(inter_id))), ]
        }
      )
    }
    suppressMessages({
      ## False positives: how many 'wrong' IDs in the 'changed_A'?
      df_list$fp <- anti_join(df_list$pr, df_list$tp)
      ## True negatives: net out false negatives from nonmatch IDs (two refs)
      df_list$tnA <- anti_join(match$data$only_A, df_list$fn)
      df_list$tnB <- anti_join(match$data$only_B, df_list$fn)
    })

    ## Pairwise precision, recall, and F1
    precision <- nrow(df_list$tp) / (nrow(df_list$tp) + nrow(df_list$fp))
    recall <- nrow(df_list$tp) / (nrow(df_list$tp) + nrow(df_list$fn))
    f1 <- 2 * (precision * recall) / (precision + recall)

    error_report <-
      dplyr::bind_rows(
        error_report,
        data.frame(
          precision = precision,
          recall = recall,
          f1 = f1,
          pr = nrow(df_list$pr),
          pt = nrow(df_list$pt),
          tp = nrow(df_list$tp),
          fn = nrow(df_list$fn),
          fp = nrow(df_list$fp),
          tnA = nrow(df_list$tnA),
          tnB = nrow(df_list$tnB)
        ) %>%
          dplyr::mutate(date_origin = day2)
      )
    print(paste0("Performance measures for ", day1, " finished."))
    gc(reset = TRUE)
  }
  return(error_report)
}
