#' Dispose of Uninformative Emails and Phones
#'
#' This function takes a dataframe that contains personal contacts such as
#' emails and addresses, and cleans out uninformative contacts. For example,
#' if a particular email address is input as fake or a default value from
#' some particular setting, these values are replaced as NA.
#' Similarly for phone numbers, if they are likely fake such as (999)999-9999
#' or contains only area numbers, they are replaced as NA.
#'
#' @import dplyr
#' @importFrom stringi stri_trans_general
#'
#' @param df Input dataframe to be cleaned of uninformative contacts.
#' @param email Name of the email address field.
#' Defaults to NULL.
#' @param email_exc Emails that are to be cleaned.
#' Defaults to a single vector of abc at example.com
#' @param phone Name of the phone number field.
#' Defaults to NULL.
#' @param phone_exc Phone numbers that are to be cleaned.
#' Defaults to "___-____".
#'
#' @return Output dataframe with cleaned contacts.
#'
#' @export

clean_contact <- function(df,
                          email = NULL,
                          email_exc = "abc@example.com",
                          phone = NULL,
                          phone_exc = "___-____") {
  output <- df

  ## Uninformative emails
  ## OCROV particularly has many entries of abc@example.com
  if (!is.null(email)) {
    output[[email]] <-
      stri_trans_general(tolower(output[[email]]), "latin-ascii")
    if (length(email_exc) > 0) {
      for (x in email_exc) {
        output[[email]] <-
          ifelse(grepl(x, output[[email]]), NA, output[[email]])
      }
    }
  }

  ## Uninformative phone numbers
  ## For example, ( __)___-____, 000, (999)999-9999, (714)___-____
  if (!is.null(phone)) {
    output[[phone]] <-
      ifelse(
        nchar(gsub(
          "([[:alnum:]])\\1+", "\\1",
          gsub(
            "\\(|\\)|\\-|\\_", "",
            output[[phone]]
          )
        )) == 1,
        NA, output[[phone]]
      )
    for (x in phone_exc) {
      output[[phone]] <-
        ifelse(grepl(x, output[[phone]]), NA, output[[phone]])
    }
  }
  return(output)
}
