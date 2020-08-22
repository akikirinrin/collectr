#' Crop data frame at specific keyword
#' 
#' @param regex regex to match keyword
#' @param use_after If TRUE, use part after match
#' @inheritParams pull_vector
#' @inheritParams unclusterize
crop <- function(df, direction = NULL, pos = NULL, regex = NULL,
                 use_after = FALSE) {
  if (is.null(direction)) return(df)

  match <- pull_vector(df, direction = direction, pos = pos) %>%
    stringr::str_which(regex)

  if (direction == "h") {
    before <- df[, 1:match]
    after  <- df[, match:ncol(df)]
  } else {
    before <- df[1:match, ]
    after  <- df[match:nrow(df), ]
  }

  if (use_after == TRUE) {
    return(after)
  }
  before
}

#' Vectorize a row
#'
#' @inheritParams unclusterize
#' @param row Position of row to be vectorized
vectorize_row <- function(df, row) {
  df[row, ] %>%
    unlist() %>%
    unname()
}
#' Pull vector out of data frame
#'
#' @inheritParams unclusterize
#' @param direction Direction of the vector to be pulled from df
#' #' \describe{
#'  \item{"h"}{specific row of the df will be returned as avector}
#'  \item{"v"}{specific column of the df will be returned as a vector}
#' }
#' @param pos Row- or column position to be searched
pull_vector <- function(df, direction, pos) {
  if (direction == "v") {
    return(dplyr::pull(df, pos))
  }
  vectorize_row(df, pos)
}
