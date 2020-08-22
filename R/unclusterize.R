#' Extract data clusters from data frame using the keyword
#'
#' This function extracts data clusters from single Excel sheet.
#' @inheritParams extract_a_cluster
#' @inheritParams rebel
#' @param df Data frame to be processed
#' @param regex Regular expression to match keywords
#' @param direction Directoin of the cluster revolution
#' @param pos Positon of row/column to scan using \code{regex}
#' @param crop Params of \code{crop()} in list format
unclusterize <- function(df, regex, direction, pos,
                         offset = c(0, 0), ends,
                         info = NULL, crop = NULL) {
  if (is.null(pos)) return(df)

  cropped <- crop(df, direction = crop$direction,
                  pos = crop$pos, regex = crop$regex,
                  use_after = crop$use_after)
  if (direction == "h") {
    pos_key <- locate_keys(df = cropped, row = pos, regex = regex)
    purrr::map(pos_key, extract_a_cluster, find_from = pos,
               direction = "h", df = cropped,
               offset = offset, ends = ends, info = info)
  } else if (direction == "v") {
    pos_key <- locate_keys(df = cropped, col = pos, regex = regex)
    purrr::map(pos_key, extract_a_cluster, find_from = pos,
               direction = "v", df = cropped,
               offset = offset, ends = ends, info = info)
  } else {
    warning("Set 'direction' correctly")
    cropped
  }
}

#' Locate keywords in row or column of the given data frame
#'
#' @inheritParams unclusterize
#' @param row Row position of df where the keyword appears
#' @param col Column position of df where the keyword appears
#' @param regex Regex to match keyword
locate_keys <- function(df, row = NULL, col = NULL, regex){
  if ( (!is.null(row) & !is.null(col)) |
      (is.null(row) & is.null(col))) {
    stop("Give either 'row' or 'col'")
  } else if (!is.null(row)){
    str <- vectorize_row(df, row)
  } else if (!is.null(col)){
    str <- dplyr::pull(df, col)
  }
  stringr::str_which(str, regex)
}

#' Extract a cluster from df using the keyword
#'
#' This function is the substancial function of \code{unclusterize}.
#' @inheritParams unclusterize
#' @param direction The direction to which data clusters distribute
#' @param find_from The row or column position
#'   which \code{excract_cluster()} search key
#' @param pos_key Position where the \code{regex} of \code{unclusterize}
#'   matched the keyword
#' @param offset The offset (\code{c(row, pos})) of the cluster topleft from
#'   the coordination of keyword
#' @param ends List of regex to locate row- and column- ends of each cluster
#'   Form should be like \code{ends = list(row = "2019", col = "[Dd]ecember$")}.
#' Regex \code{row = } must specify the end of 'left most' columnn of df,
#'  not that of the column with key matched by \code{regex}
#' @param info Parameters to make key:value list such as
#' \describe{
#'  \item{key_offset}{Offset \code{c(row, col)} of \code{key} topleft
#'   from df topleft. If \code{NULL}, automatically set to \code{keyn}}
#'  \item{key_dim}{Dimension \code{c(row, col)} of \code{key}}
#'  \item{value_offset}{Offset \code{c(row, col)} of \code{value} topleft from
#'   df topleft}
#'  \item{value_dim}{Dimension \code{c(row, col)} of \code{value}}
#' }
extract_a_cluster <- function(pos_key, find_from, direction, df,
                              offset = c(0, 0), ends,
                              info = NULL) {
  rofst <- offset[1]
  cofst <- offset[2]

  if (direction == "v") {
    row <- pos_key + rofst
    col <- find_from + cofst
    maxrow <- locate_matchend(dplyr::pull(df, col)[row:nrow(df)],
                              ends[["row"]]) + row - 1
    maxcol <- locate_matchend(vectorize_row(df, row)[find_from:ncol(df)],
                              ends[["col"]])
    nrow <- maxrow - row + 1
    ncol <- maxcol - cofst
  } else if (direction == "h"){
    row <- find_from + rofst
    col <- pos_key + cofst
    maxrow <- locate_matchend(dplyr::pull(df, col)[row:nrow(df)],
                              ends[["row"]]) + row - 1
    maxcol <- locate_matchend(vectorize_row(df, row)[col:ncol(df)],
                              ends[["col"]]) + col - 1
    nrow <- maxrow - row + 1
    ncol <- maxcol - pos_key - cofst + 1
  }

  out <- df[row:(row + nrow - 1), col:(col + ncol - 1)] %>%
    make_ascii(row = 1)

  if (offset[1] == -1 && offset[2] == 0) {
    out[1, 1] <- out[2, 1]
    out <- out[-2, ]
  }

  if (is.null(info)) return(out)

  value_offset <- info$value_offset
  value_dim    <- info$value_dim
  rvalue       <- row + value_offset[1]
  cvalue       <- col + value_offset[2]
  value        <- df[rvalue:(rvalue + value_dim[1] - 1),
                     cvalue:(cvalue + value_dim[2] - 1)] %>%
    unlist() %>%
    as.vector()
  if (is.null(info$key_offset)) {
    key <- paste0("key", 1:max(value_dim))
  } else {
    key_offset   <- info$key_offset
    key_dim      <- info$key_dim
    rkey <- row + key_offset[1]
    ckey <- col + key_offset[2]
    key  <- df[rkey:(rkey + key_dim[1] - 1),
               ckey:(ckey + key_dim[2] - 1)] %>%
      unlist() %>% as.vector()
  }

  info_list <- as.list(stats::setNames(value, key))
  out %>%
    append_info(info = info_list, headerized = FALSE)
}
#' Locate the end of repeated match
#'
#' This function locates the end of the repeated matchs in string.
#' The first end of repeated match will be returned if there are
#'   multiple repeted match.
#' @param str String to be searched
#' @param regex Regex for search
locate_matchend <- function(str, regex) {
  matched        <- stringr::str_which(str, regex)
  if (length(matched) == 0) {
    browser()
    rlang::abort(message = "Match failed. Re-consider regex.",
                 .subclass = "locate_matchend_error",
                 regex = regex, str = str)
  }
  multiple_match <- length(matched) > 1
  if (multiple_match) {
    if (all(diff(matched) == 1)) {
      out <- length(matched) + min(matched) - 1
    } else {
      out <- min(which(diff(matched) != 1)) + min(matched) - 1
    }
  } else {
    out <- matched
  }
  out
}
#' Convert full-width numbers in df into ASCII numbers
#'
#' @param x Data frame or vector to be processed
#' @param col Number of the target column
#' @param row Number of the target row
#' @param numerize If TRUE, remove characters convert column to numeric
#' @param headerized If FALSE (default), allow df with tentative colnames
#' @export
make_ascii <- function(x, col = NULL, row = NULL,
                       numerize = FALSE, headerized = FALSE) {
  if (!is.data.frame(x) & !is.list(x)) {
    string <- x
  } else {
    row_offset <- 0
    x <- as.data.frame(x)
    if (headerized) {
      header <- colnames(x)
      body   <- x
    } else {
      header     <- vectorize_row(x, 1)
      body       <- x[-1, ]
      row_offset <- -1
    }
    if (is.null(col) & is.null(row)) {
      rlang::abort(message = "Give me at least 'col' or 'row'.",
                   .subclass = "make_ascii_error")
    } else {
      edit_row    <- !is.null(row) && (row > 1 | headerized == TRUE)
      edit_col    <- !is.null(col)
      edit_header <- !is.null(row) && row == 1 && headerized == FALSE
    }
    if (edit_col) {
      string <- dplyr::pull(body, col)
    } else if (edit_header) {
      string <- header
    } else if (edit_row) {
      string <- vectorize_row(body, row + row_offset)
    }
  }

  ascii <- purrr::map_chr(string, zipangu::str_conv_zenhan, to = "hankaku")

  if (numerize) {
    ascii <- ascii %>%
      stringr::str_remove_all("\\D")
  }

  if (is.vector(x)) return(ascii)

  if (edit_col) {
    body[, col] <- ascii
  } else if (edit_header) {
    header <- ascii
  } else if (edit_row) {
    body[row + row_offset, ] <- ascii
  }
  if (headerized) {
    colnames(body) <- header
    out <- body
  } else {
    out <- rbind(header, body)
  }
  out
}

#' Append information stored in list to data frame
#'
#' @param info List conposed of `key = value` pairs
#' @param headerized If FALSE, allow appending to data frame with
#'   tentative colnames
#' @inheritParams unclusterize
append_info <- function(df, info, headerized = FALSE) {
  df_info <- list2df(info, nrow = nrow(df))
  if (headerized == FALSE) {
    df_info[1, ] <- names(info)
    tentative_name <- as.character(seq(ncol(df) + 1, ncol(df) + length(info)))
    colnames(df_info) <- tentative_name
  }
  cbind(df, df_info)
}

#' Expand single value list to data frame
#'
#' @param list List with `key = value` pairs
#' @param nrow Nrows of df to be created
list2df <- function(list, nrow) {
  names <- names(list)
  list %>%
    unlist() %>%
    unname() %>%
    rep(nrow, each = nrow) %>%
    matrix(ncol = length(list), nrow = nrow) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    magrittr::set_colnames(names)
}
