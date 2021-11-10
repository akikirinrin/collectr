#' Collect clustered data on multiple sheets into rectangle dataframe
#'
#' @inheritParams load_kami_excel
#' @inheritParams unclusterize
#' @inheritParams locate_keys
#' @param sheet_regex regex to search key
#' @export
colrect <- function(path, sheet_regex, row = NULL, col = NULL,
                    regex = NULL, offset = c(0, 0), ends = NULL, info = NULL) {
  if (is.null(col)) {
    direction <- "h"
    pos       <- row
  } else {
    direction <- "v"
    pos       <- col
  }

  instruct_next_step(row = row, col = col,
                     regex = regex, offset = offset)
  colrect_ <- function(path, sheet, regex, direction, pos, offset, ends, info) {
    load_kami_excel(path = path, sheet = sheet) %>%
      unclusterize(regex     = regex,
                   direction = direction,
                   pos       = pos,
                   offset    = offset,
                   ends      = ends,
                   info      = info)
  }
  readxl::excel_sheets(path) %>%
    stringr::str_extract(sheet_regex) %>%
    stats::na.omit() %>%
    lapply(colrect_,
           path      = path,
           regex     = regex,
           direction = direction,
           pos       = pos,
           offset    = offset,
           ends      = ends,
           info      = info) %>%
     purrr::invoke(dplyr::bind_rows, .) %>%
     tibble::as_tibble()
}

instruct_next_step <- function(row, col, regex, offset) {
  if (is.null(col) & is.null(row)) {
    msg       <- "Give me 'row' or 'col' to search keyword:"
  } else if (is.null(regex)) {
    msg       <- "Give me 'regex' to match keyword from this string:"
  } else if (is.null(offset)) {
    msg       <- "Match result:"
  } else {
    msg       <- ""
  }
  message(msg)
}
