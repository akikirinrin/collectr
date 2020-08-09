#' Collect clustered data on multiple sheets into rectangle dataframe
#'
#' @inheritParams load_kami_excel
#' @param col column position to search keyword
#' @export
colrect <- function(path, sheet, col = NULL, regex = NULL) {
  load_kami_excel(path = path, sheet = sheet) %>%
    show_next_step(col = col, regex = regex)
}
