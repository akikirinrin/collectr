#' Collect clustered data on multiple sheets into rectangle dataframe
#'
#' @inheritParams load_kami_excel
#' @param col column position to search keyword
#' @export
colrect <- function(path, sheet, col, regex) {
  load_kami_excel(path = path, sheet = sheet) %>%
    show_regex_targets(col = col, regex = regex) %>%
    show_match_result(col = col, regex = regex)
}
