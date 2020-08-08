colrect <- function(path, sheet, col) {
  load_kami_excel(path = path, sheet = sheet) %>%
    show_regex_targets(col = col)
}
