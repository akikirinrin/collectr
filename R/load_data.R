#' Load data from all range of Excel sheet as character
#'
#' @inheritParams readxl::read_excel
#' @export
load_kami_excel <- function(path, sheet) {
  suppressMessages(
    readxl::read_excel(path      = path,
                       sheet     = sheet,
                       col_names = FALSE,
                       col_types = "text")
  )
}
