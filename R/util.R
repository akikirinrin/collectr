colrow2dirpos <- function(col, row) {
  if (is.null(col)) {
    list(direction = "h",
         pos       = row)
  } else if (is.null(row)) {
    list(direction = "v",
         pos       = col)
  } else {
  }
}
