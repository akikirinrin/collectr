show_next_step <- function(df, col, regex) {
  print(df)
  if (is.null(col)) {
    msg       <- "Give me 'col' to search keyword:"
    next_step <- "enter col"
    obj       <- NULL
  }
  if (is.null(regex)) {
    msg       <- "Give me 'regex' to match keyword from this string:"
    next_step <- "enter regex"
    obj       <- as.data.frame(df)[, col]
  } else {
    msg       <- "Match result:"
    next_step <- "enter ofset"
    obj       <- stringr::str_extract(as.data.frame(df)[, col], regex)
  }
  print(msg)
  print(obj)
  print(next_step)
  invisible(df)
}
