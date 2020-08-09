show_regex_targets <- function(df, col, regex) {
  if (is.null(regex)) {
    print(df)
    print("Give me 'regex' to match keyword from this string:")
    print(as.data.frame(df)[, col])
  }
  invisible(df)
}

show_match_result <- function(df, col, regex) {
  print(df)
  print("Match result:")
  print(stringr::str_extract(as.data.frame(df)[, col], regex))
  invisible(df)
}
