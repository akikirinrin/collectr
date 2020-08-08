show_regex_targets <- function(df, col) {
  print(df)
  print("Give me 'regex' to match keyword from this string:")
  print(as.data.frame(df)[, col])
  invisible(df)
}
