test_that("locate_keys() locate positions of keys
 in row or column of given df", {
  data <- data.frame(a = c("foo", "bar", "baz", "bum"),
                     b = 1:4,
                     c = c("this", "is", "a", "test"),
                     stringsAsFactors = FALSE)
  expect_equal(locate_keys(df = data, regex = "foo", col = 1), 1)
  expect_equal(locate_keys(df = data, regex = "ba", col = 1), c(2, 3))
  expect_equal(locate_keys(df = data, regex = "foo", row = 1), 1)
  expect_equal(locate_keys(df = data, regex = "a", row = 3), c(1, 3))
  expect_success(expect_error(locate_keys(df = data, regex = "a"),
                              "Give either 'row' or 'col'"))
  expect_success(expect_error(locate_keys(df = data, regex = "a",
                                          row = 1, col = 1),
                              "Give either 'row' or 'col'"))
})
