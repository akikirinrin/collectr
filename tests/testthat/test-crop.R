test_that("crop() returns cropped bf before match", {
  data <- data.frame(a = letters, b = 1:26, stringsAsFactors = FALSE)
  expect_equal(crop(data, direction = "v", pos = 1, regex = "p"),
               data.frame(a = letters[1:16], b = 1:16,
                          stringsAsFactors = FALSE))

  data_horiz <- data.frame(t(data))
  expect_equal(crop(data_horiz, direction = "h", pos = 1, regex = "p"),
               data.frame(t(data.frame(a = letters[1:16],
                                       b = 1:16,
                                       stringsAsFactors = FALSE))))
})

test_that("crop() returns cropped df after match", {
  data <- data.frame(a = letters, b = 1:26, stringsAsFactors = FALSE)
  expect_equal(crop(data, direction = "v", pos = 1,
                    regex = "p", use_after = TRUE),
               data.frame(a = letters[16:26], b = 16:26,
                          row = 16:26, stringsAsFactors = FALSE,
                          row.names = "row"))

  data_horiz <- data.frame(t(data), stringsAsFactors = FALSE)
  expect_equal(crop(data_horiz, direction = "h", pos = 1,
                    regex = "x", use_after = TRUE),
               data.frame(row = c("a", "b"),
                          X24 = c("x", "24"),
                          X25 = c("y", "25"),
                          X26 = c("z", "26"),
                          row.names = "row", stringsAsFactors = FALSE))
})

test_that("crop() do nothing if direction == NULL", {
  data <- data.frame(a = letters, b = 1:26, stringsAsFactors = FALSE)
  expect_equal(crop(data, direction = NULL), data)
})

test_that("pull_vector() make vector from a row of df", {
  data <- data.frame(a = letters, b = 1:26, stringsAsFactors = FALSE)
  expect_equal(pull_vector(data, direction = "h", pos = 1),
               c("a", "1"))
  expect_equal(pull_vector(data, direction = "h", pos = 2),
               c("b", "2"))
  expect_equal(pull_vector(data, direction = "h", pos = 26),
               c("z", "26"))
})

test_that("vectorize_row() convert row into vector", {
  df  <- data.frame(a = 1:10, b = 11:20)
  expect_equal(vectorize_row(df, 2), c(2, 12))
  expect_equal(vectorize_row(df, 3), c(3, 13))
})
