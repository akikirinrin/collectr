test_that("colrow2dirpos() converts col and row to direction and pos", {
  expect_equal(colrow2dirpos(col = 3, row = NULL),
               list(direction = "v", pos = 3))
})
