context("Colrect kami Excel")

test_that("colrect() loads data as is", {
  expect_is(colrect(path  = "excels/clustered.xlsx",
                    sheet = "2019"),
            "data.frame")
})

test_that("colrect() loads data as is", {
  expect_is(colrect(path  = "excels/clustered.xlsx",
                    sheet = "2019",
                    col   = 3,
                    regex = "^a$",
                    offset = c(0, -2),
                    ends = list(row = "A30", col = "test")),
            "data.frame")
})

## test_that("colrect() loads data as is", {
##   expect_is(colrect(path  = "excels/clustered.xlsx",
##                     sheet = "2019",
##                     col   = 3,
##                     regex = "^a$",
##                     ends = list(row = 1, col = 3)),
##             "data.frame")
## })
