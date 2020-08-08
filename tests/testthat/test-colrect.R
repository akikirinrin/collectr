context("Colrect kami Excel")

test_that("colrect() loads data as is", {
  expect_is(colrect(path  = "excels/clustered.xlsx",
                    sheet = "2019",
                    col   = 3),
            "data.frame")
})
