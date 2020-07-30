context("Load Excel book")

test_that("load_kami_excel() works", {
  expect_is(load_kami_excel(path  = "excels/clustered.xlsx",
                            sheet = "2019"),
            "data.frame")
})
