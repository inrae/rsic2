profT <- list(
  B = 2,
  S = (6 - 2) / 2 / 2,
  ZF = 100,
  ZB = 100 + 2
)

test_that("Trapezoidal minor section", {
  expect_equal(
    create_uniform_reach_txt(abscissas = c(1000, 2000),
                             upstream_bed_elevation = 100,
                             slope = 0.001,
                             section_type = "T",
                             profile = profT),
    list(
      "00001000" = c("Section x=1000 $ 1000 $  $ 0 $ T", "2\t1", "102\t100"),
      "00002000" = c("Section x=2000 $ 2000 $  $ 0 $ T", "2\t1", "101\t99")
    )
  )
})
