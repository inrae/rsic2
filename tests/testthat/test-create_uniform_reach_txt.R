profT <- list(
  B = 2,
  S = (6 - 2) / 2 / 2,
  ZF = 100,
  ZB = 100 + 2
)

test_that("Trapezoidal minor section", {
  profT2 <- profT
  profT2$ZF <- 99
  profT2$ZB <- 101
  expect_equal(
    create_uniform_reach_txt(abscissas = c(1000, 2000),
                             upstream_bed_elevation = 100,
                             slope = 0.001,
                             section_type = "T",
                             profile = profT,
                             singular = 2000)[1:2],
    list(
      "00001000" = create_section_txt("Section x=1000", 1000, "T", profT),
      "00002000" = create_section_txt("Section x=2000", 2000, "T", profT2, singular = TRUE)
    )
  )
})
