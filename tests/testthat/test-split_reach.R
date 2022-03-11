# Minor bed generation
profT <- list(
  B = 2,
  S = (6 - 2) / 2 / 2,
  ZF = 100,
  ZB = 100 + 2
)
min_reach <- create_uniform_reach_txt(abscissas = seq(0, 10000, 100),
                                      upstream_bed_elevation = 10 + 2000 * 0.002,
                                      slope = 0.002,
                                      section_type = "T",
                                      profile = profT)

test_that("extract_reach works", {
  sel_reach <- extract_reach(min_reach, c(2000, 4000))
  expect_s3_class(sel_reach, "ReachTxt")
  expect_s3_class(sel_reach[[1]], "SectionTxt")
  expect_length(sel_reach, 21)
})

test_that("split_reach works", {
  reaches <- split_reach(min_reach, seq(0, 10000, 2000))
  expect_length(reaches, 5)
})
