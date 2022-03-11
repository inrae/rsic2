profT <- list(
  B = 2,
  S = (6 - 2) / 2 / 2,
  ZF = 100,
  ZB = 100 + 2
)

test_that("Trapezoidale minor section", {
  expect_equal(
    create_section_txt("toto", 1000, "T", profT, distance_majeur = FALSE)[1:3],
    c("toto $ 1000 $  $ 0 $ T", "2\t1", "102\t100")
  )
  profT_wrong <- profT
  profT_wrong$ZB <- NULL
  expect_error(
    create_section_txt("toto", 1000, "T", profT_wrong, distance_majeur = FALSE)
  )
})

test_that("X/Z minor section", {
  expect_error(
    create_section_txt("toto", 1000, "A", profT, distance_majeur = FALSE)
  )
  profA <- matrix(c(0, 2, 4, 6, 102, 100, 100, 102), ncol = 2)
  expect_equal(
    create_section_txt("toto", 1000, "A", profA, distance_majeur = FALSE)[1:5],
    c("toto $ 1000 $  $ 0 $ A", "0\t102", "2\t100", "4\t100", "6\t102")
  )
})


test_that("Major section", {
  expect_equal(
    create_section_txt("toto", 1000, "T", profT, distance_majeur = 100)[1],
    "toto $ 1000 $ 100 $ 1 $ T"
  )
  expect_equal(
    create_section_txt("toto", 1000, "T", profT, distance_majeur = 0)[1],
    "toto $ 1000 $ 0 $ 1 $ T"
  )
})
