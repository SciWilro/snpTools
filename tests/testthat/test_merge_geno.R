library(snpTools)
context("Testing merge_geno()")

load("sire_barrow_snp_sample.RData")

test_that("merge_geno() can merge!", {
  expect_is(merge_geno(barrow_sample, sire_sample), "matrix")
  expect_equal(dim(merge_geno(barrow_sample, sire_sample)), c(76, 111))
})