context("measure_taxa_and_mbness")

test_that("use", {
  df <- mbd.SimTrees::measure_taxa_and_mbness(n_replicates = 1, saveit = FALSE)

  # Column names
  testthat::expect_true("lambda" %in% names(df))
  testthat::expect_true("mu" %in% names(df))
  testthat::expect_true("nu" %in% names(df))
  testthat::expect_true("q" %in% names(df))
  testthat::expect_true("crown_age" %in% names(df))
  testthat::expect_true("cond" %in% names(df))
  testthat::expect_true("seed" %in% names(df))
  testthat::expect_true("n_taxas" %in% names(df))
  testthat::expect_true("percentage_mb_species" %in% names(df))
  testthat::expect_true("setting" %in% names(df))

  # Values
  testthat::expect_true(all(df$lambda >= 0.0))
  testthat::expect_true(all(df$mu >= 0.0))
  testthat::expect_true(all(df$nu >= 0.0))
  testthat::expect_true(all(df$q >= 0.0))
  testthat::expect_true(all(df$crown_age >= 0.0))
  testthat::expect_true(all(df$cond == 1))
  testthat::expect_true(all(df$n_taxas >= 0.0))
  testthat::expect_true(all(df$precentage_mb_species >= 0.0))
  testthat::expect_true(all(df$precentage_mb_species <= 100.0))

})
