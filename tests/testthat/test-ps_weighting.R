subset_dat <- example_dat %>%
  tidyr::drop_na(age, treat)

test_that("propensity weighting function adds column with mean 1", {

  output <- subset_dat %>%
    propensity_weighting(
      treatment = "treat",
      ivs = c("age", "sugar_factor", "gender")
    )

  expect_equal(ncol(output), (ncol(subset_dat) + 1))
  expect_true("propensity_weight" %in% colnames(output))
  expect_equal(mean(output$propensity_weight), 1)

  ## Error when missing data
  expect_error(
    propensity_weighting(
      example_dat,
      treatment = "treat",
      ivs = c("age", "sugar_factor", "gender")
    )
  )

})
