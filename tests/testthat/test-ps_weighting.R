subset_dat <- example_dat %>%
  tidyr::drop_na(age, treat)

test_that("propensity weighting function adds column with mean 1", {

  output <- subset_dat %>%
    add_propensity_weights(
      treatment = "treat",
      ivs = c("age", "sugar_factor", "gender")
    )

  expect_equal(ncol(output), (ncol(subset_dat) + 1))
  expect_true("propensity_weight" %in% colnames(output))
  expect_equal(mean(output$propensity_weight), 1)

})

test_that("Error when missing treatment data", {

  expect_error(
    add_propensity_weights(
      example_dat,
      treatment = "treat",
      ivs = c("age", "sugar_factor", "gender")
    )
  )

})

subset_dat_2 <- example_dat %>%
  tidyr::drop_na(treat)

test_that("Error when missing IV and impute_missing = FALSE", {

  expect_error(
    subset_dat_2 %>%
      add_propensity_weights(
        treatment = "treat",
        ivs = c("age", "gender", "happiness")
      )
  )

})

test_that("Imputation works as expected", {

  output_2 <- subset_dat_2 %>%
    add_propensity_weights(
      treatment = "treat",
      ivs = c("age", "gender", "happiness"),
      impute_missing = TRUE
    )

  expect_equal(
    output_2 %>% dplyr::select(-propensity_weight),
    subset_dat_2
  )

  expect_equal(ncol(output_2), (ncol(subset_dat_2) + 1))
  expect_true("propensity_weight" %in% colnames(output_2))
  expect_equal(mean(output_2$propensity_weight), 1)

})
