descriptives_obj <- example_dat %>%
  dplyr::mutate(happy_true = dplyr::recode(
    happiness,
    "happy" = TRUE,
    "sad" = FALSE
  )) %>%
  descriptives(
    treatment = "treat",
    variables = c("age", "gender", "happy_true")
  )

test_that("all FALSE only changes p-values", {

  all_false <- descriptives_obj %>%
    format_tbl(
      add_Ns = FALSE,
      remove_duplicates = FALSE,
      remove_false = FALSE
    )

  expect_equal(
    all_false %>% dplyr::select(-`P Value`),
    descriptives_obj %>% dplyr::select(-`P Value`)
  )

})

test_that("adding count to column-names works", {

  add_n <- descriptives_obj %>%
    format_tbl(
      add_Ns = TRUE,
      remove_duplicates = FALSE,
      remove_false = FALSE
    )

  expect_equal(
    colnames(add_n),
    c("Variable", "Label", "candy (N=257)", "ice cream (N=243)", "P Value")
  )

})


no_duplicates_truth <- tibble::tribble(
     ~Variable,   ~Label,          ~candy,      ~`ice cream`, ~`P Value`,
         "age",       "", "42.26 (22.62)",   "42.39 (21.61)",    "0.946",
      "gender",       "",              "",                "",    "0.563",
            "", "female",  "124 (48.25%)",    "110 (45.27%)",         "",
            "",   "male",  "133 (51.75%)",    "133 (54.73%)",         "",
  "happy_true",       "",              "",                "",    "0.694",
            "",  "FALSE",   "56 (23.24%)",      "49 (21.3%)",         "",
            "",   "TRUE",  "185 (76.76%)",     "181 (78.7%)",         ""
)

test_that("removing duplicate variable names and p-values works", {

  remove_dupe <- descriptives_obj %>%
    format_tbl(
      add_Ns = FALSE,
      remove_duplicates = TRUE,
      remove_false = FALSE
    )

  expect_equal(
    remove_dupe,
    no_duplicates_truth
  )

})



no_false_truth <- tibble::tribble(
     ~Variable,   ~Label,          ~candy,      ~`ice cream`, ~`P Value`,
         "age",       "", "42.26 (22.62)",   "42.39 (21.61)",    "0.946",
      "gender", "female",  "124 (48.25%)",    "110 (45.27%)",    "0.563",
      "gender",   "male",  "133 (51.75%)",    "133 (54.73%)",    "0.563",
  "happy_true",       "",  "185 (76.76%)",     "181 (78.7%)",    "0.694"
)

test_that("dropping false label from binary variables works", {

  remove_false <- descriptives_obj %>%
    format_tbl(
      add_Ns = FALSE,
      remove_duplicates = FALSE,
      remove_false = TRUE
    )

  expect_equal(
    remove_false,
    no_false_truth
  )

})

all_truth <- tibble::tribble(
     ~Variable,   ~Label,  ~`candy (N=257)`, ~`ice cream (N=243)`, ~`P Value`,
         "age",       "",   "42.26 (22.62)",      "42.39 (21.61)",    "0.946",
      "gender",       "",                "",                   "",    "0.563",
            "", "female",    "124 (48.25%)",       "110 (45.27%)",         "",
            "",   "male",    "133 (51.75%)",       "133 (54.73%)",         "",
  "happy_true",       "",    "185 (76.76%)",        "181 (78.7%)",    "0.694"
)

test_that("all features work together", {

  all_features <- descriptives_obj %>%
    format_tbl()

  expect_equal(
    all_features,
    all_truth
  )

})

univariate_obj <- example_dat %>%
  dplyr::mutate(happy_true = dplyr::recode(
    happiness,
    "happy" = TRUE,
    "sad" = FALSE
  )) %>%
  descriptives(
    variables = c("age", "gender", "happy_true")
  )

univariate_truth <- tibble::tribble(
     ~Variable,   ~Label,    ~Statistics,
         "age",       "",  "42.3 (22.1)",
      "gender",       "",             "",
            "", "female", "235 (46.91%)",
            "",   "male", "266 (53.09%)",
  "happy_true",       "", "367 (77.75%)"
)

test_that("works in univariate case", {

  univariate <- univariate_obj %>%
    format_tbl()

  expect_equal(
    univariate,
    univariate_truth
  )

})
