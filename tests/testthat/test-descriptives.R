treatment_tbl <- tibble::tribble(
        ~Variable,   ~Label,          ~candy,    ~`ice cream`,
         "gender", "female",  "124 (48.25%)",  "110 (45.27%)",
         "gender",   "male",  "133 (51.75%)",  "133 (54.73%)",
      "happiness",  "happy",  "185 (76.76%)",   "181 (78.7%)",
      "happiness",    "sad",   "56 (23.24%)",    "49 (21.3%)",
          "happy",     "no",   "56 (23.24%)",    "49 (21.3%)",
          "happy",    "yes",    "47 (19.5%)",   "44 (19.13%)",
          "happy",    "Yes",  "138 (57.26%)",  "137 (59.57%)",
            "age",       "", "42.26 (22.62)", "42.39 (21.61)",
   "sugar_factor",       "",    "0.46 (0.3)",   "0.52 (0.29)"
)

cont_tbl <- tibble::tribble(
       ~Variable,          ~candy,    ~`ice cream`,
           "age", "42.26 (22.62)", "42.39 (21.61)",
  "sugar_factor",    "0.46 (0.3)",   "0.52 (0.29)"
)

cat_tbl <- tibble::tribble(
    ~Variable,  ~Label,         ~candy,  ~`ice cream`,
  "happiness", "happy", "185 (76.76%)", "181 (78.7%)",
  "happiness",   "sad",  "56 (23.24%)",  "49 (21.3%)"
)

simple_tbl <- tibble::tribble(
       ~Variable,   ~Label,    ~Statistics,
        "gender", "female", "235 (46.91%)",
        "gender",   "male", "266 (53.09%)",
     "happiness",  "happy", "367 (77.75%)",
     "happiness",    "sad", "105 (22.25%)",
         "happy",     "no", "105 (22.25%)",
         "happy",    "yes",  "91 (19.28%)",
         "happy",    "Yes", "276 (58.47%)",
           "age",       "",  "42.3 (22.1)",
  "sugar_factor",       "",   "0.49 (0.3)"
)


test_that("descriptives produces correct output", {

  treat_out <- example_dat %>%
    descriptives(
      treatment = "treat",
      variables = c("age", "sugar_factor", "gender", "happiness", "happy")
    )
  expect_equal(treat_out, treatment_tbl)

  treat_out2 <- example_dat %>%
    dplyr::select(-treat2, -weight, -no_weight) %>%
    descriptives(treatment = "treat")
  expect_equal(treat_out2, treatment_tbl)

  cat_out <- example_dat %>%
    descriptives(
      treatment = "treat",
      variables = "happiness"
    )
  expect_equal(cat_out, cat_tbl)

  cont_out <- example_dat %>%
    descriptives(
      treatment = "treat",
      variables = c("age", "sugar_factor")
    )
  expect_equal(cont_out, cont_tbl)

  simple_out <- example_dat %>%
    descriptives(variables = c("age", "sugar_factor", "gender", "happiness", "happy"))
  expect_equal(simple_out, simple_tbl)

})
