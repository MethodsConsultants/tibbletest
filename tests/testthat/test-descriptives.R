treatment_tbl <- tibble::tribble(
       ~Variable,   ~Label,          ~candy,    ~`ice cream`,         ~`P Value`,
        "gender", "female",  "124 (48.25%)",  "110 (45.27%)",   0.56317088298012,
        "gender",   "male",  "133 (51.75%)",  "133 (54.73%)",   0.56317088298012,
     "happiness",  "happy",  "185 (76.76%)",   "181 (78.7%)",  0.694415226643762,
     "happiness",    "sad",   "56 (23.24%)",    "49 (21.3%)",  0.694415226643762,
         "happy",     "no",   "56 (23.24%)",    "49 (21.3%)",  0.855356296516553,
         "happy",    "yes",    "47 (19.5%)",   "44 (19.13%)",  0.855356296516553,
         "happy",    "Yes",  "138 (57.26%)",  "137 (59.57%)",  0.855356296516553,
           "age",       "", "42.26 (22.62)", "42.39 (21.61)",  0.946208619349777,
  "sugar_factor",       "",    "0.46 (0.3)",   "0.52 (0.29)", 0.0128504785023786
)

cont_tbl <- tibble::tribble(
       ~Variable,          ~candy,    ~`ice cream`,         ~`P Value`,
           "age", "42.26 (22.62)", "42.39 (21.61)",  0.946208619349777,
  "sugar_factor",    "0.46 (0.3)",   "0.52 (0.29)", 0.0128504785023786
)

cat_tbl <- tibble::tribble(
    ~Variable,  ~Label,         ~candy,  ~`ice cream`,        ~`P Value`,
  "happiness", "happy", "185 (76.76%)", "181 (78.7%)", 0.694415226643762,
  "happiness",   "sad",  "56 (23.24%)",  "49 (21.3%)", 0.694415226643762
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
  expect_equal(
    treat_out %>% select(-`P Value`),
    treatment_tbl %>% select(-`P Value`)
  )
  expect_equal(
    treat_out %>% pull(`P Value`),
    treatment_tbl %>% pull(`P Value`),
    tolerance = 0.0001
  )

  treat_out2 <- example_dat %>%
    dplyr::select(-treat2, -weight, -no_weight) %>%
    descriptives(treatment = "treat")
  expect_equal(
    treat_out2 %>% select(-`P Value`),
    treatment_tbl %>% select(-`P Value`)
  )
  expect_equal(
    treat_out2 %>% pull(`P Value`),
    treatment_tbl %>% pull(`P Value`),
    tolerance = 0.0001
  )

  cat_out <- example_dat %>%
    descriptives(
      treatment = "treat",
      variables = "happiness"
    )
  expect_equal(
    cat_out %>% select(-`P Value`),
    cat_tbl %>% select(-`P Value`)
  )
  expect_equal(
    cat_out %>% pull(`P Value`),
    cat_tbl %>% pull(`P Value`),
    tolerance = 0.0001
  )

  cont_out <- example_dat %>%
    descriptives(
      treatment = "treat",
      variables = c("age", "sugar_factor")
    )
  expect_equal(
    treat_out %>% select(-`P Value`),
    treatment_tbl %>% select(-`P Value`)
  )
  expect_equal(
    cont_out %>% pull(`P Value`),
    cont_tbl %>% pull(`P Value`),
    tolerance = 0.0001
  )

  simple_out <- example_dat %>%
    descriptives(variables = c("age", "sugar_factor", "gender", "happiness", "happy"))
  expect_equal(simple_out, simple_tbl)

})

base_t_test <- t.test(
  age ~ treat,
  data = example_dat
)$p.value

base_chi <- chisq.test(
  example_dat$happiness,
  example_dat$treat
)$p.value

test_that("p-values are accurate", {

  anova_output <- example_dat %>%
    p_anova(var = "age", treatment = "treat", weight_var = "no_weight")
  expect_equal(base_t_test, anova_output, tolerance = 0.0001)

  chi_output <- example_dat %>%
    p_chi_fisher(var = "happiness", treatment = "treat", weight_var = "no_weight")
  expect_equal(base_chi, chi_output, tolerance = 0.0001)

})

no_weight_treatment_tbl <- tibble::tribble(
       ~Variable,   ~Label,          ~candy,    ~`ice cream`,         ~`P Value`,
        "gender", "female",        "48.25%",        "45.27%",   0.56317088298012,
        "gender",   "male",        "51.75%",        "54.73%",   0.56317088298012,
     "happiness",  "happy",        "76.76%",         "78.7%",  0.694415226643762,
     "happiness",    "sad",        "23.24%",         "21.3%",  0.694415226643762,
         "happy",     "no",        "23.24%",         "21.3%",  0.855356296516553,
         "happy",    "yes",         "19.5%",        "19.13%",  0.855356296516553,
         "happy",    "Yes",        "57.26%",        "59.57%",  0.855356296516553,
           "age",       "", "42.26 (22.62)", "42.39 (21.61)",  0.946208619349777,
  "sugar_factor",       "",    "0.46 (0.3)",   "0.52 (0.29)", 0.0128504785023786
)

weight_treatment_tbl <- tibble::tribble(
       ~Variable,   ~Label,          ~candy,    ~`ice cream`,          ~`P Value`,
        "gender", "female",        "48.65%",        "44.97%",   0.474663713560467,
        "gender",   "male",        "51.35%",        "55.03%",   0.474663713560467,
     "happiness",  "happy",         "77.1%",        "77.38%",                   1,
     "happiness",    "sad",         "22.9%",        "22.62%",                   1,
         "happy",     "no",         "22.9%",        "22.62%",   0.990418633579381,
         "happy",    "yes",        "19.22%",         "18.8%",   0.990418633579381,
         "happy",    "Yes",        "57.89%",        "58.58%",   0.990418633579381,
           "age",       "", "42.17 (22.55)", "42.43 (21.93)",   0.897019783725244,
  "sugar_factor",       "",    "0.45 (0.3)",   "0.52 (0.28)", 0.00691716256075117
)

test_that("descriptives produces correct weighted tables", {

  no_weight_treat_out <- example_dat %>%
    descriptives(
      treatment = "treat",
      variables = c("age", "sugar_factor", "gender", "happiness", "happy"),
      weights = "no_weight"
    )

  expect_equal(
    no_weight_treat_out %>% select(-`P Value`),
    no_weight_treatment_tbl %>% select(-`P Value`)
  )
  expect_equal(
    no_weight_treat_out %>% pull(`P Value`),
    no_weight_treatment_tbl %>% pull(`P Value`),
    tolerance = 0.0001
  )

  weight_treat_out <- example_dat %>%
    descriptives(
      treatment = "treat",
      variables = c("age", "sugar_factor", "gender", "happiness", "happy"),
      weights = "weight"
    )

  expect_equal(
    weight_treat_out %>% select(-`P Value`),
    weight_treatment_tbl %>% select(-`P Value`)
  )
  expect_equal(
    weight_treat_out %>% pull(`P Value`),
    weight_treatment_tbl %>% pull(`P Value`),
    tolerance = 0.0001
  )

})
