treatment_tbl <- tibble::tribble(
       ~Variable,   ~Label,          ~candy,    ~`ice cream`,   ~`Absolute Standardized Difference (%)`,
        "gender", "female",  "124 (48.25%)",  "110 (45.27%)",   5.9783168,
        "gender",   "male",  "133 (51.75%)",  "133 (54.73%)",   5.9783168,
           "age",       "", "42.26 (22.62)", "42.39 (21.61)",   0.6073798 ,
  "sugar_factor",       "",    "0.46 (0.3)",   "0.52 (0.29)",   22.3556751,
     "happiness",  "happy",  "185 (76.76%)",   "181 (78.7%)",   4.6451983,
     "happiness",    "sad",   "56 (23.24%)",    "49 (21.3%)",   4.6451983,
         "happy",     "no",   "56 (23.24%)",    "49 (21.3%)",   4.6755379,
         "happy",    "yes",    "47 (19.5%)",   "44 (19.13%)",   4.6755379,
         "happy",    "Yes",  "138 (57.26%)",  "137 (59.57%)",   4.6755379
) %>%
  dplyr::arrange(Variable, Label)

cont_tbl <- tibble::tribble(
       ~Variable,          ~candy,    ~`ice cream`,  ~`Absolute Standardized Difference (%)`,
           "age", "42.26 (22.62)", "42.39 (21.61)",  0.6073798,
  "sugar_factor",    "0.46 (0.3)",   "0.52 (0.29)",  22.3556751
)

cat_tbl <- tibble::tribble(
    ~Variable,  ~Label,         ~candy,  ~`ice cream`, ~`Absolute Standardized Difference (%)`,
  "happiness", "happy", "185 (76.76%)", "181 (78.7%)", 4.6451983,
  "happiness",   "sad",  "56 (23.24%)",  "49 (21.3%)", 4.6451983
) %>%
  dplyr::arrange(Variable, Label)

example_dat %>%
  covariate_balance(
    treatment = "treat",
    variables = c("gender", "age", "sugar_factor", "happiness", "happy")
  ) %>%
  pull(`Absolute Standardized Difference (%)`)

test_that("descriptives produces correct output", {

  treat_out <- example_dat %>%
    covariate_balance(
      treatment = "treat",
      variables = c("gender", "age", "sugar_factor", "happiness", "happy")
    ) %>%
    dplyr::arrange(Variable, Label)

  expect_equivalent(
    treat_out %>% select(-`Absolute Standardized Difference (%)`),
    treatment_tbl %>% select(-`Absolute Standardized Difference (%)`)
  )

  expect_equal(
    treat_out %>% pull(`Absolute Standardized Difference (%)`),
    treatment_tbl %>% pull(`Absolute Standardized Difference (%)`),
    tolerance = 0.0001
  )

  expect_s3_class(treat_out, "covariate_balance")

  treat_out2 <- example_dat %>%
    dplyr::select(-treat2, -weight, -no_weight) %>%
    covariate_balance(treatment = "treat") %>%
    dplyr::arrange(Variable, Label)

  expect_equivalent(
    treat_out2 %>% select(-`Absolute Standardized Difference (%)`),
    treatment_tbl %>% select(-`Absolute Standardized Difference (%)`)
  )

  expect_equal(
    treat_out2 %>% pull(`Absolute Standardized Difference (%)`),
    treatment_tbl %>% pull(`Absolute Standardized Difference (%)`),
    tolerance = 0.0001
  )

  expect_s3_class(treat_out2, "covariate_balance")

  treat_out3 <- example_dat %>%
    covariate_balance(
      treatment = treat,
      variables = c(gender, age, sugar_factor, happiness, happy)
    ) %>%
    dplyr::arrange(Variable, Label)

  expect_equivalent(
    treat_out3 %>% select(-`Absolute Standardized Difference (%)`),
    treatment_tbl %>% select(-`Absolute Standardized Difference (%)`)
  )
  expect_equal(
    treat_out3 %>% pull(`Absolute Standardized Difference (%)`),
    treatment_tbl %>% pull(`Absolute Standardized Difference (%)`),
    tolerance = 0.0001
  )
  expect_s3_class(treat_out3, "covariate_balance")

  cat_out <- example_dat %>%
    covariate_balance(
      treatment = "treat",
      variables = "happiness"
    ) %>%
    dplyr::arrange(Variable, Label)

  expect_equivalent(
    cat_out %>% select(-`Absolute Standardized Difference (%)`),
    cat_tbl %>% select(-`Absolute Standardized Difference (%)`)
  )

  expect_equal(
    cat_out %>% pull(`Absolute Standardized Difference (%)`),
    cat_tbl %>% pull(`Absolute Standardized Difference (%)`),
    tolerance = 0.0001
  )

  expect_s3_class(cat_out, "covariate_balance")

  cat_out2 <- example_dat %>%
    covariate_balance(
      treatment = treat,
      variables = happiness
    ) %>%
    dplyr::arrange(Variable, Label)

  expect_equivalent(
    cat_out2 %>% select(-`Absolute Standardized Difference (%)`),
    cat_tbl %>% select(-`Absolute Standardized Difference (%)`)
  )

  expect_equal(
    cat_out2 %>% pull(`Absolute Standardized Difference (%)`),
    cat_tbl %>% pull(`Absolute Standardized Difference (%)`),
    tolerance = 0.0001
  )

  expect_s3_class(cat_out2, "covariate_balance")

  cont_out <- example_dat %>%
    covariate_balance(
      treatment = "treat",
      variables = c("age", "sugar_factor")
    )

  expect_equivalent(
    cont_out %>% select(-`Absolute Standardized Difference (%)`),
    cont_tbl %>% select(-`Absolute Standardized Difference (%)`)
  )

  expect_equal(
    cont_out %>% pull(`Absolute Standardized Difference (%)`),
    cont_tbl %>% pull(`Absolute Standardized Difference (%)`),
    tolerance = 0.0001
  )

  expect_s3_class(cont_out, "covariate_balance")

  cont_out2 <- example_dat %>%
    covariate_balance(
      treatment = treat,
      variables = c(age, sugar_factor)
    )

  expect_equivalent(
    cont_out2 %>% select(-`Absolute Standardized Difference (%)`),
    cont_tbl %>% select(-`Absolute Standardized Difference (%)`)
  )

  expect_equal(
    cont_out2 %>% pull(`Absolute Standardized Difference (%)`),
    cont_tbl %>% pull(`Absolute Standardized Difference (%)`),
    tolerance = 0.0001
  )

  expect_s3_class(cont_out2, "covariate_balance")

})

no_weight_treatment_tbl <- tibble::tribble(
       ~Variable,   ~Label,          ~candy,    ~`ice cream`,   ~`Absolute Standardized Difference (%)`,
        "gender", "female",        "48.25%",        "45.27%",   5.9783168,
        "gender",   "male",        "51.75%",        "54.73%",   5.9783168,
           "age",       "", "42.26 (22.62)", "42.39 (21.61)",   0.6073798,
  "sugar_factor",       "",    "0.46 (0.3)",   "0.52 (0.29)",   22.3556751,
     "happiness",  "happy",        "76.76%",         "78.7%",   4.6451983,
     "happiness",    "sad",        "23.24%",         "21.3%",   4.6451983,
         "happy",     "no",        "23.24%",         "21.3%",   4.6755379,
         "happy",    "yes",         "19.5%",        "19.13%",   4.6755379,
         "happy",    "Yes",        "57.26%",        "59.57%",   4.6755379
) %>%
  dplyr::arrange(Variable, Label)

weight_treatment_tbl <- tibble::tribble(
       ~Variable,   ~Label,          ~candy,    ~`ice cream`,   ~`Absolute Standardized Difference (%)`,
        "gender", "female",        "48.65%",        "44.97%",   7.373193,
        "gender",   "male",        "51.35%",        "55.03%",   7.373193,
           "age",       "", "42.17 (22.55)", "42.43 (21.93)",   1.164837,
  "sugar_factor",       "",    "0.45 (0.3)",   "0.52 (0.28)",   24.286854,
     "happiness",  "happy",         "77.1%",        "77.38%",   0.661961,
     "happiness",    "sad",         "22.9%",        "22.62%",   0.661961,
         "happy",     "no",         "22.9%",        "22.62%",   1.416440,
         "happy",    "yes",        "19.22%",         "18.8%",   1.416440,
         "happy",    "Yes",        "57.89%",        "58.58%",   1.416440
) %>%
  dplyr::arrange(Variable, Label)

test_that("descriptives produces correct weighted tables", {

  no_weight_treat_out <- example_dat %>%
    covariate_balance(
      treatment = "treat",
      variables = c("gender", "age", "sugar_factor", "happiness", "happy"),
      weights = "no_weight"
    ) %>%
    dplyr::arrange(Variable, Label)

  expect_equivalent(
    no_weight_treat_out %>% select(-`Absolute Standardized Difference (%)`),
    no_weight_treatment_tbl %>% select(-`Absolute Standardized Difference (%)`)
  )
  expect_equivalent(
    no_weight_treat_out %>% pull(`Absolute Standardized Difference (%)`),
    no_weight_treatment_tbl %>% pull(`Absolute Standardized Difference (%)`),
    tolerance = 0.0001
  )
  expect_s3_class(no_weight_treat_out, "covariate_balance")

  no_weight_treat_out2 <- example_dat %>%
    covariate_balance(
      treatment = treat,
      variables = c(gender, age, sugar_factor, happiness, happy),
      weights = no_weight
    ) %>%
    dplyr::arrange(Variable, Label)

  expect_equivalent(
    no_weight_treat_out2 %>% select(-`Absolute Standardized Difference (%)`),
    no_weight_treatment_tbl %>% select(-`Absolute Standardized Difference (%)`)
  )
  expect_equivalent(
    no_weight_treat_out2 %>% pull(`Absolute Standardized Difference (%)`),
    no_weight_treatment_tbl %>% pull(`Absolute Standardized Difference (%)`),
    tolerance = 0.0001
  )
  expect_s3_class(no_weight_treat_out2, "covariate_balance")

  weight_treat_out <- example_dat %>%
    covariate_balance(
      treatment = "treat",
      variables = c("gender", "age", "sugar_factor", "happiness", "happy"),
      weights = "weight"
    ) %>%
    dplyr::arrange(Variable, Label)

  expect_equivalent(
    weight_treat_out %>% select(-`Absolute Standardized Difference (%)`),
    weight_treatment_tbl %>% select(-`Absolute Standardized Difference (%)`)
  )
  expect_equal(
    weight_treat_out %>% pull(`Absolute Standardized Difference (%)`),
    weight_treatment_tbl %>% pull(`Absolute Standardized Difference (%)`),
    tolerance = 0.0001
  )
  expect_s3_class(weight_treat_out, "covariate_balance")

  weight_treat_out2 <- example_dat %>%
    covariate_balance(
      treatment = treat,
      variables = c(gender, age, sugar_factor, happiness, happy),
      weights = weight
    ) %>%
    dplyr::arrange(Variable, Label)

  expect_equivalent(
    weight_treat_out2 %>% select(-`Absolute Standardized Difference (%)`),
    weight_treatment_tbl %>% select(-`Absolute Standardized Difference (%)`)
  )
  expect_equal(
    weight_treat_out2 %>% pull(`Absolute Standardized Difference (%)`),
    weight_treatment_tbl %>% pull(`Absolute Standardized Difference (%)`),
    tolerance = 0.0001
  )
  expect_s3_class(weight_treat_out2, "covariate_balance")

})

nonparametric_tbl <- tibble::tribble(
       ~Variable, ~Label,              ~candy,        ~`ice cream`,   ~`Absolute Standardized Difference (%)`,
         "happy",   "no",       "56 (23.24%)",        "49 (21.3%)",   4.6755379,
         "happy",  "yes",        "47 (19.5%)",       "44 (19.13%)",   4.6755379,
         "happy",  "Yes",      "138 (57.26%)",      "137 (59.57%)",   4.6755379,
           "age",     "",     "42.26 (22.62)",     "42.39 (21.61)",   0.6073798,
  "sugar_factor",     "", "0.44 [0.18, 0.72]", "0.52 [0.28, 0.76]",   22.3556751
) %>%
  dplyr::arrange(Variable, Label)

nonparametric_weight_tbl <- tibble::tribble(
       ~Variable,              ~candy,       ~`ice cream`,  ~`Absolute Standardized Difference (%)`,
           "age",       "42 [22, 62]",      "41 [26, 61]",  1.164837,
  "sugar_factor", "0.44 [0.18, 0.72]", "0.51 [0.3, 0.76]",  24.286854
)

test_that("descriptives produces correct non-parametric tables", {

  nonparametric_out <- example_dat %>%
    select(treat, happy, age, sugar_factor) %>%
    covariate_balance(
      treatment = "treat",
      nonparametric = "sugar_factor"
    ) %>%
    dplyr::arrange(Variable, Label)

  nonparametric_out2 <- example_dat %>%
    covariate_balance(
      treatment = "treat",
      variables = c("happy", "age", "sugar_factor"),
      nonparametric = "sugar_factor"
    ) %>%
    dplyr::arrange(Variable, Label)

  nonparametric_out3 <- example_dat %>%
    covariate_balance(
      treatment = "treat",
      variables = c("happy", "age"),
      nonparametric = "sugar_factor"
    ) %>%
    dplyr::arrange(Variable, Label)

  nonparametric_out4 <- example_dat %>%
    covariate_balance(
      treatment = treat,
      variables = c(happy, age),
      nonparametric = sugar_factor
    ) %>%
    dplyr::arrange(Variable, Label)

  expect_equivalent(
    nonparametric_out %>% select(-`Absolute Standardized Difference (%)`),
    nonparametric_tbl %>% select(-`Absolute Standardized Difference (%)`)
  )
  expect_equal(
    nonparametric_out %>% pull(`Absolute Standardized Difference (%)`),
    nonparametric_tbl %>% pull(`Absolute Standardized Difference (%)`),
    tolerance = 0.0001
  )
  expect_s3_class(nonparametric_out, "covariate_balance")

  expect_equivalent(
    nonparametric_out2 %>% select(-`Absolute Standardized Difference (%)`),
    nonparametric_tbl %>% select(-`Absolute Standardized Difference (%)`)
  )
  expect_equal(
    nonparametric_out2 %>% pull(`Absolute Standardized Difference (%)`),
    nonparametric_tbl %>% pull(`Absolute Standardized Difference (%)`),
    tolerance = 0.0001
  )
  expect_s3_class(nonparametric_out2, "covariate_balance")

  expect_equivalent(
    nonparametric_out3 %>% select(-`Absolute Standardized Difference (%)`),
    nonparametric_tbl %>% select(-`Absolute Standardized Difference (%)`)
  )
  expect_equal(
    nonparametric_out3 %>% pull(`Absolute Standardized Difference (%)`),
    nonparametric_tbl %>% pull(`Absolute Standardized Difference (%)`),
    tolerance = 0.0001
  )
  expect_s3_class(nonparametric_out3, "covariate_balance")

  expect_equivalent(
    nonparametric_out4 %>% select(-`Absolute Standardized Difference (%)`),
    nonparametric_tbl %>% select(-`Absolute Standardized Difference (%)`)
  )
  expect_equal(
    nonparametric_out4 %>% pull(`Absolute Standardized Difference (%)`),
    nonparametric_tbl %>% pull(`Absolute Standardized Difference (%)`),
    tolerance = 0.0001
  )
  expect_s3_class(nonparametric_out4, "covariate_balance")

  nonparametric_weight_out <- example_dat %>%
    covariate_balance(
      treatment = "treat",
      variables = c("age", "sugar_factor"),
      nonparametric = c("age", "sugar_factor"),
      weights = "weight"
    )

  expect_equivalent(
    nonparametric_weight_out %>% select(-`Absolute Standardized Difference (%)`),
    nonparametric_weight_tbl %>% select(-`Absolute Standardized Difference (%)`)
  )
  expect_equal(
    nonparametric_weight_out %>% pull(`Absolute Standardized Difference (%)`),
    nonparametric_weight_tbl %>% pull(`Absolute Standardized Difference (%)`),
    tolerance = 0.0001
  )
  expect_s3_class(nonparametric_weight_out, "covariate_balance")

  nonparametric_weight_out2 <- example_dat %>%
    covariate_balance(
      treatment = treat,
      variables = c(age, sugar_factor),
      nonparametric = c(age, sugar_factor),
      weights = weight
    )

  expect_equivalent(
    nonparametric_weight_out2 %>% select(-`Absolute Standardized Difference (%)`),
    nonparametric_weight_tbl %>% select(-`Absolute Standardized Difference (%)`)
  )
  expect_equal(
    nonparametric_weight_out2 %>% pull(`Absolute Standardized Difference (%)`),
    nonparametric_weight_tbl %>% pull(`Absolute Standardized Difference (%)`),
    tolerance = 0.0001
  )
  expect_s3_class(nonparametric_weight_out2, "covariate_balance")

})
