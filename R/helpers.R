#' Helper function which calculates p-value via chi-square or fisher
#'
#' @param df <`tbl_df`> Dataframe that has variable and treatment columns of interest
#' @param var <`character(1)`> Name of variable column
#' @param treatment <`character(1)`> Name of treatment column
#'
#' @examples
#'
#' p_chi_fisher(mtcars, var = "am", treatment = "vs")
#'
#' @return <`numeric(1)`> p-value
#'
#' @noRd
p_chi_fisher <- function(df, var, treatment) {

  chisq_wrapper <- function(var, df, treatment) {
    table <- stats::chisq.test(as.factor(df[[var]]), as.factor(df[[treatment]]))
    return(table$p.value)
  }

  fisher_wrapper <- function(var, df, treatment) {
    table <- stats::fisher.test(as.factor(df[[var]]), as.factor(df[[treatment]]), simulate.p.value = TRUE)
    return(table$p.value)
  }

  chisq_wrapper <- purrr::quietly(chisq_wrapper)
  chisq <- chisq_wrapper(var, df, treatment)

  if (length(chisq$warnings) == 0) {
    return(chisq$result)
  } else {
    return(fisher_wrapper(var, df, treatment))
  }

}

#' Helper function which calculates p-value via t-test
#'
#' @inheritParams p_chi_fisher
#'
#' @examples
#'
#' p_t_test(mtcars, var = "mpg", treatment = "vs")
#'
#' @return <`numeric(1)`> p-value
#'
#' @import purrr
#'
#' @noRd
p_t_test <- function(df, var, treatment) {

  df %>%
    pull(var) %>%
    split(df %>% pull(treatment)) %>%
    set_names(c("x", "y")) %>%
    do.call(stats::t.test, .) %>%
    pluck("p.value")

}


#' Helper function which calculates p-value via anova
#'
#' @inheritParams p_chi_fisher
#'
#' @return <`numeric(1)`> p-value
#'
#' @import dplyr
#' @import purrr
#'
#' @noRd
p_anova <- function(df, var, treatment) {

  form <- stats::as.formula(paste0(var, " ~ ", treatment))
  stats::aov(form, df) %>%
    summary() %>%
    pluck(1) %>%
    pull(`Pr(>F)`) %>%
    pluck(1)

}

#' Helper function which calculates p-value via Kruskal-Wallis Rank Sum Test
#'
#' @inheritParams p_chi_fisher
#'
#' @return <`numeric(1)`> p-value
#'
#' @noRd
p_kruskal <- function(df, var, treatment) {

  stats::kruskal.test(df[[var]], df[[treatment]]) %>%
    purrr::pluck("p.value")

}

#' Helper function which calculates p-value via Wilcoxon-Mann-Whitney Test
#'
#' @inheritParams p_chi_fisher
#'
#' @examples
#'
#' p_wilcox(mtcars, var = "mpg", treatment = "vs")
#'
#' @return <`numeric(1)`> p-value
#'
#' @import purrr
#'
#' @noRd
p_wilcox <- function(df, var, treatment) {

  df %>%
    pull(var) %>%
    split(df %>% pull(treatment)) %>%
    set_names(c("x", "y")) %>%
    do.call(stats::wilcox.test, .) %>%
    pluck("p.value")

}

#' Helper function which calculates p-value via chi-square or fisher
#'
#' @param df <`tbl_df`> Dataframe that has variable and treatment columns of interest
#' @param var <`character(1)`> Name of variable column
#' @param treatment <`character(1)`> Name of treatment column
#' @param weight_var <`character(1)`> Name of weight column
#'
#' @return <`numeric(1)`> p-value
#'
#' @noRd
p_chi_weighted <- function(df, var, treatment, weight_var) {

  weight_var <- sym(weight_var)

  survey_obj <- svydesign(~1, data = df, weights = df[[quo_name(weight_var)]])

  quo_name(var) %>%
    paste0(" ~ ", ., " + ", quo_name(treatment)) %>%
    stats::as.formula() %>%
    svychisq(survey_obj) %>%
    purrr::pluck("p.value")

}


#' Helper function which returns whether or not the column is numeric
#'
#' @inheritParams summary_cat
#' @param var <`character(1)`> Name of variable in dataframe.
#'
#' @return <`logical(1)`>
#'
#' @keywords internal
#' @noRd
is_numeric_variable <- function(df, var) {

  var <- df %>%
    pull(var)

  vals <- var %>%
    stats::na.omit() %>%
    unique()

  logical <- vals %>%
    `%in%`(c(0,1)) %>%
    all()

  if (length(vals) == 0) {
    logical <- FALSE
  }

  if (logical) return(FALSE)

  is_bare_numeric(var)

}

#' Helper function which returns whether or not the column is categorical
#'
#' @inheritParams is_numeric_variable
#'
#' @return <`logical(1)`>
#'
#' @keywords internal
#' @noRd
is_categorical_variable <- function(df, var) {

  var <- df %>%
    pull(var)

  vals <- var %>%
    stats::na.omit() %>%
    unique()

  logical <- vals %>%
    `%in%`(c(0,1)) %>%
    all()

  if (length(vals) == 0) {
    logical <- FALSE
  }

  if (logical) return(TRUE)

  (is_bare_character(var) | is_bare_logical(var) | is.factor(var))

}

#' Helper function which calculates proportions and outputs in "N (%)" format
#'
#' @param int_vec <`integer`> Vector of counts.
#'
#' @return <`character`> Count and proportion "N (%)"
#'
#' @keywords internal
#' @noRd
proportions <- function(int_vec) {

  total <- sum(int_vec, na.rm = TRUE)
  probs <- round(int_vec / total * 100, 2)

  probs <- case_when(
    is.na(probs) ~ 0,
    TRUE ~ probs
  )

  int_vec <- case_when(
    is.na(int_vec) ~ 0L,
    TRUE ~ int_vec
  )

  paste0(as.character(int_vec), " (", as.character(probs), "%)")

}

#' Adds proper spacing to categorical summary table
#'
#' @param messy_table <`tbl_df`> Unformatted data frame with summary statistics.
#' @param is_treatment <`logical(1)`> Indicator for whether or not this table is split by treatment.
#' @param p_value <`numeric(1)`/`character(1)`> P-Value to be inserted into table for this variable. (Only for treatment)
#'
#' @return <`tbl_df`> Formatted tibble with proper spacing.
#'
#' @keywords internal
#' @noRd
add_spacing <- function(messy_table, is_treatment, p_value = NULL) {

  if (is_treatment) {

    # Setup empty matrix which will be filled in clean format.
    clean_table <- matrix(data = "", nrow = (nrow(messy_table) + 1), ncol = (ncol(messy_table) + 2))

    # If logical label, leave variable name and p-values on same row as stats.
    if ((nrow(messy_table) == 1) & (messy_table[1, 1] == "")) {

      clean_table <- messy_table %>%
        mutate(Variable = names(messy_table)[1],
               `P Value` = p_value) %>%
        select(Variable, everything())

      colnames(clean_table) <- c("Variable", "Label", names(messy_table)[2:ncol(messy_table)], "P Value")

      # Otherwise, fill empty matrix with correct spacing.
    } else {

      clean_table[2:nrow(clean_table), 2:(ncol(clean_table) - 1)] <- as.matrix(messy_table)
      clean_table[1, 1] <- names(messy_table)[1]
      clean_table[1, ncol(clean_table)] <- p_value

      colnames(clean_table) <- c("Variable", "Label", names(messy_table)[2:ncol(messy_table)], "P Value")

    }

  } else {

    clean_table <- matrix(data = "", nrow = (nrow(messy_table) + 1), ncol = (ncol(messy_table) + 1))

    if ((nrow(messy_table) == 1) & (messy_table[1, 1] == "")) {

      clean_table <- messy_table %>%
        mutate(Variable = names(messy_table)[1]) %>%
        select(Variable, everything())

      colnames(clean_table) <- c("Variable", "Label", "N (%)")

      # Otherwise fill labels with correct spacing.
    } else {

      clean_table[2:nrow(clean_table), 2:ncol(clean_table)] <- as.matrix(messy_table)
      clean_table[1, 1] <- names(messy_table)[1]

      colnames(clean_table) <- c("Variable", "Label", "N (%)")

    }
  }

  clean_table %>%
    as_tibble()

}

#' Converts yes/no or 0/1 variable to logical
#'
#' @inheritParams is_numeric_variable
#'
#' @return <`tbl_df`> Data frame with updated column.
#'
#' @import dplyr
#' @import stringr
#'
#' @keywords internal
#' @noRd
convert_logical <- function(df, var) {

  # If "yes"/"no" or "true"/"false" variable, convert to logical.
  if (df %>% pull(!!var) %>% is.character()) {

    vals <- df %>%
      pull(!!var) %>%
      stats::na.omit() %>%
      unique()

    if (length(vals) > 0) {

      is_yes_no <- vals %>%
        str_trim() %>%
        str_to_lower() %>%
        `%in%`(c("no", "yes")) %>%
        all()

      if (is_yes_no) {

        df <- df %>%
          mutate(!!quo_name(var) := (!!var %>% str_trim() %>% str_to_lower()),
                 !!quo_name(var) := case_when(
                   !!var == "yes" ~ TRUE,
                   !!var == "no" ~ FALSE,
                   TRUE ~ NA
                 ))

      }

      is_true_false_str <- vals %>%
        str_trim() %>%
        str_to_lower() %>%
        `%in%`(c("true", "false")) %>%
        all()

      if (is_true_false_str) {

        df <- df %>%
          mutate(!!quo_name(var) := (!!var %>% str_trim() %>% str_to_lower()),
                 !!quo_name(var) := case_when(
                   !!var == "true" ~ TRUE,
                   !!var == "false" ~ FALSE,
                   TRUE ~ NA
                 ))

      }
    }
  }

  # If 0/1 variable, convert to logical.
  if (df %>% pull(!!var) %>% is.numeric()) {

    vals <- df %>%
      pull(!!var) %>%
      as.character() %>%
      stats::na.omit() %>%
      unique()

    if (length(vals) > 0) {

      is_0_1 <- vals %>%
        `%in%`(c("0", "1")) %>%
        all()

      if (is_0_1) {

        df <- df %>%
          mutate(!!quo_name(var) := case_when(
            !!var == 1 ~ TRUE,
            !!var == 0 ~ FALSE,
            TRUE ~ NA
          ))

      }
    }
  }

  df

}

#' Keeps only the `TRUE` row from binary variables.
#'
#' @inheritParams is_numeric_variable
#' @param tab <`tbl_df`> Summary table prior to spacing.
#' @param treatment <`character(1)`> Name of treatment variable if applicable.
#'
#' @return <`tbl_df`> Updated dataframe.
#'
#' @keywords internal
#' @noRd
remove_false <- function(tab, var, treatment = NULL) {

  # If labels are logical, only show the TRUE label in the table
  labels <- pull(tab, !!var)

  if (is.logical(labels)) {
    if (length(labels) == 2) {

      tab <- tab %>%
        filter((!!var)) %>%
        mutate(!!var := "")

    } else if (identical(labels, TRUE)) {

      tab <- tab %>%
        mutate(!!var := "")

    } else if ((identical(labels, FALSE)) & (!is.null(treatment))) {

      tab[1,] <- list("", "0 (0.00%)", "0 (0.00%)")

    } else if ((identical(labels, FALSE)) & (is.null(treatment))) {

      tab[1,] <- list("", "0 (0.00%)")

    } else {
      stop("Logical label vector is not of expected structure")
    }
  }

  tab

}

#' Helper function which uses calculates a quantile of a vector with observation weights.
#'
#' @param x <`numeric`> Continuous vector of which we want to calculate a quantile
#' @param weights <`numeric`> Nonnegative vector of observation weights
#' @param probs <`numeric(1)`> Quantile to compute (0.5 is median)
#'
#' @return <`numeric(1)`>
#'
#' @import dplyr
#'
#' @noRd
weighted_quantile <- function(x, weights, probs) {

  dat <- tibble(x, weights) %>%
    tidyr::drop_na() %>%
    group_by(x) %>%
    summarise(weights = sum(weights)) %>%
    filter(weights > 0)

  x <- dat$x
  wts <- dat$weights
  n <- sum(wts)
  order <- 1 + (n - 1) * probs
  low <- pmax(floor(order), 1)
  high <- pmin(low + 1, n)
  order <- order%%1
  allq <- stats::approx(cumsum(wts), x, xout = c(low, high), method = "constant",
                 f = 1, rule = 2)$y
  (1 - order) * allq[1] + order * allq[-1]

}

