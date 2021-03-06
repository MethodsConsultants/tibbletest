% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descriptives.R
\name{descriptives}
\alias{descriptives}
\title{Creates a table of summary statistics}
\usage{
descriptives(
  df,
  treatment = NULL,
  variables = NULL,
  weights = NULL,
  nonparametric = NULL
)
}
\arguments{
\item{df}{<\code{tbl_df}> Data frame with treatment and variables of interest as columns.}

\item{treatment}{<\code{tidy-select}> Treatment column. If left blank, will produce univariate summary statistics.}

\item{variables}{<\code{tidy-select}> Columns to summarize in table. If left blank, will be inferred from data.}

\item{weights}{<\code{tidy-select}> Column with observation weights. If left blank, will not use observation weights.}

\item{nonparametric}{<\code{tidy-select}> Columns of continuous variables to calculate median/IQR/non-parametric test.}
}
\value{
<\code{tbl_df}> Tibble with summary statistics split by treatment (optional).
}
\description{
Generates a table with summary statistics for each variable, optionally split by a grouping variable.
}
