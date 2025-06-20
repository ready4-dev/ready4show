% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_table_fns_ls}
\alias{make_table_fns_ls}
\title{Make table functions list}
\usage{
make_table_fns_ls(
  what_1L_chr = c("df", "gtsummary", "null", "identity"),
  html_table_fn = NULL,
  pdf_table_fn = NULL,
  word_table_fn = NULL
)
}
\arguments{
\item{what_1L_chr}{What (a character vector of length one), Default: c("df", "gtsummary", "null", "identity")}

\item{html_table_fn}{Html table (a function), Default: NULL}

\item{pdf_table_fn}{Pdf table (a function), Default: NULL}

\item{word_table_fn}{Word table (a function), Default: NULL}
}
\value{
Table functions (a list)
}
\description{
make_table_fns_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make table functions list. The function returns Table functions (a list).
}
