% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_table_fn}
\alias{make_table_fn}
\title{Make table function}
\usage{
make_table_fn(
  type_1L_chr = c("HTML", "PDF", "Word"),
  what_1L_chr = c("df", "gtsummary", "null")
)
}
\arguments{
\item{type_1L_chr}{Type (a character vector of length one), Default: c("HTML", "PDF", "Word")}

\item{what_1L_chr}{What (a character vector of length one), Default: c("df", "gtsummary", "null")}
}
\value{
Table (a function)
}
\description{
make_table_fn() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make table function. The function returns Table (a function).
}
\keyword{internal}
