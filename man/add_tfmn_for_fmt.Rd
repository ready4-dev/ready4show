% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_add.R
\name{add_tfmn_for_fmt}
\alias{add_tfmn_for_fmt}
\title{Add transformation for format}
\usage{
add_tfmn_for_fmt(
  data_xx,
  output_type_1L_chr = c("PDF", "HTML", "Word"),
  tfmns_fn_ls = make_table_fns_ls("identity")
)
}
\arguments{
\item{data_xx}{Data (an output object of multiple potential types)}

\item{output_type_1L_chr}{Output type (a character vector of length one), Default: c("PDF", "HTML", "Word")}

\item{tfmns_fn_ls}{Transformations (a list of functions), Default: make_table_fns_ls("identity")}
}
\value{
Data (an output object of multiple potential types)
}
\description{
add_tfmn_for_fmt() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add transformation for format. The function returns Data (an output object of multiple potential types).
}
\keyword{internal}
