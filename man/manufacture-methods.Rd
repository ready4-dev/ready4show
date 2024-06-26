% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mthd_manufacture.R
\name{manufacture.ready4show_correspondences}
\alias{manufacture.ready4show_correspondences}
\alias{manufacture,ready4show_correspondences-method}
\alias{manufacture-Ready4showSynopsis}
\alias{manufacture,Ready4showSynopsis-method}
\title{Manufacture a new object}
\usage{
manufacture.ready4show_correspondences(
  x,
  data_ls,
  flatten_1L_lgl = F,
  type_1L_chr = "new",
  what_1L_chr = "names",
  ...
)

\S4method{manufacture}{ready4show_correspondences}(
  x,
  data_ls,
  flatten_1L_lgl = F,
  type_1L_chr = "new",
  what_1L_chr = "names",
  ...
)

\S4method{manufacture}{Ready4showSynopsis}(x, what_1L_chr = "paths_ls")
}
\arguments{
\item{x}{An object of class Ready4showSynopsis}

\item{data_ls}{Data (a list)}

\item{flatten_1L_lgl}{Flatten (a logical vector of length one), Default: F}

\item{type_1L_chr}{Type (a character vector of length one), Default: 'new'}

\item{what_1L_chr}{What (a character vector of length one), Default: 'paths_ls'}

\item{...}{Additional arguments}
}
\value{
Object (an output object of multiple potential types)

Object (an output object of multiple potential types)
}
\description{
manufacture.ready4show_correspondences() is a manufacture method that manufactures a novel R object using data contained in an instance of a class. This method is implemented for the Name correspondences lookup table The function returns Object (an output object of multiple potential types).

manufacture method applied to Ready4showSynopsis
}
