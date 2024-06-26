% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mthd_renew.R
\name{renew.ready4show_authors}
\alias{renew.ready4show_authors}
\alias{renew,ready4show_authors-method}
\alias{renew.ready4show_correspondences}
\alias{renew,ready4show_correspondences-method}
\alias{renew.ready4show_institutes}
\alias{renew,ready4show_institutes-method}
\title{Renew (update) values}
\usage{
renew.ready4show_authors(
  x,
  first_nm_chr = NA_character_,
  middle_nm_chr = NA_character_,
  last_nm_chr = NA_character_,
  title_chr = NA_character_,
  qualifications_chr = NA_character_,
  institute_chr = NA_character_,
  sequence_int = NA_integer_,
  is_corresponding_lgl = NA,
  email_chr = NA_character_,
  is_equal_first_lgl = NA,
  filter_cdn_1L_chr = NA_character_,
  slice_indcs_int = NA_integer_,
  ...
)

\S4method{renew}{ready4show_authors}(
  x,
  first_nm_chr = NA_character_,
  middle_nm_chr = NA_character_,
  last_nm_chr = NA_character_,
  title_chr = NA_character_,
  qualifications_chr = NA_character_,
  institute_chr = NA_character_,
  sequence_int = NA_integer_,
  is_corresponding_lgl = NA,
  email_chr = NA_character_,
  is_equal_first_lgl = NA,
  filter_cdn_1L_chr = NA_character_,
  slice_indcs_int = NA_integer_,
  ...
)

renew.ready4show_correspondences(
  x,
  old_nms_chr = NA_character_,
  new_nms_chr = NA_character_,
  filter_cdn_1L_chr = NA_character_,
  slice_indcs_int = NA_integer_,
  ...
)

\S4method{renew}{ready4show_correspondences}(
  x,
  old_nms_chr = NA_character_,
  new_nms_chr = NA_character_,
  filter_cdn_1L_chr = NA_character_,
  slice_indcs_int = NA_integer_,
  ...
)

renew.ready4show_institutes(
  x,
  short_name_chr = NA_character_,
  long_name_chr = NA_character_,
  filter_cdn_1L_chr = NA_character_,
  slice_indcs_int = NA_integer_,
  ...
)

\S4method{renew}{ready4show_institutes}(
  x,
  short_name_chr = NA_character_,
  long_name_chr = NA_character_,
  filter_cdn_1L_chr = NA_character_,
  slice_indcs_int = NA_integer_,
  ...
)
}
\arguments{
\item{x}{An instance of \code{ready4show_institutes}, a ready4 submodule class for institutes lookup table}

\item{first_nm_chr}{First name (a character vector), Default: 'NA'}

\item{middle_nm_chr}{Middle name (a character vector), Default: 'NA'}

\item{last_nm_chr}{Last name (a character vector), Default: 'NA'}

\item{title_chr}{Title (a character vector), Default: 'NA'}

\item{qualifications_chr}{Qualifications (a character vector), Default: 'NA'}

\item{institute_chr}{Institute (a character vector), Default: 'NA'}

\item{sequence_int}{Sequence (an integer vector), Default: NA}

\item{is_corresponding_lgl}{Is corresponding (a logical vector), Default: NA}

\item{email_chr}{Email (a character vector), Default: 'NA'}

\item{is_equal_first_lgl}{Is equal first (a logical vector), Default: NA}

\item{filter_cdn_1L_chr}{Filter condition (a character vector of length one), Default: 'NA'}

\item{slice_indcs_int}{Slice indices (an integer vector), Default: NA}

\item{...}{Additional arguments}

\item{old_nms_chr}{Old names (a character vector), Default: 'NA'}

\item{new_nms_chr}{New names (a character vector), Default: 'NA'}

\item{short_name_chr}{Short name (a character vector), Default: 'NA'}

\item{long_name_chr}{Long name (a character vector), Default: 'NA'}
}
\value{
x (An object)

x (An object)

x (An object)
}
\description{
renew.ready4show_authors() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the ready4 submodule class for authors lookup table The function is called for its side effects and does not return a value.

renew.ready4show_correspondences() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the Name correspondences lookup table The function is called for its side effects and does not return a value.

renew.ready4show_institutes() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the ready4 submodule class for institutes lookup table The function is called for its side effects and does not return a value.
}
