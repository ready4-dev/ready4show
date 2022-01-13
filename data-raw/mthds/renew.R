renew.ready4show_authors <- function(x,
                                     first_nm_chr = NA_character_,
                                     middle_nm_chr = NA_character_,
                                     last_nm_chr = NA_character_,
                                     title_chr = NA_character_,
                                     qualifications_chr = NA_character_,
                                     institute_chr = NA_character_,
                                     sequence_int = NA_integer_,
                                     is_corresponding_lgl = NA,
                                     email_chr = NA_character_,
                                     is_equal_first_lgl = NA){
 x <- dplyr::bind_rows(x,
                   tibble::tibble(first_nm_chr = first_nm_chr,
                                  middle_nm_chr = middle_nm_chr,
                                  last_nm_chr = last_nm_chr,
                                  title_chr = title_chr,
                                  qualifications_chr = qualifications_chr,
                                  institute_chr = institute_chr,
                                  sequence_int = sequence_int,
                                  is_corresponding_lgl = is_corresponding_lgl,
                                  email_chr = email_chr,
                                  is_equal_first_lgl = is_equal_first_lgl))
 return(x)
}
renew.ready4show_correspondences<- function(x,
                                            old_nms_chr = NA_character_,
                                            new_nms_chr = NA_character_){
  x <- dplyr::bind_rows(x,
                        tibble::tibble(old_nms_chr = old_nms_chr,
                                       new_nms_chr = new_nms_chr))
  return(x)
}
renew.ready4show_institutes <- function(x,
                                        short_name_chr = NA_character_,
                                        long_name_chr = NA_character_){
  x <- dplyr::bind_rows(x,
                        tibble::tibble(short_name_chr = short_name_chr,
                                       long_name_chr = long_name_chr))
  return(x)
}
