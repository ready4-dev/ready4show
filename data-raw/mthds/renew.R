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
                                     is_equal_first_lgl = NA,
                                     filter_cdn_1L_chr = NA_character_,
                                     slice_idxs_int = NA_integer_){
fn_env_ls <- as.list(rlang::current_env())[-1]
x <- ready4::update_tb_r3(x,
                          filter_cdn_1L_chr = filter_cdn_1L_chr,
                          fn = renew.ready4show_authors, ## ## ##
                          fn_env_ls = fn_env_ls,
                          slice_idxs_int = slice_idxs_int)
 return(x)
}
renew.ready4show_correspondences<- function(x,
                                            old_nms_chr = NA_character_,
                                            new_nms_chr = NA_character_,
                                            filter_cdn_1L_chr = NA_character_,
                                            slice_idxs_int = NA_integer_){
  fn_env_ls <- as.list(rlang::current_env())[-1]
  x <- ready4::update_tb_r3(x,
                            filter_cdn_1L_chr = filter_cdn_1L_chr,
                            fn = renew.ready4show_correspondences, ## ## ##
                            fn_env_ls = fn_env_ls,
                            slice_idxs_int = slice_idxs_int)
  return(x)
}
renew.ready4show_institutes <- function(x,
                                        short_name_chr = NA_character_,
                                        long_name_chr = NA_character_,
                                        filter_cdn_1L_chr = NA_character_,
                                        slice_idxs_int = NA_integer_){
  fn_env_ls <- as.list(rlang::current_env())[-1]
  x <- ready4::update_tb_r3(x,
                            filter_cdn_1L_chr = filter_cdn_1L_chr,
                            fn = renew.ready4show_institutes, ## ## ##
                            fn_env_ls = fn_env_ls,
                            slice_idxs_int = slice_idxs_int)
  return(x)
}
