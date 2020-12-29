make_rprt_type_ls <- function(rprt_nm_1L_chr,
                              rprt_lup){
  values_chr <- names(rprt_lup)[names(rprt_lup) != "rprt_nms_chr"] %>% purrr::map_chr(~ready4fun::get_from_lup_obj(rprt_lup,
                                                                                                                   match_value_xx = rprt_nm_1L_chr,
                                                                                                                   match_var_nm_1L_chr = "rprt_nms_chr",
                                                                                                                   target_var_nm_1L_chr = .x,
                                                                                                                   evaluate_lgl = F))
  rprt_type_ls <- list(path_to_RMD_dir_1L_chr = ifelse(!is.na(values_chr[2]),values_chr[2],system.file(values_chr[3],
                                                                                                       package = values_chr[4])),
                       nm_of_RMD_1L_chr = values_chr[5],
                       rltv_path_to_outpt_yaml_1L_chr = values_chr[6],
                       file_nm_1L_chr = rprt_nm_1L_chr,
                       title_1L_chr = values_chr[1])
  return(rprt_type_ls)
}
