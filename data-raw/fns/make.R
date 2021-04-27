make_abstract_lines <- function(abstract_ls){
  abstract_lines_chr <- purrr::map2(abstract_ls,
                                    names(abstract_ls),
                                    ~ {
                                      c(paste0("\\\\textbf{",.y,": }"),
                                        .x,
                                        "\\\\newline",
                                        "\\\\newline")
                                    }) %>%
    purrr::flatten_chr()
  return(abstract_lines_chr)
}
make_authorship_lines <- function(authors_tb){
  authorship_lines_chr <- 1:nrow(authors_tb) %>% purrr::map(~ make_sngl_author_lines(authors_tb,
                                                                                     slice_1L_int = .x)) %>%
    purrr::flatten_chr()
  return(authorship_lines_chr)
}
make_eqn_ref <- function(eqn_nm_1L_chr,
                         output_type_1L_chr){
  eqn_ref_1L_chr <- ifelse(output_type_1L_chr == "Word",
                           paste0("\\@ref(eq:",eqn_nm_1L_chr,")"),
                           paste0("\\ref{eq:",eqn_nm_1L_chr,"}"))
  return(eqn_ref_1L_chr)
}
make_institutes_lines <- function(authors_tb,
                                  institutes_tb){
  institutes_chr <- authors_tb$institute_chr %>% purrr::map(~stringr::str_remove_all(.x,"\"") %>%
                                                              strsplit(", ")) %>%
    purrr::flatten() %>% purrr:::flatten_chr() %>% unique()
  institutes_lines_chr <- institutes_chr %>% purrr::map_chr(~{
    paste0("  - ",.x,": ",
           ready4fun::get_from_lup_obj(institutes_tb,
                                       match_var_nm_1L_chr = "short_name_chr",
                                       match_value_xx = .x,
                                       target_var_nm_1L_chr = "long_name_chr",
                                       evaluate_lgl = F) %>%
             stringr::str_remove_all("\""))
  })
  return(institutes_lines_chr)
}
make_paths_ls <- function(params_ls,
                          depth_1L_int = 1){
  paths_ls <- list(path_from_top_level_1L_chr = params_ls$path_from_top_level_1L_chr,
                   path_to_data_from_top_level_chr = params_ls$path_to_data_from_top_level_chr)
  if(!params_ls$use_fake_data_1L_lgl){
    paths_ls$write_to_dir_nm_1L_chr <- "Real"
  }else{
    if(is.null(paths_ls$path_from_top_level_1L_chr)){
      path_elements_chr <- dirname(getwd()) %>% strsplit("/") %>% purrr::pluck(1)
      paths_ls$path_from_top_level_1L_chr <- path_elements_chr[length(path_elements_chr)-depth_1L_int]
    }
    paths_ls$write_to_dir_nm_1L_chr <- "Fake"
    paths_ls$path_to_fake_data_1L_chr <- paste0(paths_ls$path_from_top_level_1L_chr,"/",paths_ls$write_to_dir_nm_1L_chr,"/fake_data.rds")
    paths_ls$path_to_data_from_top_level_chr <- c(paths_ls$path_from_top_level_1L_chr,paths_ls$write_to_dir_nm_1L_chr,"fake_data.rds")
  }
  return(paths_ls)
}
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
make_sngl_author_lines <- function(authors_tb,
                                   slice_1L_int,
                                   inc_quals_1L_lgl = F){
  sngl_author_tb <- authors_tb %>%
    dplyr::slice(slice_1L_int)
  indent_1L_chr <- ifelse(slice_1L_int ==1,"      ","    ")
  author_1L_chr <- paste0(ifelse(slice_1L_int ==1,"  - ","  - name: "),
                          sngl_author_tb$first_nm_chr,
                          " ",
                          ifelse(is.na(sngl_author_tb$middle_nm_chr),
                                 "",
                                 paste0(stringr::str_sub(sngl_author_tb$middle_nm_chr,end=1)," ")),
                          sngl_author_tb$last_nm_chr,
                          ifelse(inc_quals_1L_lgl,
                                 ifelse(is.na(sngl_author_tb$qualifications_chr),
                                        "",
                                        paste0(" (", sngl_author_tb$qualifications_chr %>% stringr::str_replace_all("\"",""),")")),
                                 ""),
                          ifelse(slice_1L_int==1,":",""))
  email_1L_chr <- ifelse(sngl_author_tb$email_chr =="",
                         NA_character_,
                         paste0(indent_1L_chr,
                                "email: ",
                                sngl_author_tb$email_chr))
  institute_1L_chr <- paste0(indent_1L_chr,"institute: [",
                             sngl_author_tb$institute_chr %>% stringr::str_replace_all("\"",""),
                             "]")
  correspondence_1L_chr <- ifelse(sngl_author_tb$is_corresponding_lgl,
                                  paste0(indent_1L_chr,
                                         "correspondence: true"),
                                  NA_character_)
  equal_1L_chr <- ifelse(sngl_author_tb$is_equal_first_lgl,
                         paste0(indent_1L_chr,
                                "equal_contributor: \"yes\""),
                         NA_character_)
  author_lines_chr <- c(author_1L_chr, email_1L_chr, institute_1L_chr, correspondence_1L_chr, equal_1L_chr) %>%
    purrr::discard(is.na)
  return(author_lines_chr)
}
