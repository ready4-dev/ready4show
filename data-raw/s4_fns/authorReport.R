authorReport_Ready4showSynopsis <- function (x) {
  if(is.na(x@fl_nm_1L_chr)){
    fl_nm_1L_chr <- "Manuscript"
  }else{
    fl_nm_1L_chr <- x@fl_nm_1L_chr
  }
  paths_ls <- manufacture(x,
                          what_1L_chr = "paths_ls")
  write_new_dirs(paths_ls %>%
                   purrr::flatten_chr())
  header_yaml_args_ls <- make_header_yaml_args_ls(x@authors_r3,
                                                  institutes_tb = x@institutes_r3,
                                                  keywords_chr = x@keywords_chr,
                                                  title_1L_chr = x@title_1L_chr) # Fake data / fl_nm ?
  if (identical(x@abstract_args_ls,list())) {
    x@abstract_args_ls <- NULL
  }
  write_header_fls(path_to_header_dir_1L_chr = paste0(paths_ls$path_to_ms_mkdn_dir_1L_chr,
                                                      "/Header"),
                   header_yaml_args_ls = header_yaml_args_ls,
                   abstract_args_ls = x@abstract_args_ls)
  write_custom_authors(paths_ls)
  params_ls <- list(X = x
    # output_type_1L_chr = x@outp_formats_chr[1],
    #                 results_ls = results_ls
                    )
    # params_ls$figures_in_body_lgl <- x@figures_in_body_lgl
    # params_ls$tables_in_body_lgl <- x@tables_in_body_lgl
    output_fl_1L_chr <- paste0(x@fl_nm_1L_chr,
                            ifelse(x@outp_formats_chr[1] == "Word",
                                   ".docx",
                                   paste0(".",
                                          tolower(x@outp_formats_chr[1]))))

  consent_1L_chr <- ready4::make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to write the file ",
                                                             output_fl_1L_chr,
                                                             " to the directory ",
                                                             paths_ls$path_to_ms_outp_dir_1L_chr,
                                                             " ?"),
                                            options_chr = c("Y", "N"),
                                            force_from_opts_1L_chr = T)

  if(consent_1L_chr == "Y"){
    rmarkdown::render(paste0(paths_ls$path_to_ms_mkdn_dir_1L_chr,
                             "/Parent_",
                             x@outp_formats_chr[1],
                             "/",
                             x@rmd_fl_nms_ls[names(x@rmd_fl_nms_ls)=="PDF"][[1]],
                             ".Rmd"),
                      output_format = NULL,
                      params = params_ls,
                      output_file = output_fl_1L_chr,
                      output_dir = paths_ls$path_to_ms_outp_dir_1L_chr)
    }
}
