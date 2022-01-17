author_Ready4showSynopsis <- function (x,
                                       results_ls = NULL) {
  if(is.na(x@fl_nm_1L_chr)){
    fl_nm_1L_chr <- "Manuscript"
  }else{
    fl_nm_1L_chr <- x@fl_nm_1L_chr
  }
  mkdn_data_dir_1L_chr <- x@a_Ready4showPaths@mkdn_data_dir_1L_chr
  outp_dir_1L_chr <- x@a_Ready4showPaths@outp_data_dir_1L_chr
  output_type_1L_chr <- x@outp_formats_chr[1]
  path_to_ms_mkdn_dir_1L_chr <- paste0(mkdn_data_dir_1L_chr,
                                   "/",
                                   x@a_Ready4showPaths@ms_mkdn_dir_1L_chr)
  path_to_ms_outp_dir_1L_chr <- paste0(outp_dir_1L_chr,
                                   "/",
                                   x@a_Ready4showPaths@ms__dir_1L_chr)
  write_new_dirs(c(mkdn_data_dir_1L_chr,
                   outp_dir_1L_chr,
                   path_to_ms_mkdn_dir_1L_chr,
                   path_to_ms_outp_dir_1L_chr))
  # if (!dir.exists(path_to_ms_mkdn_1L_dir)) {
  #   tmp_fl <- tempfile()
  #   download.file(paste0("https://github.com/ready4-dev/ttu_lng_ss/archive/refs/tags/v",
  #                        version_1L_chr, ".zip"), tmp_fl)
  #   utils::unzip(tmp_fl, exdir = mkdn_data_dir_1L_chr)
  #   unlink(tmp_fl)
  # }
  header_yaml_args_ls <- make_header_yaml_args_ls(x@authors_r3,
                                                  institutes_tb = x@institutes_r3,
                                                  keywords_chr = x@keywords_chr,
                                                  title_1L_chr = x@title_1L_chr) # Fake data / fl_nm ?
  if (identical(x@abstract_args_ls,list())) {
    x@abstract_args_ls <- NULL
  }
  write_header_fls(path_to_header_dir_1L_chr = paste0(path_to_ms_mkdn_dir_1L_chr,
                                                      "/Header"),
                   header_yaml_args_ls = header_yaml_args_ls,
                   abstract_args_ls = x@abstract_args_ls)
  params_ls <- list(output_type_1L_chr = output_type_1L_chr,
                    results_ls = results_ls)
    params_ls$figures_in_body_lgl <- x@figures_in_body_lgl
    params_ls$tables_in_body_lgl <- x@tables_in_body_lgl
  # Add interactive prompt
  rmarkdown::render(paste0(path_to_ms_mkdn_1L_dir,
                           "/Parent_",
                           output_type_1L_chr,
                           "/Parent_",
                           output_type_1L_chr,
                           ".Rmd"),
                    output_format = NULL,
                    params = params_ls,
                    output_file = paste0(fl_nm_1L_chr,
                                         ifelse(output_type_1L_chr == "Word",
                                                ".docx",
                                                paste0(".",tolower(output_type_1L_chr)))),
                    output_dir = path_to_ms_outp_dir_1L_chr)
}
