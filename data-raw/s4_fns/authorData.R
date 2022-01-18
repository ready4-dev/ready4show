authorData_Ready4showSynopsis <- function(x,
                                          tmpl_version_1_L_chr = "0.0.9.1"){
  if(!is.na(x@a_Ready4showPaths@mkdn_source_dir_1L_chr)){
    mkdn_source_dir_1L_chr <- x@a_Ready4showPaths@mkdn_source_dir_1L_chr
  }else{
    temp_fl_1L_chr <- tempfile()
    temp_dir_1L_chr <- tempdir()
    download.file(paste0("https://github.com/ready4-dev/ms_tmpl/archive/refs/tags/v",
                         tmpl_version_1_L_chr,
                         ".zip"),
                  temp_fl_1L_chr)
    utils::unzip(temp_fl_1L_chr,
                 exdir = temp_dir_1L_chr)
    unlink(temp_fl_1L_chr)
    mkdn_source_dir_1L_chr <- paste0(temp_dir_1L_chr,"/ms_tmpl-",tmpl_version_1_L_chr)
  }
  paths_ls <- manufacture(x,
                          what_1L_chr = "paths_ls")
  write_new_dirs(paths_ls %>%
                   purrr::flatten_chr())
  ready4::write_new_files(paths_ls$path_to_ms_mkdn_dir_1L_chr,
                          source_paths_ls = list(mkdn_source_dir_1L_chr))
  if(exists("temp_dir_1L_chr")){
    unlink(temp_dir_1L_chr)
  }
}
