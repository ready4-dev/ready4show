authorData_Ready4showSynopsis <- function(x){
  # if(is.na(x@a_Ready4showPaths@mkdn_source_dir_1L_chr)){
  #   mkdn_source_dir_1L_chr <- system.file("Markdown/Manuscript", package = "ready4show")
  # }else{
  #   mkdn_source_dir_1L_chr <- x@a_Ready4showPaths@mkdn_source_dir_1L_chr
  # }
  paths_ls <- manufacture(x,
                          what_1L_chr = "paths_ls")
  write_new_dirs(paths_ls %>%
                   purrr::flatten_chr())
  ready4::write_new_files(paths_ls$path_to_ms_mkdn_dir_1L_chr,
                          source_paths_ls = list(x@a_Ready4showPaths@mkdn_source_dir_1L_chr))
}
