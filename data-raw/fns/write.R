write_mkdn_from_pkg <- function(pkg_nm_1L_chr,
                                destn_dir_1L_chr = "Markdown",
                                overwrite_1L_lgl = F){
  all_mkdn_chr <- system.file("Markdown",package=pkg_nm_1L_chr) %>% list.files()
  is_dir_lgl <- all_mkdn_chr %>% purrr::map_lgl(~system.file(paste0("Markdown/",.x),package=pkg_nm_1L_chr) %>% dir.exists())
  all_mkdn_chr[is_dir_lgl] %>% purrr::walk(~{
    if(!dir.exists(paste0(destn_dir_1L_chr,"/",.x)))
      dir.create(paste0(destn_dir_1L_chr,"/",.x))
  })
  all_mkdn_files_chr <- system.file("Markdown",package=pkg_nm_1L_chr) %>% list.files(recursive = T)
  all_mkdn_files_chr %>% purrr::walk(~{
    if(!file.exists(paste0(destn_dir_1L_chr,"/",.x)) | (file.exists(paste0(destn_dir_1L_chr,"/",.x)) & overwrite_1L_lgl))
      file.create(paste0(destn_dir_1L_chr,"/",.x))
  })
}
write_rndrd_rprt <- function(rprt_type_ls,
                             params_ls = list(output_type_1L_chr = "HTML"),
                             paths_to_fls_to_copy_chr = NA_character_,
                             path_to_write_dirs_to_1L_chr = NA_character_,
                             nm_of_mkdn_dir_1L_chr = "Markdown",
                             path_to_rprt_dir_1L_chr = "./",
                             overwrite_1L_lgl = T){
  if (!is.na(path_to_write_dirs_to_1L_chr)) {
    path_to_mkdn_dir_1L_chr <- paste0(path_to_write_dirs_to_1L_chr,
                                      "/", nm_of_mkdn_dir_1L_chr)
    if (!dir.exists(path_to_mkdn_dir_1L_chr))
      dir.create(path_to_mkdn_dir_1L_chr)
    if (is.na(paths_to_fls_to_copy_chr[1]))
      paths_to_fls_to_copy_chr <- list.files(rprt_type_ls$path_to_RMD_dir_1L_chr,
                                             full.names = T)
    file.copy(paths_to_fls_to_copy_chr, path_to_mkdn_dir_1L_chr,
              overwrite = overwrite_1L_lgl)
    path_to_wd_1L_chr <- path_to_mkdn_dir_1L_chr
  }else{
    path_to_wd_1L_chr <- rprt_type_ls$path_to_RMD_dir_1L_chr
  }
  if (!dir.exists(path_to_rprt_dir_1L_chr))
    dir.create(path_to_rprt_dir_1L_chr)
  path_to_RMD_1L_chr <- paste0(path_to_wd_1L_chr, "/", rprt_type_ls$nm_of_RMD_1L_chr)
  rmarkdown::render(path_to_RMD_1L_chr,
                    output_format = switch(params_ls$output_type_1L_chr,
                                           PDF = "bookdown::pdf_book",
                                           HTML = "bookdown::html_document2",
                                           Word = "officedown::rdocx_document"),
                    output_yaml = paste0(path_to_wd_1L_chr, "/", rprt_type_ls$rltv_path_to_outpt_yaml_1L_chr),
                    params = params_ls,
                    envir = new.env(),
                    output_file = paste0(rprt_type_ls$file_nm_1L_chr, ".",
                                         ifelse(params_ls$output_type_1L_chr == "Word",
                                                "docx", tolower(params_ls$output_type_1L_chr))),
                    output_dir = path_to_rprt_dir_1L_chr)
}
write_rprt <- function(rprt_type_ls,
                       outp_smry_ls,
                       output_type_1L_chr = "PDF",
                       section_type_1L_chr = "#",
                       path_to_prjs_dir_1L_chr = "../../../../Data/Project",
                       prt_dir_dir_1L_chr = "My_Project",
                       reports_dir_1L_chr = "Reports",
                       rltv_path_to_data_dir_1L_chr = "../Output",
                       nm_of_mkdn_dir_1L_chr = "Markdown",
                       push_copy_to_dv_1L_lgl = T){
  path_to_outpt_dir_1L_chr <- paste0(path_to_prjs_dir_1L_chr,"/",prt_dir_dir_1L_chr)#"../../../../Data/Project/Utility_Models",
  path_to_rprt_dir_1L_chr <- paste0(path_to_outpt_dir_1L_chr, "/", reports_dir_1L_chr)
  if(!dir.exists(path_to_rprt_dir_1L_chr))
    dir.create(path_to_rprt_dir_1L_chr)
  path_to_rprt_dir_1L_chr <- normalizePath(path_to_rprt_dir_1L_chr)
  write_rndrd_rprt(rprt_type_ls = rprt_type_ls,
                   paths_to_fls_to_copy_chr = list.files(rprt_type_ls$path_to_RMD_dir_1L_chr,
                                                         full.names = T),
                   params_ls = list(outp_smry_ls = outp_smry_ls,
                                    output_type_1L_chr = output_type_1L_chr,
                                    rltv_path_to_data_dir_1L_chr = rltv_path_to_data_dir_1L_chr,
                                    section_type_1L_chr = section_type_1L_chr),
                   path_to_write_dirs_to_1L_chr = normalizePath(path_to_outpt_dir_1L_chr),
                   nm_of_mkdn_dir_1L_chr = nm_of_mkdn_dir_1L_chr,
                   path_to_rprt_dir_1L_chr = path_to_rprt_dir_1L_chr)
  if(!is.null(outp_smry_ls$dv_ls) & push_copy_to_dv_1L_lgl) {
    outp_smry_ls$rprt_dss_tb <- tibble::tibble(ds_obj_nm_chr = rprt_type_ls$file_nm_1L_chr,
                                               title_chr = rprt_type_ls$title_1L_chr)
    ready4use::write_fls_to_dv_ds(outp_smry_ls$rprt_dss_tb,
                                  dv_nm_1L_chr = outp_smry_ls$dv_ls$dv_nm_1L_chr,
                                  ds_url_1L_chr = outp_smry_ls$dv_ls$ds_url_1L_chr,
                                  parent_dv_dir_1L_chr = outp_smry_ls$dv_ls$parent_dv_dir_1L_chr,
                                  paths_to_dirs_chr = paste0(path_to_outpt_dir_1L_chr,
                                                             "/", reports_dir_1L_chr),
                                  inc_fl_types_chr = paste0(".",
                                                            ifelse(output_type_1L_chr == "Word",
                                                                   "docx",
                                                                   tolower(output_type_1L_chr))))
  }
  return(outp_smry_ls)
}