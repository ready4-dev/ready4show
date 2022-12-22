write_all_outp_dirs <- function(paths_ls){
  output_data_dir_1L_chr <- paste0(here::here(paths_ls$path_from_top_level_1L_chr),
                                   "/",
                                   paths_ls$write_to_dir_nm_1L_chr,
                                   "/Output")
  reports_dir_1L_chr <- paste0(here::here(paths_ls$path_from_top_level_1L_chr),"/",
                               paths_ls$write_to_dir_nm_1L_chr,"/Reports")
  mkdn_data_dir_1L_chr <- paste0(here::here(paths_ls$path_from_top_level_1L_chr),
                                 "/",
                                 paths_ls$write_to_dir_nm_1L_chr,
                                 "/Markdown")
  descv_outp_dir_1L_chr <- paste0(output_data_dir_1L_chr,"/_Descriptives")
  dv_dir_1L_chr <- paste0(output_data_dir_1L_chr,"/H_Dataverse")
  purrr::walk(c(paste0(here::here(paths_ls$path_from_top_level_1L_chr),
                       "/",
                       paths_ls$write_to_dir_nm_1L_chr),
                mkdn_data_dir_1L_chr,
                output_data_dir_1L_chr,
                reports_dir_1L_chr,
                descv_outp_dir_1L_chr,
                dv_dir_1L_chr),
              ~ if(!dir.exists(.x))
                dir.create(.x))
  paths_ls <- append(paths_ls,
                     list(output_data_dir_1L_chr = output_data_dir_1L_chr,
                          mkdn_data_dir_1L_chr = mkdn_data_dir_1L_chr,
                          reports_dir_1L_chr = reports_dir_1L_chr,
                          descv_outp_dir_1L_chr = descv_outp_dir_1L_chr,
                          dv_dir_1L_chr = dv_dir_1L_chr))
  return(paths_ls)
}
write_custom_authors <- function(paths_ls,
                                 rmd_fl_nms_ls = make_rmd_fl_nms_ls()){
  original_chr <- readLines(paste0(paths_ls$path_to_ms_mkdn_dir_1L_chr,
                                   "/Parent_PDF/",
                                   rmd_fl_nms_ls$PDF,
                                   ".Rmd"))
  placeholder_idx_1L_int <- which(original_chr=="## CUSTOM_AUTHORS_PLACEHOLDER ##")
  if(!identical(placeholder_idx_1L_int, integer(0))){
    header_chr <- readLines(paste0(paths_ls$path_to_ms_mkdn_dir_1L_chr,
                                   "/Header/header_common.yaml"))
    names_indcs_int <- c(which(header_chr == "author:")+1,
                        (1:length(header_chr))[header_chr %>% startsWith("  - name: ")])
    affiliations_indcs_int <- c(1:length(header_chr))[header_chr %>%
                                                        startsWith("    institute: ") | header_chr %>%
                                                       startsWith("      institute: ")]
    affiliations_ls <- header_chr[affiliations_indcs_int] %>%
      stringr::str_remove_all("      institute: ") %>%
      stringr::str_remove_all("    institute: ") %>%
      purrr::map(~stringr::str_sub(.x,start = 2, end = -2) %>%
                       strsplit(", ")
                       )
    affiliations_chr <- affiliations_ls %>%
      purrr::flatten() %>%
      purrr::flatten_chr() %>%
      unique()
    affiliations_chr <- affiliations_ls %>%
      purrr::map_chr(~paste0("    affiliation: ",
                             .x[[1]] %>%
                       purrr::map_int(~which(affiliations_chr ==.x)) %>%
                       paste0(collapse = ",")))
    replacement_chr <- purrr::reduce(1:length(names_indcs_int),
                                     .init = c("authors:"),
                                     ~{
                                       if(.y==1){
                                         c(.x,
                                           header_chr[names_indcs_int[.y]] %>%
                                             stringr::str_replace("  - ",
                                                                  "  - name: ") %>%
                                             stringi::stri_replace_last_regex(":",""),
                                           affiliations_chr[.y])
                                       }else{
                                         c(.x,
                                           header_chr[names_indcs_int[.y]] %>%
                                             stringr::str_replace("  - name: ",
                                                                  "  - name: "),
                                           affiliations_chr[.y])
                                       }
                                       })
    # replacement_chr <- readLines(paste0(paths_ls$path_to_ms_mkdn_dir_1L_chr,
    #                                     "/Parent_PDF/TEMPLATE.RMD")) %>% `[`(20:24)
    c(original_chr[1:(placeholder_idx_1L_int-1)],
      replacement_chr,
      original_chr[(placeholder_idx_1L_int+1):length(original_chr)]) %>%
      writeLines(con = paste0(paths_ls$path_to_ms_mkdn_dir_1L_chr,
                              "/Parent_PDF/",
                              rmd_fl_nms_ls$PDF,
                              ".Rmd"))
  }
}
write_csp_output <- function(path_to_csp_1L_chr, # Move to ready4show?
                             dv_ds_doi_1L_chr = NULL,
                             execute_1L_lgl = T){
  readLines(path_to_csp_1L_chr) %>%
    purrr::map_chr(~ifelse(.x == "knitr::opts_chunk$set(eval = F)",
                           "knitr::opts_chunk$set(eval = T)",
                           .x)) %>%
    writeLines(con = path_to_csp_1L_chr)
  path_to_r_script_1L_chr <- stringr::str_sub(path_to_csp_1L_chr,end=-3)
  knitr::purl(path_to_csp_1L_chr,
              path_to_r_script_1L_chr)
  readLines(path_to_csp_1L_chr) %>%
    purrr::map_chr(~ifelse(.x == "knitr::opts_chunk$set(eval = T)",
                           "knitr::opts_chunk$set(eval = F)",
                           .x)) %>%
    writeLines(con = path_to_csp_1L_chr)
  if(execute_1L_lgl){
    old_wd_1L_chr <- getwd()
    path_info_ls <- DescTools::SplitPath(path_to_r_script_1L_chr)
    setwd(path_info_ls$dirname)
    source(path_info_ls$fullfilename)
    setwd(old_wd_1L_chr)
  }
  rmarkdown::render(path_to_csp_1L_chr)
  if(!is.null(dv_ds_doi_1L_chr)){
    dataverse::add_dataset_file(paste0(stringr::str_sub(path_to_csp_1L_chr, end=-4),"pdf"),
                                dataset = dv_ds_doi_1L_chr,
                                description = "Methods Report 1: Complete Study Program")
  }
}
write_header_fls <- function(path_to_header_dir_1L_chr,
                             header_yaml_args_ls,
                             abstract_args_ls = NULL){
  if(!dir.exists(path_to_header_dir_1L_chr))
    dir.create(path_to_header_dir_1L_chr)
  rlang::exec(write_header_yaml, path_to_header_dir_1L_chr, !!!header_yaml_args_ls )
  if(!is.null(abstract_args_ls))
    abstract_args_ls$abstract_ls %>%
    make_abstract_lines() %>%
    writeLines(paste0(path_to_header_dir_1L_chr,
                      "/",
                      abstract_args_ls$fl_nm_1L_chr))
}
write_header_yaml <- function(path_to_header_dir_1L_chr,
                              fl_nm_1L_chr = "header.yaml",
                              authors_tb,
                              institutes_tb,
                              title_1L_chr = "Example title",
                              keywords_chr = c("Example keyword one", "Example keyword two"),
                              path_to_tmpl_header_1L_chr = NULL,
                              inc_quals_1L_lgl = F){
  if(is.null(path_to_tmpl_header_1L_chr)){
    tmpl_header_chr <- c("title: TITLE_PLACEHOLDER",
                         "author:",
                         "AUTHOR_PLACEHOLDER",
                         "institute:",
                         "INSTITUTE_PLACEHOLDER",
                         "keywords:  KEYWORDS_PLACEHOLDER")
  }else{
    tmpl_header_chr <- readLines(path_to_tmpl_header_1L_chr)
  }
  authors_tb <- authors_tb %>%
    dplyr::arrange(sequence_int)
  tmpl_header_chr  %>% purrr::map(~{
    if(.x=="AUTHOR_PLACEHOLDER"){
      make_authorship_lines(authors_tb,
                            inc_quals_1L_lgl = inc_quals_1L_lgl)
    }else{
      if(.x=="INSTITUTE_PLACEHOLDER"){
        make_institutes_lines(authors_tb,
                              institutes_tb = institutes_tb)
      }else{
        if(stringr::str_detect(.x,"KEYWORDS_PLACEHOLDER")){
          if(is.na(keywords_chr[1])){
            NA_character_
          }else{
            stringr::str_replace(.x,
                                 "KEYWORDS_PLACEHOLDER",
                                 paste0(keywords_chr, collapse = ", "))
          }
        }else{
          .x %>% stringr::str_replace_all("TITLE_PLACEHOLDER",
                                          title_1L_chr)
        }
      }
    }
  }) %>%
    purrr::flatten_chr() %>%
    purrr::discard(is.na) %>%
    writeLines(paste0(path_to_header_dir_1L_chr,
                      "/",
                      fl_nm_1L_chr))
}
write_main_outp_dir <- function(params_ls = NULL, # Move to ready4show?
                                use_fake_data_1L_lgl = F,
                                R_fl_nm_1L_chr = "aaaaaaaaaa.txt"){
  file.create(R_fl_nm_1L_chr)
  R_fl_nm_1L_chr <- list.files() %>% purrr::pluck(1)
  paths_ls <- make_paths_ls(append(params_ls,list(use_fake_data_1L_lgl = use_fake_data_1L_lgl)),
                                        depth_1L_int = 0) #
  paths_ls$path_to_current_1L_chr <- ifelse(!is.null(paths_ls$path_to_current_1L_chr),
                                            paths_ls$path_to_current_1L_chr,
                                            params_ls$path_to_current_1L_chr)
  here::i_am(paste0(paths_ls$path_from_top_level_1L_chr,
                    "/",
                    paths_ls$path_to_current_1L_chr,
                    "/",
                    R_fl_nm_1L_chr))
  dir.create(paste0(here::here(paths_ls$path_from_top_level_1L_chr),
                    "/",
                    paths_ls$write_to_dir_nm_1L_chr))
  paths_ls$R_fl_nm_1L_chr <- R_fl_nm_1L_chr
  paths_ls <- write_all_outp_dirs(paths_ls)
  return(paths_ls)
}
write_manuscript <- function(abstract_args_ls = NULL,# Move to ready4show?
                             input_params_ls = NULL,
                             results_ls = NULL,
                             figures_in_body_lgl = NULL,
                             ms_mkdn_fl_nm_1L_chr = "TTU_Study_Manuscript",
                             ms_mkdn_parent_1L_chr = "ready4-dev",
                             ms_mkdn_repo_1L_chr = "ttu_lng_ss",
                             output_type_1L_chr = NULL,
                             tables_in_body_lgl = NULL,
                             title_1L_chr = "Scientific manuscript",
                             version_1L_chr = "0.5",
                             write_to_dv_1L_lgl = F){
  mkdn_data_dir_1L_chr <- ifelse(!is.null(input_params_ls),
                                 input_params_ls$path_params_ls$paths_ls$mkdn_data_dir_1L_chr,
                                 results_ls$path_params_ls$paths_ls$mkdn_data_dir_1L_chr)
  outp_dir_1L_chr <- ifelse(!is.null(input_params_ls),
                            input_params_ls$path_params_ls$paths_ls$output_data_dir_1L_chr,
                            results_ls$path_params_ls$paths_ls$output_data_dir_1L_chr)
  output_type_1L_chr <- ifelse(!is.null(output_type_1L_chr),
                               output_type_1L_chr,
                               ifelse(!is.null(input_params_ls),
                                      input_params_ls$output_format_ls$manuscript_outp_1L_chr,
                                      results_ls$output_format_ls$manuscript_outp_1L_chr))
  path_to_ms_mkdn_1L_dir <- paste0(mkdn_data_dir_1L_chr,
                                   "/",
                                   ms_mkdn_repo_1L_chr, # MAKE ARG
                                   "-",
                                   version_1L_chr)
  path_to_results_dir_1L_chr <- ifelse(!is.null(input_params_ls),
                                       input_params_ls$path_params_ls$paths_ls$reports_dir_1L_chr,
                                       results_ls$path_params_ls$paths_ls$reports_dir_1L_chr)
  if(!dir.exists(path_to_ms_mkdn_1L_dir)){
    tmp_fl <- tempfile()
    download.file(paste0("https://github.com/",
                         ms_mkdn_parent_1L_chr,
                         "/",
                         ms_mkdn_repo_1L_chr,
                         "/archive/refs/tags/v",
                         version_1L_chr,
                         ".zip"), # Use piggyback
                  tmp_fl)
    utils::unzip(tmp_fl,
                 exdir = mkdn_data_dir_1L_chr)
    unlink(tmp_fl)
  }
  if(!is.null(input_params_ls)){
    header_yaml_args_ls <- input_params_ls$header_yaml_args_ls
  }else{
    header_yaml_args_ls <- results_ls$header_yaml_args_ls
  }
  # if(is.null(results_ls)){ # PROBLEMATIC - MAKE_RESULTS_LS NOT MIGRATED IN. TRIALING REMOVAL.
  #   results_ls <- make_results_ls(dv_ds_nm_and_url_chr = input_params_ls$path_params_ls$dv_ds_nm_and_url_chr,
  #                                 output_format_ls = input_params_ls$output_format_ls,
  #                                 params_ls_ls = input_params_ls,
  #                                 path_params_ls = input_params_ls$path_params_ls,
  #                                 study_descs_ls = input_params_ls$study_descs_ls,
  #                                 var_nm_change_lup = input_params_ls$study_descs_ls$var_nm_change_lup,
  #                                 version_1L_chr = version_1L_chr)
  # }
  if(is.null(abstract_args_ls)){
    abstract_args_ls <- make_abstract_args_ls(results_ls)
  }
  write_header_fls(path_to_header_dir_1L_chr = paste0(path_to_ms_mkdn_1L_dir,"/Header"),
                               header_yaml_args_ls = header_yaml_args_ls,
                               abstract_args_ls = abstract_args_ls)
  params_ls <- list(output_type_1L_chr = output_type_1L_chr,
                    results_ls = results_ls)
  if(!is.null(figures_in_body_lgl))
    params_ls$figures_in_body_lgl <- figures_in_body_lgl
  if(!is.null(tables_in_body_lgl))
    params_ls$tables_in_body_lgl <- tables_in_body_lgl
  rmarkdown::render(paste0(path_to_ms_mkdn_1L_dir,
                           "/",
                           output_type_1L_chr,
                           "/",
                           output_type_1L_chr,
                           ".Rmd"),
                    output_format = NULL,
                    params = params_ls,
                    output_file = paste0(ms_mkdn_fl_nm_1L_chr, # Make arg
                                         ifelse(output_type_1L_chr == "Word",".docx",".pdf")),
                    output_dir = path_to_results_dir_1L_chr)
  if(write_to_dv_1L_lgl){
    if(!is.null(input_params_ls)){
      paths_ls <- input_params_ls$path_params_ls$paths_ls
    }else{
      paths_ls <- results_ls$path_params_ls$paths_ls
    }
    ready4::write_to_dv_with_wait(dss_tb = tibble::tibble(ds_obj_nm_chr = ms_mkdn_fl_nm_1L_chr, # Make arg
                                                          title_chr = title_1L_chr),
                                  dv_nm_1L_chr = ifelse(!is.null(input_params_ls),
                                                        input_params_ls$path_params_ls$dv_ds_nm_and_url_chr[1],
                                                        results_ls$path_params_ls$dv_ds_nm_and_url_chr[1]),
                                  ds_url_1L_chr = ifelse(!is.null(input_params_ls),
                                                         input_params_ls$path_params_ls$dv_ds_nm_and_url_chr[2],
                                                         results_ls$path_params_ls$dv_ds_nm_and_url_chr[2]),
                                  parent_dv_dir_1L_chr = paths_ls$dv_dir_1L_chr,
                                  paths_to_dirs_chr = paths_ls$reports_dir_1L_chr,
                                  inc_fl_types_chr = ifelse(output_type_1L_chr == "Word",".docx",".pdf"),
                                  paths_are_rltv_1L_lgl = F)
  }
  results_ls$path_params_ls$paths_ls$path_to_ms_mkdn_1L_dir <- path_to_ms_mkdn_1L_dir
  saveRDS(results_ls,paste0(outp_dir_1L_chr,"/results_ls.RDS"))
  return(results_ls)
}
write_mdl_plt_fl <- function (plt_fn = NULL, fn_args_ls = NULL, path_to_write_to_1L_chr,
                              plt_nm_1L_chr, grpx_fn = grDevices::png, units_1L_chr = "in",
                              width_1L_dbl = 6, height_1L_dbl = 6, rsl_1L_dbl = 300)
{
  if (!is.null(plt_fn)) {
    path_to_plot_1L_chr <- paste0(path_to_write_to_1L_chr,
                                  "/", plt_nm_1L_chr, ifelse(identical(grpx_fn, grDevices::png),
                                                             ".png", ".tiff"))
    rlang::exec(grpx_fn, !!!list(path_to_plot_1L_chr, units = units_1L_chr,
                                 width = width_1L_dbl, height = height_1L_dbl, res = rsl_1L_dbl))
    plt <- rlang::exec(plt_fn, !!!fn_args_ls)
    print(plt)
    grDevices::dev.off()
  }
  else {
    path_to_plot_1L_chr <- NA_character_
  }
  return(path_to_plot_1L_chr)
}
write_mkdn_from_pkg <- function(pkg_nm_1L_chr,
                                dest_dir_1L_chr = "Markdown",
                                overwrite_1L_lgl = F){
  all_mkdn_chr <- system.file("Markdown",
                              package = pkg_nm_1L_chr) %>%
    list.files()
  is_dir_lgl <- all_mkdn_chr %>% purrr::map_lgl(~system.file(paste0("Markdown/",
                                                                    .x),
                                                             package=pkg_nm_1L_chr) %>%
                                                  dir.exists())
  all_mkdn_chr[is_dir_lgl] %>% purrr::walk(~{
    if(!dir.exists(paste0(dest_dir_1L_chr,"/",.x)))
      dir.create(paste0(dest_dir_1L_chr,"/",.x))
  })
  all_mkdn_files_chr <- system.file("Markdown",package=pkg_nm_1L_chr) %>% list.files(recursive = T)
  all_mkdn_files_chr %>% purrr::walk(~{
    if(!file.exists(paste0(dest_dir_1L_chr,"/",.x)) | (file.exists(paste0(dest_dir_1L_chr,"/",.x)) & overwrite_1L_lgl))
      file.create(paste0(dest_dir_1L_chr,"/",.x))
  })
}
write_report <- function(params_ls,# Move to ready4show [????]
                         paths_ls,
                         rprt_nm_1L_chr,
                         abstract_args_ls = NULL,
                         header_yaml_args_ls = NULL,
                         rprt_lup){ # REMOVE NULL
  rprt_type_ls <- rprt_lup %>%
    make_rprt_type_ls(rprt_nm_1L_chr = rprt_nm_1L_chr)
  here::i_am(paste0(paths_ls$path_from_top_level_1L_chr,
                    "/",
                    paths_ls$path_to_current_1L_chr,
                    "/",
                    paths_ls$R_fl_nm_1L_chr
  ))
  args_ls <- list(rprt_type_ls = rprt_type_ls,
                  params_ls = params_ls,
                  output_type_1L_chr = params_ls$output_type_1L_chr,
                  path_to_prjs_dir_1L_chr = here::here(paths_ls$path_from_top_level_1L_chr),
                  prj_dir_1L_chr = paths_ls$write_to_dir_nm_1L_chr,
                  header_yaml_args_ls = header_yaml_args_ls,
                  abstract_args_ls = abstract_args_ls,
                  reports_dir_1L_chr = "Reports",
                  rltv_path_to_data_dir_1L_chr = "../Output",
                  nm_of_mkdn_dir_1L_chr = "Markdown")
  rlang::exec(write_rprt_from_tmpl,!!!args_ls)
}
write_reporting_dir <- function(path_to_write_to_1L_chr = getwd(),# Move to ready4show
                                new_dir_nm_1L_chr = "TTU_Project",
                                overwrite_1L_lgl = FALSE,
                                path_to_rmd_dir_1L_chr){ # system.file("Project/CSP", package = "TTU")
  path_to_prjt_dir_1L_chr <- paste0(path_to_write_to_1L_chr,"/",new_dir_nm_1L_chr)
  if(!dir.exists(path_to_prjt_dir_1L_chr))
    dir.create(path_to_prjt_dir_1L_chr)
  # path_to_RMD_dir_1L_chr <- system.file("Project/CSP", package = "TTU")
  file.copy(path_to_rmd_dir_1L_chr, path_to_prjt_dir_1L_chr,
            recursive = T, overwrite = overwrite_1L_lgl)
  path_to_csp_1L_chr <- paste0(path_to_prjt_dir_1L_chr,
                               "/CSP/CSP.Rmd")
  return(path_to_csp_1L_chr)
}
write_rndrd_rprt <- function(rprt_type_ls,
                             params_ls = list(output_type_1L_chr = "HTML"),
                             #paths_to_fls_to_copy_chr = NA_character_,
                             path_to_write_dirs_to_1L_chr = NA_character_,
                             nm_of_mkdn_dir_1L_chr = "Markdown",
                             path_to_rprt_dir_1L_chr = "./",
                             header_yaml_args_ls = NULL,
                             abstract_args_ls = NULL,
                             overwrite_1L_lgl = T){
  if (!is.na(path_to_write_dirs_to_1L_chr)) {
    file.copy(rprt_type_ls$path_to_RMD_dir_1L_chr,
              path_to_write_dirs_to_1L_chr,
              recursive = T,
              overwrite = overwrite_1L_lgl)
    dir_nm_1L_chr <- rprt_type_ls$path_to_RMD_dir_1L_chr %>%
      normalizePath() %>%
      strsplit("\\\\") %>%
      purrr::pluck(1) %>%
      tail(1)
    path_to_mkdn_dir_1L_chr <- paste0(path_to_write_dirs_to_1L_chr,
                                      "/", nm_of_mkdn_dir_1L_chr)
    if(dir_nm_1L_chr != nm_of_mkdn_dir_1L_chr)
      file.rename(paste0(path_to_write_dirs_to_1L_chr,
                         "/", dir_nm_1L_chr),
                  path_to_mkdn_dir_1L_chr)

    # if (!dir.exists(path_to_mkdn_dir_1L_chr))
    #   dir.create(path_to_mkdn_dir_1L_chr)
    # if (is.na(paths_to_fls_to_copy_chr[1])){
    #   paths_to_fls_to_copy_chr <- list.files(rprt_type_ls$path_to_RMD_dir_1L_chr,
    #                                          full.names = T)
    # }
    path_to_wd_1L_chr <- path_to_mkdn_dir_1L_chr
  }else{
    path_to_wd_1L_chr <- rprt_type_ls$path_to_RMD_dir_1L_chr
  }
  if (!dir.exists(path_to_rprt_dir_1L_chr))
    dir.create(path_to_rprt_dir_1L_chr)
  if(!is.null(header_yaml_args_ls)){
    write_header_fls(path_to_header_dir_1L_chr = paste0(path_to_mkdn_dir_1L_chr,
                                                       "/Header"),
                     header_yaml_args_ls,
                     abstract_args_ls = abstract_args_ls)
  }
  path_to_RMD_1L_chr <- paste0(path_to_wd_1L_chr, "/", rprt_type_ls$nm_of_RMD_1L_chr)
  rmarkdown::render(path_to_RMD_1L_chr,
                    output_format = switch(params_ls$output_type_1L_chr,
                                           PDF = "bookdown::pdf_book",
                                           HTML = "bookdown::html_document2",
                                           Word = "officedown::rdocx_document"),
                    output_yaml = paste0(path_to_wd_1L_chr, "/", rprt_type_ls$rltv_path_to_outp_yaml_1L_chr),
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
                       prj_dir_dir_1L_chr = "My_Project",
                       reports_dir_1L_chr = "Reports",
                       rltv_path_to_data_dir_1L_chr = "../Output",
                       nm_of_mkdn_dir_1L_chr = "Markdown",
                       push_copy_to_dv_1L_lgl = T,
                       append_params_ls = NULL){
  path_to_outp_dir_1L_chr <- paste0(path_to_prjs_dir_1L_chr,"/",prj_dir_dir_1L_chr)#"../../../../Data/Project/Utility_Models",
  path_to_rprt_dir_1L_chr <- paste0(path_to_outp_dir_1L_chr, "/", reports_dir_1L_chr)
  if(!dir.exists(path_to_rprt_dir_1L_chr))
    dir.create(path_to_rprt_dir_1L_chr)
  path_to_rprt_dir_1L_chr <- normalizePath(path_to_rprt_dir_1L_chr)
  params_ls <- list(outp_smry_ls = outp_smry_ls,
                   output_type_1L_chr = output_type_1L_chr,
                   rltv_path_to_data_dir_1L_chr = rltv_path_to_data_dir_1L_chr,
                   section_type_1L_chr = section_type_1L_chr)
  if(!is.null(append_params_ls)){
    params_ls <- append(params_ls, append_params_ls)
  }
  write_rndrd_rprt(rprt_type_ls = rprt_type_ls,
                   params_ls = params_ls,
                   path_to_write_dirs_to_1L_chr = normalizePath(path_to_outp_dir_1L_chr),
                   nm_of_mkdn_dir_1L_chr = nm_of_mkdn_dir_1L_chr,
                   path_to_rprt_dir_1L_chr = path_to_rprt_dir_1L_chr)
  if(!is.null(outp_smry_ls$dv_ls) & push_copy_to_dv_1L_lgl) {
    outp_smry_ls$rprt_dss_tb <- tibble::tibble(ds_obj_nm_chr = rprt_type_ls$file_nm_1L_chr,
                                               title_chr = rprt_type_ls$title_1L_chr)
    ready4::write_to_dv_with_wait(outp_smry_ls$rprt_dss_tb,
                                  dv_nm_1L_chr = outp_smry_ls$dv_ls$dv_nm_1L_chr,
                                  ds_url_1L_chr = outp_smry_ls$dv_ls$ds_url_1L_chr,
                                  parent_dv_dir_1L_chr = outp_smry_ls$dv_ls$parent_dv_dir_1L_chr,
                                  paths_to_dirs_chr = paste0(path_to_outp_dir_1L_chr,
                                                             "/", reports_dir_1L_chr),
                                  inc_fl_types_chr = paste0(".",
                                                            ifelse(output_type_1L_chr == "Word",
                                                                   "docx",
                                                                   tolower(output_type_1L_chr))))
  }
  return(outp_smry_ls)
}
write_rprt_from_tmpl <- function (rprt_type_ls,
                                  params_ls = NULL,
                                  output_type_1L_chr = "PDF",
                                  path_to_prjs_dir_1L_chr, #  = "../../../../Data/Project"
                                  prj_dir_1L_chr = "Fake",
                                  header_yaml_args_ls = NULL,
                                  abstract_args_ls = NULL,
                                  reports_dir_1L_chr = "Reports",
                                  rltv_path_to_data_dir_1L_chr = "../Output",
                                  nm_of_mkdn_dir_1L_chr = "Markdown"
){
  path_to_outp_dir_1L_chr <- paste0(path_to_prjs_dir_1L_chr,
                                     "/", prj_dir_1L_chr)
  path_to_rprt_dir_1L_chr <- paste0(path_to_outp_dir_1L_chr,
                                    "/", reports_dir_1L_chr)
  if (!dir.exists(path_to_rprt_dir_1L_chr))
    dir.create(path_to_rprt_dir_1L_chr)
  path_to_rprt_dir_1L_chr <- normalizePath(path_to_rprt_dir_1L_chr)
  write_rndrd_rprt(rprt_type_ls = rprt_type_ls,
                   params_ls = params_ls,
                   path_to_write_dirs_to_1L_chr = normalizePath(path_to_outp_dir_1L_chr),
                   nm_of_mkdn_dir_1L_chr = nm_of_mkdn_dir_1L_chr,
                   path_to_rprt_dir_1L_chr = path_to_rprt_dir_1L_chr,
                   header_yaml_args_ls = header_yaml_args_ls,
                   abstract_args_ls = abstract_args_ls)
}
write_rprt_with_rcrd <- function(path_to_outp_fl_1L_chr,# Move to ready4show
                                 paths_ls,
                                 header_yaml_args_ls = NULL,
                                 rprt_lup,# REMOVE NULL
                                 use_fake_data_1L_lgl = F,
                                 rprt_nm_1L_chr = "AAA_TTU_MDL_CTG", # REMOVE DEFAULT
                                 rcrd_nm_1L_chr = "AAA_RPRT_WRTNG_MTH",
                                 reference_1L_int = NULL,
                                 start_at_int = c(2,1),
                                 output_type_1L_chr = "PDF",
                                 rprt_output_type_1L_chr = "PDF",
                                 nbr_of_digits_1L_int = 2L,
                                 abstract_args_ls = NULL,
                                 main_rprt_append_ls = NULL,
                                 rcrd_rprt_append_ls = NULL){
  # NEED TO INVESTIGATE TFM LGC IN SPECIFIC / TTU
  #   rprt_lup <- rprt_lup %>% transform_rprt_lup(add_suplry_rprt_1L_lgl = !is.null(reference_1L_int),
  #                                               add_sharing_rprt_1L_lgl = F,#T
  #                                               start_at_int = start_at_int,
  #                                               reference_1L_int = reference_1L_int)
  # }
  params_ls <- list(abstract_args_ls = NULL,
                    eval_1L_lgl = F,
                    header_yaml_args_ls = header_yaml_args_ls,
                    output_type_1L_chr = rprt_output_type_1L_chr,
                    nbr_of_digits_1L_int = nbr_of_digits_1L_int,
                    rprt_lup = rprt_lup,
                    rprt_nm_1L_chr = rprt_nm_1L_chr,
                    rprt_output_type_1L_chr = output_type_1L_chr,
                    rprt_subtitle_1L_chr = ready4::get_from_lup_obj(rprt_lup,
                                                                    match_value_xx = rprt_nm_1L_chr,
                                                                    match_var_nm_1L_chr = "rprt_nms_chr",
                                                                    target_var_nm_1L_chr = "title_chr",
                                                                    evaluate_1L_lgl = F),
                    subtitle_1L_chr = ready4::get_from_lup_obj(rprt_lup,
                                                               match_value_xx = "AAA_RPRT_WRTNG_MTH",
                                                               match_var_nm_1L_chr = "rprt_nms_chr",
                                                               target_var_nm_1L_chr = "title_chr",
                                                               evaluate_1L_lgl = F),
                    use_fake_data_1L_lgl = use_fake_data_1L_lgl) %>%
    append(rcrd_rprt_append_ls)
  params_ls %>%
    write_report(paths_ls = paths_ls,
                 rprt_nm_1L_chr = rcrd_nm_1L_chr,
                 abstract_args_ls = NULL,
                 header_yaml_args_ls = header_yaml_args_ls,
                 rprt_lup = rprt_lup)
  list(outp_smry_ls =  append(readRDS(path_to_outp_fl_1L_chr),
                              list(rprt_lup = rprt_lup)),
       output_type_1L_chr = output_type_1L_chr,
       subtitle_1L_chr = ready4::get_from_lup_obj(rprt_lup,
                                                  match_value_xx = rprt_nm_1L_chr,
                                                  match_var_nm_1L_chr = "rprt_nms_chr",
                                                  target_var_nm_1L_chr = "title_chr",
                                                  evaluate_1L_lgl = F)) %>%
    append(main_rprt_append_ls) %>%
    write_report(paths_ls = paths_ls,
                 rprt_nm_1L_chr = rprt_nm_1L_chr,
                 abstract_args_ls = abstract_args_ls,
                 header_yaml_args_ls = header_yaml_args_ls,
                 rprt_lup = rprt_lup)
}
