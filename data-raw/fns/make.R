make_abstract_args_ls <- function(background_1L_chr = NA_character_,
                                  objectives_1L_chr = NA_character_,
                                  methods_1L_chr = NA_character_,
                                  results_1L_chr = NA_character_,
                                  conclusions_1L_chr = NA_character_,
                                  data_1L_chr = NA_character_,
                                  fl_nm_1L_chr = "abstract.txt"){
  if(is.na(background_1L_chr))
    background_1L_chr <- "Brief background to study goes here."
  if(is.na(objectives_1L_chr))
    objectives_1L_chr <- "Brief study objectives goes here."
  if(is.na(methods_1L_chr))
    methods_1L_chr <- "Brief description of methods goes here."
  if(is.na(results_1L_chr))
    results_1L_chr <- "Brief summary of results goes here."
  if(is.na(conclusions_1L_chr))
    conclusions_1L_chr <- "Headline conclusions go here."
  if(is.na(data_1L_chr))
    data_1L_chr <- "Statement about availability of study data and materials goes here."
  abstract_args_ls <- list(abstract_ls = list(Background = background_1L_chr,
                                              Objectives = objectives_1L_chr,
                                              Methods = methods_1L_chr,
                                              Results = results_1L_chr,
                                              Conclusions = conclusions_1L_chr,
                                              Data = data_1L_chr),
                           fl_nm_1L_chr = fl_nm_1L_chr)

  return(abstract_args_ls)
}
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
make_authorship_lines <- function(authors_tb,
                                  inc_quals_1L_lgl = F){
  authorship_lines_chr <- 1:nrow(authors_tb) %>% purrr::map(~ make_sngl_author_lines(authors_tb,
                                                                                     slice_1L_int = .x,
                                                                                     inc_quals_1L_lgl = inc_quals_1L_lgl)) %>%
    purrr::flatten_chr()
  return(authorship_lines_chr)
}
make_default_paths <- function(){
  x_Ready4showPaths <- Ready4showPaths(mkdn_data_dir_1L_chr = 'Markdown',
                                       mkdn_source_dir_1L_chr = NA_character_,#system.file('Markdown/Manuscript', package = 'ready4show')
                                       ms_mkdn_dir_1L_chr = 'Manuscript',
                                       ms_dir_1L_chr = 'Manuscript',
                                       outp_data_dir_1L_chr = 'Output',
                                       reports_dir_1L_chr = 'Reports')
  return(x_Ready4showPaths)
}
make_eq_ref <- function(eq_nm_1L_chr,
                         output_type_1L_chr){
  eq_ref_1L_chr <- ifelse(output_type_1L_chr == "Word",
                           paste0("\\@ref(eq:",eq_nm_1L_chr,")"),
                           paste0("\\ref{eq:",eq_nm_1L_chr,"}"))
  return(eq_ref_1L_chr)
}
make_header_yaml_args_ls <- function(authors_tb, # Migrated from specific / TTU
                                     institutes_tb,
                                     title_1L_chr,
                                     keywords_chr,
                                     fl_nm_1L_chr = "header_common.yaml",
                                     use_fake_data_1L_lgl = F){
  if(!use_fake_data_1L_lgl){
    header_yaml_args_ls <- list(authors_tb = authors_tb,
                                institutes_tb = institutes_tb,
                                fl_nm_1L_chr = "header_common.yaml",
                                title_1L_chr = title_1L_chr,
                                keywords_chr = keywords_chr)
  }else{
    data("authors_tb", package = "ready4show", envir = environment())
    data("institutes_tb", package = "ready4show", envir = environment())
    header_yaml_args_ls <- make_header_yaml_args_ls(authors_tb = authors_tb,
                                                    institutes_tb = institutes_tb,
                                                    title_1L_chr = "A hypothetical study using fake data for instructional purposes only",
                                                    keywords_chr = c("this","is","a","replication","using","fake","data","do", "not","cite"))
  }
  return(header_yaml_args_ls)
}
make_institutes_lines <- function(authors_tb,
                                  institutes_tb){
  institutes_chr <- authors_tb$institute_chr %>% purrr::map(~stringr::str_remove_all(.x,"\"") %>%
                                                              strsplit(", ")) %>%
    purrr::flatten() %>% purrr:::flatten_chr() %>% unique()
  institutes_lines_chr <- institutes_chr %>% purrr::map_chr(~{
    paste0("  - ",.x,": ",
           ready4::get_from_lup_obj(institutes_tb,
                                       match_var_nm_1L_chr = "short_name_chr",
                                       match_value_xx = .x,
                                       target_var_nm_1L_chr = "long_name_chr",
                                       evaluate_1L_lgl = F) %>%
             stringr::str_remove_all("\""))
  })
  return(institutes_lines_chr)
}
make_output_format_ls <- function(manuscript_outp_1L_chr = "Word", # Migrated from specific / TTU
                                  manuscript_digits_1L_int = 2L,
                                  supplementary_outp_1L_chr = "PDF",
                                  supplementary_digits_1L_int = 2L){
  output_format_ls <- list(manuscript_outp_1L_chr = manuscript_outp_1L_chr,
                           manuscript_digits_1L_int = manuscript_digits_1L_int,
                           supplementary_outp_1L_chr = supplementary_outp_1L_chr,
                           supplementary_digits_1L_int = supplementary_digits_1L_int)
  return(output_format_ls)
}
make_paths_ls <- function(params_ls,
                          depth_1L_int = 1){
  paths_ls <- list(path_from_top_level_1L_chr = params_ls$path_from_top_level_1L_chr,
                   path_to_data_from_top_level_chr = params_ls$path_to_data_from_top_level_chr)
  if(!params_ls$use_fake_data_1L_lgl){
    paths_ls$write_to_dir_nm_1L_chr <- "Real"
  }else{
    if(is.null(paths_ls$path_from_top_level_1L_chr)){
      path_elements_chr <- dirname(getwd()) %>% normalizePath() %>% strsplit("\\\\") %>% purrr::pluck(1)
      paths_ls$path_from_top_level_1L_chr <- path_elements_chr[length(path_elements_chr)-depth_1L_int]
      extended_path_elmts_chr <- getwd() %>% normalizePath() %>% strsplit("\\\\") %>% purrr::pluck(1)
      paths_ls$path_to_current_1L_chr <- paste0(extended_path_elmts_chr[(length(extended_path_elmts_chr)-depth_1L_int):length(extended_path_elmts_chr)],
                                                collapse = "/")
    }
    paths_ls$write_to_dir_nm_1L_chr <- "Fake"
    paths_ls$path_to_fake_data_1L_chr <- paste0(paths_ls$path_from_top_level_1L_chr,"/",paths_ls$write_to_dir_nm_1L_chr,"/fake_data.rds")
    paths_ls$path_to_data_from_top_level_chr <- c(paths_ls$path_from_top_level_1L_chr,paths_ls$write_to_dir_nm_1L_chr,"fake_data.rds")
  }
  return(paths_ls)
}
make_path_params_ls <- function(path_to_data_from_top_level_chr = NULL, # MIGRATE From specific / TTU
                                consent_1L_chr = "",
                                consent_indcs_int = 1L,
                                dv_ds_nm_and_url_chr = NULL,
                                options_chr = c("Y", "N"),
                                path_from_top_level_1L_chr = NULL,
                                path_to_current_1L_chr = NULL,
                                R_fl_nm_1L_chr = 'aaaaaaaaaa.txt',
                                use_fake_data_1L_lgl = F,
                                write_new_dir_1L_lgl = F){
  if(is.null(path_to_data_from_top_level_chr))
    path_to_data_from_top_level_chr <- ifelse(use_fake_data_1L_lgl,
                                              "fake_data.rds",
                                              "data.rds")
  if(is.null(path_from_top_level_1L_chr)){
    path_from_top_level_1L_chr <- normalizePath("../") %>% strsplit("\\\\") %>% purrr::pluck(1) %>% tail(1)
  }
  if(is.null(path_to_current_1L_chr)){
    path_to_current_1L_chr <- normalizePath(".") %>% strsplit("\\\\") %>% purrr::pluck(1) %>% tail(1)
  }
  path_params_ls <- list(path_from_top_level_1L_chr = path_from_top_level_1L_chr,
                         path_to_data_from_top_level_chr = path_to_data_from_top_level_chr,
                         path_to_current_1L_chr = path_to_current_1L_chr,
                         dv_ds_nm_and_url_chr = dv_ds_nm_and_url_chr)
  if(write_new_dir_1L_lgl)
    path_params_ls$paths_ls <- write_main_outp_dir(consent_1L_chr = consent_1L_chr,
                                                   consent_indcs_int = consent_indcs_int,
                                                   options_chr = options_chr,
                                                   path_params_ls,
                                                   use_fake_data_1L_lgl = use_fake_data_1L_lgl,
                                                   R_fl_nm_1L_chr = R_fl_nm_1L_chr)

  return(path_params_ls)
}
make_rmd_fl_nms_ls <- function(html_fl_nm_1L_chr = "Main_HTML",
                               pdf_fl_nm_1L_chr = "Main_Bookdown_PDF",
                               word_fl_nm_1L_chr = "Main_Word"){
  rmd_fl_nms_ls <- list(HTML = html_fl_nm_1L_chr,
                        PDF = pdf_fl_nm_1L_chr,
                        Word = word_fl_nm_1L_chr)
  return(rmd_fl_nms_ls)
}
make_rprt_type_ls <- function(rprt_nm_1L_chr,
                              rprt_lup){
  values_chr <- names(rprt_lup)[names(rprt_lup) != "rprt_nms_chr"] %>%
    purrr::map_chr(~ready4::get_from_lup_obj(rprt_lup,
                                             match_value_xx = rprt_nm_1L_chr,
                                             match_var_nm_1L_chr = "rprt_nms_chr",
                                             target_var_nm_1L_chr = .x,
                                             evaluate_1L_lgl = F))
  rprt_type_ls <- list(path_to_RMD_dir_1L_chr = ifelse(!is.na(values_chr[2]),
                                                       values_chr[2],
                                                       system.file(values_chr[3],
                                                                   package = values_chr[4])),
                       nm_of_RMD_1L_chr = values_chr[5],
                       rltv_path_to_outp_yaml_1L_chr = values_chr[6],
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
  email_1L_chr <- ifelse(is.na(sngl_author_tb$email_chr),
                         NA_character_,
                         ifelse(sngl_author_tb$email_chr =="",
                         NA_character_,
                         paste0(indent_1L_chr,
                                "email: ",
                                sngl_author_tb$email_chr)))
  institute_1L_chr <- paste0(indent_1L_chr,"institute: [",
                             sngl_author_tb$institute_chr %>% stringr::str_replace_all("\"",""),
                             "]")
  correspondence_1L_chr <- ifelse(is.na(sngl_author_tb$is_corresponding_lgl),
                                  NA_character_,
                                  ifelse(sngl_author_tb$is_corresponding_lgl,
                                  paste0(indent_1L_chr,
                                         "correspondence: true"),
                                  NA_character_))
  equal_1L_chr <- ifelse(is.na(sngl_author_tb$is_equal_first_lgl),
                         NA_character_,
                         ifelse(sngl_author_tb$is_equal_first_lgl,
                         paste0(indent_1L_chr,
                                "equal_contributor: \"yes\""),
                         NA_character_))
  author_lines_chr <- c(author_1L_chr, email_1L_chr, institute_1L_chr, correspondence_1L_chr, equal_1L_chr) %>%
    purrr::discard(is.na)
  return(author_lines_chr)
}
make_table_fn <- function(type_1L_chr = c("HTML","PDF", "Word"),
                          what_1L_chr = c("df", "gtsummary", "null")){
  type_1L_chr <- match.arg(type_1L_chr)
  what_1L_chr <- match.arg(what_1L_chr)
  if(what_1L_chr=="null"){
    table_fn = NULL
  }else{
    if(type_1L_chr == "HTML"){
      if(what_1L_chr=="df"){
        table_fn <- function(x) {x %>% gt::gt() %>% gt::tab_caption(caption = knitr::opts_current$get("tab.cap"))} #gt::gt
      }else{
        table_fn <- identity
      }

    }
    if(type_1L_chr == "PDF"){
      if(what_1L_chr=="df"){
        table_fn <- function(x) {kableExtra::kbl(x,
                                                 booktabs = T,
                                                 caption = knitr::opts_current$get("tab.cap"))}
      }else{
        table_fn <- function(x) {gtsummary::as_kable_extra(x, booktabs = T)}
      }
    }
    if(type_1L_chr == "Word"){
      if(what_1L_chr=="df"){
        table_fn <- function(x) {  gtsummary::as_flex_table(gtsummary::as_gtsummary(x)) %>% flextable::set_caption(caption = knitr::opts_current$get("tab.cap"))} #x %>% flextable::as_flextable()
      }else{
        table_fn <- gtsummary::as_flex_table
      }
    }
  }

  return(table_fn)
}
make_table_fns_ls <- function(what_1L_chr = c("df", "gtsummary", "null", "identity"),
                              html_table_fn = NULL,
                              pdf_table_fn = NULL,
                              word_table_fn = NULL){
  what_1L_chr <- match.arg(what_1L_chr)
  if(what_1L_chr=="identity"){
    if(is.null(html_table_fn)){
      html_table_fn <- identity
    }
    if(is.null(pdf_table_fn)){
      pdf_table_fn <- identity
    }
    if(is.null(word_table_fn)){
      word_table_fn <- identity
    }
  }
  if(F){
    html_table_fn <- function(x) x %>% gt::gt() %>% gt::tab_caption(caption = knitr::opts_current$get("tab.cap"))
    pdf_table_fn <- function(x) x %>% kableExtra::kbl(booktabs = T, caption = knitr::opts_current$get("tab.cap"))
    word_table_fn <- function(x) x %>% flextable::as_flextable() %>% flextable::set_caption(caption = knitr::opts_current$get("tab.cap"))
  }
  if(is.null(html_table_fn)){
    html_table_fn <- make_table_fn("HTML", what_1L_chr = what_1L_chr)
  }
  if(is.null(pdf_table_fn)){
    pdf_table_fn <- make_table_fn("PDF", what_1L_chr = what_1L_chr)
  }
  if(is.null(word_table_fn)){
    word_table_fn <- make_table_fn("Word", what_1L_chr = what_1L_chr)
  }
  table_fns_ls <- list(HTML = html_table_fn,
                       PDF = pdf_table_fn,
                       Word = word_table_fn)
  return(table_fns_ls)
}
