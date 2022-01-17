#' 
#' Author and save files
#' @name author-Ready4showSynopsis
#' @description author method applied to Ready4showSynopsis
#' @param x An object of class Ready4showSynopsis
#' @param results_ls Results (a list), Default: NULL
#' @return NULL
#' @rdname author-methods
#' @aliases author,Ready4showSynopsis-method
#' @export 
#' @importFrom rmarkdown render
#' @importFrom ready4 author
methods::setMethod("author", "Ready4showSynopsis", function (x, results_ls = NULL) 
{
    if (is.na(x@fl_nm_1L_chr)) {
        fl_nm_1L_chr <- "Manuscript"
    }
    else {
        fl_nm_1L_chr <- x@fl_nm_1L_chr
    }
    mkdn_data_dir_1L_chr <- x@a_Ready4showPaths@mkdn_data_dir_1L_chr
    outp_dir_1L_chr <- x@a_Ready4showPaths@outp_data_dir_1L_chr
    output_type_1L_chr <- x@outp_formats_chr[1]
    path_to_ms_mkdn_dir_1L_chr <- paste0(mkdn_data_dir_1L_chr, 
        "/", x@a_Ready4showPaths@ms_mkdn_dir_1L_chr)
    path_to_ms_outp_dir_1L_chr <- paste0(outp_dir_1L_chr, "/", 
        x@a_Ready4showPaths@ms__dir_1L_chr)
    write_new_dirs(c(mkdn_data_dir_1L_chr, outp_dir_1L_chr, path_to_ms_mkdn_dir_1L_chr, 
        path_to_ms_outp_dir_1L_chr))
    header_yaml_args_ls <- make_header_yaml_args_ls(x@authors_r3, 
        institutes_tb = x@institutes_r3, keywords_chr = x@keywords_chr, 
        title_1L_chr = x@title_1L_chr)
    if (identical(x@abstract_args_ls, list())) {
        x@abstract_args_ls <- NULL
    }
    write_header_fls(path_to_header_dir_1L_chr = paste0(path_to_ms_mkdn_dir_1L_chr, 
        "/Header"), header_yaml_args_ls = header_yaml_args_ls, 
        abstract_args_ls = x@abstract_args_ls)
    params_ls <- list(output_type_1L_chr = output_type_1L_chr, 
        results_ls = results_ls)
    params_ls$figures_in_body_lgl <- x@figures_in_body_lgl
    params_ls$tables_in_body_lgl <- x@tables_in_body_lgl
    rmarkdown::render(paste0(path_to_ms_mkdn_1L_dir, "/Parent_", 
        output_type_1L_chr, "/Parent_", output_type_1L_chr, ".Rmd"), 
        output_format = NULL, params = params_ls, output_file = paste0(fl_nm_1L_chr, 
            ifelse(output_type_1L_chr == "Word", ".docx", paste0(".", 
                tolower(output_type_1L_chr)))), output_dir = path_to_ms_outp_dir_1L_chr)
})
