#' 
#' Author and document datasets
#' @name authorData-Ready4showSynopsis
#' @description authorData method applied to Ready4showSynopsis
#' @param x An object of class Ready4showSynopsis
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param tmpl_url_1L_chr Template url (a character vector of length one), Default: 'https://github.com/ready4-dev/ms_tmpl'
#' @param tmpl_version_1L_chr Template version (a character vector of length one), Default: '0.1.1.0'
#' @param what_1L_chr What (a character vector of length one), Default: 'Manuscript'
#' @param ... Additional arguments
#' @return No return value, called for side effects.
#' @rdname authorData-methods
#' @aliases authorData,Ready4showSynopsis-method
#' @export 
#' @importFrom purrr pluck flatten_chr
#' @importFrom ready4 write_new_files authorData
methods::setMethod("authorData", "Ready4showSynopsis", function (x, consent_1L_chr = "", consent_indcs_int = 1L, options_chr = c("Y", 
    "N"), tmpl_url_1L_chr = "https://github.com/ready4-dev/ms_tmpl", 
    tmpl_version_1L_chr = "0.1.1.0", what_1L_chr = "Manuscript", 
    ...) 
{
    if (!is.na(x@a_Ready4showPaths@mkdn_source_dir_1L_chr)) {
        mkdn_source_dir_1L_chr <- x@a_Ready4showPaths@mkdn_source_dir_1L_chr
    }
    else {
        mkdn_repos_chr <- strsplit(tmpl_url_1L_chr, "/") %>% 
            purrr::pluck(1) %>% tail(2)
        temp_dir_1L_chr <- tempdir()
        write_mkdn_from_repo(consent_1L_chr = consent_1L_chr, 
            consent_indcs_int = consent_indcs_int, mkdn_data_dir_1L_chr = temp_dir_1L_chr, 
            ms_mkdn_parent_1L_chr = mkdn_repos_chr[1], ms_mkdn_repo_1L_chr = mkdn_repos_chr[2], 
            options_chr = options_chr, version_1L_chr = tmpl_version_1L_chr)
        mkdn_source_dir_1L_chr <- paste0(temp_dir_1L_chr, "/", 
            tmpl_url_1L_chr %>% basename(), "-", tmpl_version_1L_chr)
    }
    if (what_1L_chr != "Manuscript") {
        ms_mkdn_dir_1L_chr <- x@a_Ready4showPaths@ms_mkdn_dir_1L_chr
        ms_dir_1L_chr <- x@a_Ready4showPaths@ms_dir_1L_chr
        x@a_Ready4showPaths@ms_mkdn_dir_1L_chr <- x@a_Ready4showPaths@ms_dir_1L_chr <- what_1L_chr
    }
    paths_ls <- manufacture(x, what_1L_chr = "paths_ls")
    write_new_dirs(paths_ls %>% purrr::flatten_chr(), consent_1L_chr = consent_1L_chr, 
        consent_indcs_int = consent_indcs_int, options_chr = options_chr)
    ready4::write_new_files(paths_ls$path_to_ms_mkdn_dir_1L_chr, 
        source_paths_ls = list(mkdn_source_dir_1L_chr), consent_1L_chr = consent_1L_chr, 
        consent_indcs_int = consent_indcs_int, options_chr = options_chr)
    if (exists("temp_dir_1L_chr")) {
        unlink(temp_dir_1L_chr)
    }
})
