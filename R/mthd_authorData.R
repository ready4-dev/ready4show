#' 
#' Author and document datasets
#' @name authorData-Ready4showSynopsis
#' @description authorData method applied to Ready4showSynopsis
#' @param x An object of class Ready4showSynopsis
#' @param tmpl_url_1L_chr Template url (a character vector of length one), Default: 'https://github.com/ready4-dev/ms_tmpl'
#' @param tmpl_version_1_L_chr Template version 1 L (a character vector), Default: '0.0.9.2'
#' @return NULL
#' @rdname authorData-methods
#' @aliases authorData,Ready4showSynopsis-method
#' @export 
#' @importFrom utils unzip
#' @importFrom purrr flatten_chr
#' @importFrom ready4 write_new_files authorData
methods::setMethod("authorData", "Ready4showSynopsis", function (x, tmpl_url_1L_chr = "https://github.com/ready4-dev/ms_tmpl", 
    tmpl_version_1_L_chr = "0.0.9.2") 
{
    if (!is.na(x@a_Ready4showPaths@mkdn_source_dir_1L_chr)) {
        mkdn_source_dir_1L_chr <- x@a_Ready4showPaths@mkdn_source_dir_1L_chr
    }
    else {
        temp_fl_1L_chr <- tempfile()
        temp_dir_1L_chr <- tempdir()
        download.file(paste0(tmpl_url_1L_chr, "/archive/refs/tags/v", 
            tmpl_version_1_L_chr, ".zip"), temp_fl_1L_chr)
        utils::unzip(temp_fl_1L_chr, exdir = temp_dir_1L_chr)
        unlink(temp_fl_1L_chr)
        mkdn_source_dir_1L_chr <- paste0(temp_dir_1L_chr, "/", 
            tmpl_url_1L_chr %>% basename(), "-", tmpl_version_1_L_chr)
    }
    paths_ls <- manufacture(x, what_1L_chr = "paths_ls")
    write_new_dirs(paths_ls %>% purrr::flatten_chr())
    ready4::write_new_files(paths_ls$path_to_ms_mkdn_dir_1L_chr, 
        source_paths_ls = list(mkdn_source_dir_1L_chr))
    if (exists("temp_dir_1L_chr")) {
        unlink(temp_dir_1L_chr)
    }
})
