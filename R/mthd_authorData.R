#' 
#' Author and document datasets
#' @name authorData-Ready4showSynopsis
#' @description authorData method applied to Ready4showSynopsis
#' @param x An object of class Ready4showSynopsis
#' @return NULL
#' @rdname authorData-methods
#' @aliases authorData,Ready4showSynopsis-method
#' @export 
#' @importFrom purrr flatten_chr
#' @importFrom ready4 write_new_files authorData
methods::setMethod("authorData", "Ready4showSynopsis", function (x) 
{
    paths_ls <- manufacture(x, what_1L_chr = "paths_ls")
    write_new_dirs(paths_ls %>% purrr::flatten_chr())
    ready4::write_new_files(paths_ls$path_to_ms_mkdn_dir_1L_chr, 
        source_paths_ls = list(x@a_Ready4showPaths@mkdn_source_dir_1L_chr))
})
