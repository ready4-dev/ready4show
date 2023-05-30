#' Manufacture a new object
#' @description manufacture.ready4show_correspondences() is a manufacture method that manufactures a novel R object using data contained in an instance of a class. This method is implemented for the Name correspondences lookup table The function returns Object (an output object of multiple potential types).
#' @param x An instance of Name correspondences lookup table
#' @param data_ls Data (a list)
#' @param flatten_1L_chr Flatten (a character vector of length one), Default: F
#' @param type_1L_chr Type (a character vector of length one), Default: 'new'
#' @param what_1L_chr What (a character vector of length one), Default: 'names'
#' @param ... Additional arguments
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @export 
#' @importFrom purrr map map_chr flatten_chr
#' @importFrom ready4 get_from_lup_obj manufacture
manufacture.ready4show_correspondences <- function (x, data_ls, flatten_1L_chr = F, type_1L_chr = "new", 
    what_1L_chr = "names", ...) 
{
    if (what_1L_chr == "names") {
        object_xx <- data_ls %>% purrr::map(~.x %>% purrr::map_chr(~ifelse(.x %in% 
            x$old_nms_chr, ready4::get_from_lup_obj(x, match_value_xx = .x, 
            match_var_nm_1L_chr = ifelse(type_1L_chr == "old", 
                "new_nms_chr", "old_nms_chr"), target_var_nm_1L_chr = ifelse(type_1L_chr == 
                "old", "old_nms_chr", "new_nms_chr")), .x)))
    }
    if (flatten_1L_lgl) {
        object_xx <- object_xx %>% purrr::flatten_chr()
    }
    return(object_xx)
}
#' @rdname manufacture-methods
#' @aliases manufacture,ready4show_correspondences-method
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", methods::className("ready4show_correspondences", package = "ready4show"), manufacture.ready4show_correspondences)
#' 
#' Manufacture a new object
#' @name manufacture-Ready4showSynopsis
#' @description manufacture method applied to Ready4showSynopsis
#' @param x An object of class Ready4showSynopsis
#' @param what_1L_chr What (a character vector of length one), Default: 'paths_ls'
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @aliases manufacture,Ready4showSynopsis-method
#' @export 
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", "Ready4showSynopsis", function (x, what_1L_chr = "paths_ls") 
{
    if (what_1L_chr == "paths_ls") {
        outp_dir_1L_chr <- x@a_Ready4showPaths@outp_data_dir_1L_chr
        reports_dir_1L_chr <- paste0(outp_dir_1L_chr, "/", x@a_Ready4showPaths@reports_dir_1L_chr)
        object_xx <- list(outp_dir_1L_chr = outp_dir_1L_chr, 
            mkdn_data_dir_1L_chr = paste0(outp_dir_1L_chr, "/", 
                x@a_Ready4showPaths@mkdn_data_dir_1L_chr), reports_dir_1L_chr = reports_dir_1L_chr, 
            path_to_ms_mkdn_dir_1L_chr = paste0(outp_dir_1L_chr, 
                "/", x@a_Ready4showPaths@mkdn_data_dir_1L_chr, 
                "/", x@a_Ready4showPaths@ms_mkdn_dir_1L_chr), 
            path_to_ms_outp_dir_1L_chr = paste0(reports_dir_1L_chr, 
                "/", x@a_Ready4showPaths@ms_dir_1L_chr))
    }
    return(object_xx)
})
