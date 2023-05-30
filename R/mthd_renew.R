#' Renew values in a dataset
#' @description renew.ready4show_authors() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the ready4 S3 class for authors lookup table The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for authors lookup table
#' @param first_nm_chr First name (a character vector), Default: 'NA'
#' @param middle_nm_chr Middle name (a character vector), Default: 'NA'
#' @param last_nm_chr Last name (a character vector), Default: 'NA'
#' @param title_chr Title (a character vector), Default: 'NA'
#' @param qualifications_chr Qualifications (a character vector), Default: 'NA'
#' @param institute_chr Institute (a character vector), Default: 'NA'
#' @param sequence_int Sequence (an integer vector), Default: NA
#' @param is_corresponding_lgl Is corresponding (a logical vector), Default: NA
#' @param email_chr Email (a character vector), Default: 'NA'
#' @param is_equal_first_lgl Is equal first (a logical vector), Default: NA
#' @param filter_cdn_1L_chr Filter condition (a character vector of length one), Default: 'NA'
#' @param slice_indcs_int Slice indices (an integer vector), Default: NA
#' @param ... Additional arguments
#' @return x (An object)
#' @rdname renew-methods
#' @export 
#' @importFrom rlang current_env
#' @importFrom ready4 update_tb_r3 renew
renew.ready4show_authors <- function (x, first_nm_chr = NA_character_, middle_nm_chr = NA_character_, 
    last_nm_chr = NA_character_, title_chr = NA_character_, qualifications_chr = NA_character_, 
    institute_chr = NA_character_, sequence_int = NA_integer_, 
    is_corresponding_lgl = NA, email_chr = NA_character_, is_equal_first_lgl = NA, 
    filter_cdn_1L_chr = NA_character_, slice_indcs_int = NA_integer_, 
    ...) 
{
    fn_env_ls <- as.list(rlang::current_env())[-1]
    x <- ready4::update_tb_r3(x, filter_cdn_1L_chr = filter_cdn_1L_chr, 
        fn = renew.ready4show_authors, fn_env_ls = fn_env_ls, 
        slice_indcs_int = slice_indcs_int)
    return(x)
}
#' @rdname renew-methods
#' @aliases renew,ready4show_authors-method
#' @importFrom ready4 renew
methods::setMethod("renew", methods::className("ready4show_authors", package = "ready4show"), renew.ready4show_authors)
#' Renew values in a dataset
#' @description renew.ready4show_correspondences() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the Name correspondences lookup table The function is called for its side effects and does not return a value.
#' @param x An instance of Name correspondences lookup table
#' @param old_nms_chr Old names (a character vector), Default: 'NA'
#' @param new_nms_chr New names (a character vector), Default: 'NA'
#' @param filter_cdn_1L_chr Filter condition (a character vector of length one), Default: 'NA'
#' @param slice_indcs_int Slice indices (an integer vector), Default: NA
#' @param ... Additional arguments
#' @return x (An object)
#' @rdname renew-methods
#' @export 
#' @importFrom rlang current_env
#' @importFrom ready4 update_tb_r3 renew
renew.ready4show_correspondences <- function (x, old_nms_chr = NA_character_, new_nms_chr = NA_character_, 
    filter_cdn_1L_chr = NA_character_, slice_indcs_int = NA_integer_, 
    ...) 
{
    fn_env_ls <- as.list(rlang::current_env())[-1]
    x <- ready4::update_tb_r3(x, filter_cdn_1L_chr = filter_cdn_1L_chr, 
        fn = renew.ready4show_correspondences, fn_env_ls = fn_env_ls, 
        slice_indcs_int = slice_indcs_int)
    return(x)
}
#' @rdname renew-methods
#' @aliases renew,ready4show_correspondences-method
#' @importFrom ready4 renew
methods::setMethod("renew", methods::className("ready4show_correspondences", package = "ready4show"), renew.ready4show_correspondences)
#' Renew values in a dataset
#' @description renew.ready4show_institutes() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the ready4 S3 class for institutes lookup table The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for institutes lookup table
#' @param short_name_chr Short name (a character vector), Default: 'NA'
#' @param long_name_chr Long name (a character vector), Default: 'NA'
#' @param filter_cdn_1L_chr Filter condition (a character vector of length one), Default: 'NA'
#' @param slice_indcs_int Slice indices (an integer vector), Default: NA
#' @param ... Additional arguments
#' @return x (An object)
#' @rdname renew-methods
#' @export 
#' @importFrom rlang current_env
#' @importFrom ready4 update_tb_r3 renew
renew.ready4show_institutes <- function (x, short_name_chr = NA_character_, long_name_chr = NA_character_, 
    filter_cdn_1L_chr = NA_character_, slice_indcs_int = NA_integer_, 
    ...) 
{
    fn_env_ls <- as.list(rlang::current_env())[-1]
    x <- ready4::update_tb_r3(x, filter_cdn_1L_chr = filter_cdn_1L_chr, 
        fn = renew.ready4show_institutes, fn_env_ls = fn_env_ls, 
        slice_indcs_int = slice_indcs_int)
    return(x)
}
#' @rdname renew-methods
#' @aliases renew,ready4show_institutes-method
#' @importFrom ready4 renew
methods::setMethod("renew", methods::className("ready4show_institutes", package = "ready4show"), renew.ready4show_institutes)
