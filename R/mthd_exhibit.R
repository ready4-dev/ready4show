#' Exhibit features of model module data by printing them to the R console
#' @description exhibit.ready4show_authors() is an exhibit method that exhibits features of a class instance by printing to console. This method is implemented for the ready4 submodule class for authors lookup table The function is called for its side effects and does not return a value.
#' @param x An instance of `ready4show_authors`, a ready4 submodule class for authors lookup table
#' @param caption_1L_chr Caption (a character vector of length one), Default: NULL
#' @param mkdn_tbl_ref_1L_chr Markdown table reference (a character vector of length one), Default: NULL
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param use_lbls_as_col_nms_1L_lgl Use labels as column names (a logical vector of length one), Default: T
#' @param ... Additional arguments
#' @return No return value, called for side effects.
#' @rdname exhibit-methods
#' @export 
#' @importFrom ready4 exhibit
exhibit.ready4show_authors <- function (x, caption_1L_chr = NULL, mkdn_tbl_ref_1L_chr = NULL, 
    output_type_1L_chr = "HTML", use_lbls_as_col_nms_1L_lgl = T, 
    ...) 
{
    var_desc_chr = c("First-name", "Middle-name", "Last-name", 
        "Title", "Qualifications", "Institutes", "Sequence Position", 
        "Corresponding", "Email", "Joint-first")
    x %>% print_from_chunk(caption_1L_chr = caption_1L_chr, mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr, 
        output_type_1L_chr = output_type_1L_chr, use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl, 
        var_desc_chr = var_desc_chr, ...)
}
#' @rdname exhibit-methods
#' @aliases exhibit,ready4show_authors-method
#' @importFrom ready4 exhibit
methods::setMethod("exhibit", methods::className("ready4show_authors", package = "ready4show"), exhibit.ready4show_authors)
#' Exhibit features of model module data by printing them to the R console
#' @description exhibit.ready4show_correspondences() is an exhibit method that exhibits features of a class instance by printing to console. This method is implemented for the Name correspondences lookup table The function is called for its side effects and does not return a value.
#' @param x An instance of `ready4show_correspondences`, a Name correspondences lookup table
#' @param caption_1L_chr Caption (a character vector of length one), Default: NULL
#' @param mkdn_tbl_ref_1L_chr Markdown table reference (a character vector of length one), Default: NULL
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param use_lbls_as_col_nms_1L_lgl Use labels as column names (a logical vector of length one), Default: T
#' @param ... Additional arguments
#' @return No return value, called for side effects.
#' @rdname exhibit-methods
#' @export 
#' @importFrom ready4 exhibit
exhibit.ready4show_correspondences <- function (x, caption_1L_chr = NULL, mkdn_tbl_ref_1L_chr = NULL, 
    output_type_1L_chr = "HTML", use_lbls_as_col_nms_1L_lgl = T, 
    ...) 
{
    var_desc_chr = c("Old name", "New name")
    x %>% print_from_chunk(caption_1L_chr = caption_1L_chr, mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr, 
        output_type_1L_chr = output_type_1L_chr, use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl, 
        var_desc_chr = var_desc_chr, ...)
}
#' @rdname exhibit-methods
#' @aliases exhibit,ready4show_correspondences-method
#' @importFrom ready4 exhibit
methods::setMethod("exhibit", methods::className("ready4show_correspondences", package = "ready4show"), exhibit.ready4show_correspondences)
#' Exhibit features of model module data by printing them to the R console
#' @description exhibit.ready4show_institutes() is an exhibit method that exhibits features of a class instance by printing to console. This method is implemented for the ready4 submodule class for institutes lookup table The function is called for its side effects and does not return a value.
#' @param x An instance of `ready4show_institutes`, a ready4 submodule class for institutes lookup table
#' @param caption_1L_chr Caption (a character vector of length one), Default: NULL
#' @param mkdn_tbl_ref_1L_chr Markdown table reference (a character vector of length one), Default: NULL
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param use_lbls_as_col_nms_1L_lgl Use labels as column names (a logical vector of length one), Default: T
#' @param ... Additional arguments
#' @return No return value, called for side effects.
#' @rdname exhibit-methods
#' @export 
#' @importFrom ready4 exhibit
exhibit.ready4show_institutes <- function (x, caption_1L_chr = NULL, mkdn_tbl_ref_1L_chr = NULL, 
    output_type_1L_chr = "HTML", use_lbls_as_col_nms_1L_lgl = T, 
    ...) 
{
    var_desc_chr = c("Reference", "Name")
    x %>% print_from_chunk(caption_1L_chr = caption_1L_chr, mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr, 
        output_type_1L_chr = output_type_1L_chr, use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl, 
        var_desc_chr = var_desc_chr, ...)
}
#' @rdname exhibit-methods
#' @aliases exhibit,ready4show_institutes-method
#' @importFrom ready4 exhibit
methods::setMethod("exhibit", methods::className("ready4show_institutes", package = "ready4show"), exhibit.ready4show_institutes)
