#' Ready4showSynopsis
#' 
#' Metadata about a scientific summary manuscript
#' 
#' @include fn_make.R
#' @slot a_Ready4showPaths  (an instance of the Ready4showPaths class)
#' @slot abstract_args_ls Abstract arguments (a list)
#' @slot authors_r3 Authors (a ready4 submodule)
#' @slot background_1L_chr Background (a character vector of length one)
#' @slot coi_1L_chr Conflict of interest (a character vector of length one)
#' @slot conclusion_1L_chr Conclusion (a character vector of length one)
#' @slot correspondences_r3 Correspondences (a ready4 submodule)
#' @slot digits_int Digits (an integer vector)
#' @slot ethics_1L_chr Ethics (a character vector of length one)
#' @slot fl_nm_1L_chr File name (a character vector of length one)
#' @slot figures_in_body_lgl Figures in body (a logical vector)
#' @slot funding_1L_chr Funding (a character vector of length one)
#' @slot institutes_r3 Institutes (a ready4 submodule)
#' @slot interval_chr Interval (a character vector)
#' @slot keywords_chr Keywords (a character vector)
#' @slot outp_formats_chr Output formats (a character vector)
#' @slot rmd_fl_nms_ls R Markdown file names (a list)
#' @slot sample_desc_1L_chr Sample description (a character vector of length one)
#' @slot tables_in_body_lgl Tables in body (a logical vector)
#' @slot title_1L_chr Title (a character vector of length one)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name Ready4showSynopsis-class
#' @rdname Ready4showSynopsis-class
#' @export Ready4showSynopsis
#' @exportClass Ready4showSynopsis
Ready4showSynopsis <- methods::setClass("Ready4showSynopsis",
contains = "Ready4Module",
slots = c(a_Ready4showPaths = "Ready4showPaths",abstract_args_ls = "list",authors_r3 = "ready4show_authors",background_1L_chr = "character",coi_1L_chr = "character",conclusion_1L_chr = "character",correspondences_r3 = "ready4show_correspondences",digits_int = "integer",ethics_1L_chr = "character",fl_nm_1L_chr = "character",figures_in_body_lgl = "logical",funding_1L_chr = "character",institutes_r3 = "ready4show_institutes",interval_chr = "character",keywords_chr = "character",outp_formats_chr = "character",rmd_fl_nms_ls = "list",sample_desc_1L_chr = "character",tables_in_body_lgl = "logical",title_1L_chr = "character",dissemination_1L_chr = "character"),
prototype =  list(a_Ready4showPaths = make_default_paths(),abstract_args_ls = make_abstract_args_ls(),authors_r3 = ready4show_authors(),background_1L_chr = NA_character_,coi_1L_chr = 'None declared.',conclusion_1L_chr = NA_character_,correspondences_r3 = ready4show_correspondences(),digits_int = 3L,ethics_1L_chr = 'Details on ethics approvals go here.',fl_nm_1L_chr = 'Manuscript',figures_in_body_lgl = T,funding_1L_chr = 'Details on study funders go here.',institutes_r3 = ready4show_institutes(),interval_chr = NA_character_,keywords_chr = NA_character_,outp_formats_chr = 'PDF',rmd_fl_nms_ls = make_rmd_fl_nms_ls(),sample_desc_1L_chr = NA_character_,tables_in_body_lgl = T,title_1L_chr = 'Manuscript title goes here.'))


methods::setValidity(methods::className("Ready4showSynopsis"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
