#' Ready4showSynopsis
#' 
#' Metadata about a scientific summary manuscript
#' 
#' @slot authors_r3 Authors (a ready4 S3)
#' @slot background_1L_chr Background (a character vector of length one)
#' @slot coi_1L_chr Conflict of interest (a character vector of length one)
#' @slot conclusion_1L_chr Conclusion (a character vector of length one)
#' @slot correspondences_r3 Correspondences (a ready4 S3)
#' @slot digits_int Digits (an integer vector)
#' @slot ethics_1L_chr Ethics (a character vector of length one)
#' @slot funding_1L_chr Funding (a character vector of length one)
#' @slot institutes_r3 Institutes (a ready4 S3)
#' @slot interval_chr Interval (a character vector)
#' @slot keywords_chr Keywords (a character vector)
#' @slot outp_formats_chr Output formats (a character vector)
#' @slot sample_desc_1L_chr Sample description (a character vector of length one)
#' @slot title_1L_chr Title (a character vector of length one)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name Ready4showSynopsis-class
#' @rdname Ready4showSynopsis-class
#' @export Ready4showSynopsis
#' @exportClass Ready4showSynopsis
Ready4showSynopsis <- methods::setClass("Ready4showSynopsis",
contains = "Ready4Module",
slots = c(authors_r3 = "ready4show_authors",background_1L_chr = "character",coi_1L_chr = "character",conclusion_1L_chr = "character",correspondences_r3 = "ready4show_correspondences",digits_int = "integer",ethics_1L_chr = "character",funding_1L_chr = "character",institutes_r3 = "ready4show_institutes",interval_chr = "character",keywords_chr = "character",outp_formats_chr = "character",sample_desc_1L_chr = "character",title_1L_chr = "character",dissemination_1L_chr = "character"),
prototype =  list(authors_r3 = ready4show_authors(),background_1L_chr = NA_character_,coi_1L_chr = NA_character_,conclusion_1L_chr = NA_character_,correspondences_r3 = ready4show_correspondences(),digits_int = NA_integer_,ethics_1L_chr = NA_character_,funding_1L_chr = NA_character_,institutes_r3 = ready4show_institutes(),interval_chr = NA_character_,keywords_chr = NA_character_,outp_formats_chr = NA_character_,sample_desc_1L_chr = NA_character_,title_1L_chr = NA_character_))


methods::setValidity(methods::className("Ready4showSynopsis"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
