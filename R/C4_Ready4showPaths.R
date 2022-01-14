#' Ready4showPaths
#' 
#' Metadata about paths to Markdown input and output
#' 
#' @slot mkdn_data_dir_1L_chr Markdown data directory (a character vector of length one)
#' @slot mkdn_source_dir_1L_chr Markdown source directory (a character vector of length one)
#' @slot ms_mkdn_dir_1L_chr Manuscript markdown directory (a character vector of length one)
#' @slot ms_1L_dir_1L_chr Manuscript length one directory (a character vector of length one)
#' @slot outp_data_dir_1L_chr Output data directory (a character vector of length one)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name Ready4showPaths-class
#' @rdname Ready4showPaths-class
#' @export Ready4showPaths
#' @exportClass Ready4showPaths
Ready4showPaths <- methods::setClass("Ready4showPaths",
contains = "Ready4Module",
slots = c(mkdn_data_dir_1L_chr = "character",mkdn_source_dir_1L_chr = "character",ms_mkdn_dir_1L_chr = "character",ms_1L_dir_1L_chr = "character",outp_data_dir_1L_chr = "character",dissemination_1L_chr = "character"),
prototype =  list(mkdn_data_dir_1L_chr = NA_character_,mkdn_source_dir_1L_chr = NA_character_,ms_mkdn_dir_1L_chr = NA_character_,ms_1L_dir_1L_chr = NA_character_,outp_data_dir_1L_chr = NA_character_))


methods::setValidity(methods::className("Ready4showPaths"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
