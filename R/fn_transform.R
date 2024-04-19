#' Transform abstract
#' @description transform_abstract() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform abstract. The function returns Transformed abstract (a character vector).
#' @param path_to_abstract_1L_chr Path to abstract (a character vector of length one)
#' @return Transformed abstract (a character vector)
#' @rdname transform_abstract
#' @export 
#' @importFrom stringr str_replace_all
#' @keywords internal
transform_abstract <- function (path_to_abstract_1L_chr) 
{
    tfd_abstract_chr <- paste0(stringr::str_replace_all(stringr::str_replace_all(stringr::str_replace_all(stringr::str_replace_all(readLines(path_to_abstract_1L_chr), 
        "\\\\\\\\textbf", ""), "\\{", "**"), "\\}", "**"), "\\\\\\\\newline", 
        "\\\\n"), collapse = " ")
    return(tfd_abstract_chr)
}
