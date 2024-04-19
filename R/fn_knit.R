#' Knit from template
#' @description knit_from_tmpl() is a Knit function that knits an RMD or Rmarkdown file. Specifically, this function implements an algorithm to knit from template. The function is called for its side effects and does not return a value.
#' @param params_to_expand_ls Parameters to expand (a list)
#' @param path_to_tmpl_1L_chr Path to template (a character vector of length one)
#' @return No return value, called for side effects.
#' @rdname knit_from_tmpl
#' @export 
#' @importFrom purrr map
#' @importFrom rlang exec
#' @importFrom knitr knit_expand knit_child
#' @keywords internal
knit_from_tmpl <- function (params_to_expand_ls, path_to_tmpl_1L_chr) 
{
    src <- purrr::map(params_to_expand_ls, ~{
        args_ls <- append(list(file = path_to_tmpl_1L_chr), .x)
        rlang::exec(knitr::knit_expand, !!!args_ls)
    })
    res <- knitr::knit_child(text = unlist(src), quiet = TRUE)
    cat(res, sep = "\n")
}
