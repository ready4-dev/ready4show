#' Add footnote in format
#' @description add_footnote_in_fmt() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add footnote in format. The function returns Table (an output object of multiple potential types).
#' @param table_xx Table (an output object of multiple potential types)
#' @param footnotes_chr Footnotes (a character vector)
#' @param output_type_1L_chr Output type (a character vector of length one), Default: c("PDF", "HTML", "Word")
#' @param footnote_fns_ls Footnote functions (a list), Default: make_table_fns_ls("null")
#' @return Table (an output object of multiple potential types)
#' @rdname add_footnote_in_fmt
#' @export 
#' @importFrom purrr pluck
#' @importFrom flextable add_footer_lines
#' @importFrom gt tab_footnote
#' @importFrom gtsummary modify_table_styling
#' @importFrom kableExtra add_footnote
#' @keywords internal
add_footnote_in_fmt <- function (table_xx, footnotes_chr, output_type_1L_chr = c("PDF", 
    "HTML", "Word"), footnote_fns_ls = make_table_fns_ls("null")) 
{
    output_type_1L_chr <- match.arg(output_type_1L_chr)
    footnote_fn <- footnote_fns_ls %>% purrr::pluck(output_type_1L_chr)
    if (is.null(footnote_fn)) {
        if (inherits(table_xx, "flextable")) {
            footnote_fn <- function(table_xx, footnotes_chr) table_xx %>% 
                flextable::add_footer_lines(values = footnotes_chr)
        }
        if (inherits(table_xx, "gt_tbl")) {
            footnote_fn <- function(table_xx, footnotes_chr) table_xx %>% 
                gt::tab_footnote(footnote = footnotes_chr)
        }
        if (inherits(table_xx, "gtsummary")) {
            footnote_fn <- function(table_xx, footnotes_chr) table_xx %>% 
                gtsummary::modify_table_styling(footnote = footnotes_chr)
        }
        if (inherits(table_xx, "knitr_kable")) {
            footnote_fn <- function(table_xx, footnotes_chr) table_xx %>% 
                kableExtra::add_footnote(label = footnotes_chr)
        }
    }
    table_xx <- table_xx %>% footnote_fn(footnotes_chr = footnotes_chr)
    return(table_xx)
}
#' Add transformation for format
#' @description add_tfmn_for_fmt() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add transformation for format. The function returns Data (an output object of multiple potential types).
#' @param data_xx Data (an output object of multiple potential types)
#' @param output_type_1L_chr Output type (a character vector of length one), Default: c("PDF", "HTML", "Word")
#' @param tfmns_fn_ls Transformations (a list of functions), Default: make_table_fns_ls("identity")
#' @return Data (an output object of multiple potential types)
#' @rdname add_tfmn_for_fmt
#' @export 
#' @importFrom purrr pluck
#' @keywords internal
add_tfmn_for_fmt <- function (data_xx, output_type_1L_chr = c("PDF", "HTML", "Word"), 
    tfmns_fn_ls = make_table_fns_ls("identity")) 
{
    output_type_1L_chr <- match.arg(output_type_1L_chr)
    tfmn_fn <- tfmns_fn_ls %>% purrr::pluck(output_type_1L_chr)
    data_xx <- data_xx %>% tfmn_fn()
    return(data_xx)
}
