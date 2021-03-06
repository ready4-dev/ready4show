#' Print table
#' @description print_table() is a Print function that prints output to console Specifically, this function implements an algorithm to print table. The function is called for its side effects and does not return a value.
#' @param data_tb Data (a tibble)
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'PDF'
#' @param use_rdocx_1L_lgl Use rdocx (a logical vector of length one), Default: F
#' @param caption_1L_chr Caption (a character vector of length one), Default: 'NA'
#' @param footnotes_chr Footnotes (a character vector), Default: 'NA'
#' @param merge_row_idx_int Merge row index (an integer vector), Default: NA
#' @param digits_dbl Digits (a double vector), Default: NULL
#' @param big_mark_1L_chr Big mark (a character vector of length one), Default: ' '
#' @param use_lbls_as_col_nms_1L_lgl Use labels as column names (a logical vector of length one), Default: F
#' @param scroll_box_args_ls Scroll box arguments (a list), Default: NULL
#' @param mkdn_tbl_ref_1L_chr Markdown table reference (a character vector of length one)
#' @param hline_after_ls Horizonal line after (a list), Default: NULL
#' @param add_to_row_ls Add to row (a list), Default: NULL
#' @param sanitize_fn Sanitize (a function), Default: getOption("xtable.sanitize.text.function", NULL)
#' @return NULL
#' @rdname print_table
#' @export 
#' @importFrom Hmisc label
#' @importFrom dplyr rename_with
#' @importFrom xtable xtable
#' @importFrom kableExtra kbl kable_paper row_spec footnote scroll_box
#' @importFrom rlang exec
#' @importFrom flextable flextable set_caption merge_h_range bold colformat_num autofit add_footer_lines
#' @importFrom officer run_autonum
#' @importFrom purrr reduce
print_table <- function (data_tb, output_type_1L_chr = "PDF", use_rdocx_1L_lgl = F, 
    caption_1L_chr = NA_character_, footnotes_chr = NA_character_, 
    merge_row_idx_int = NA_integer_, digits_dbl = NULL, big_mark_1L_chr = " ", 
    use_lbls_as_col_nms_1L_lgl = F, scroll_box_args_ls = NULL, 
    mkdn_tbl_ref_1L_chr, hline_after_ls = NULL, add_to_row_ls = NULL, 
    sanitize_fn = getOption("xtable.sanitize.text.function", 
        NULL)) 
{
    if (use_lbls_as_col_nms_1L_lgl & !any(Hmisc::label(data_tb) == 
        "")) {
        data_tb <- data_tb %>% dplyr::rename_with(~Hmisc::label(data_tb)[names(Hmisc::label(data_tb)) == 
            .x])
    }
    if (output_type_1L_chr == "PDF") {
        data_x_tb <- data_tb %>% xtable::xtable(caption = caption_1L_chr, 
            label = mkdn_tbl_ref_1L_chr, digits = digits_dbl)
        data_x_tb %>% print(comment = F, floating = TRUE, hline.after = hline_after_ls, 
            caption.placement = "top", add.to.row = add_to_row_ls, 
            sanitize.text.function = sanitize_fn, format.args = list(big.mark = big_mark_1L_chr), 
            include.colnames = FALSE, include.rownames = FALSE)
    }
    else {
        if (output_type_1L_chr == "HTML") {
            data_kb <- data_tb %>% kableExtra::kbl(format = "html", 
                escape = F, caption = caption_1L_chr, align = c("l", 
                  rep("r", nrow(data_tb) - 1))) %>% kableExtra::kable_paper("hover", 
                full_width = F)
            if (!all(is.na(merge_row_idx_int))) {
                data_kb <- data_kb %>% kableExtra::row_spec(merge_row_idx_int, 
                  bold = T)
            }
            data_kb <- data_kb %>% kableExtra::footnote(general = ifelse(is.na(footnotes_chr[1]), 
                "", footnotes_chr), general_title = "", footnote_as_chunk = T, 
                threeparttable = T)
            if (!is.null(scroll_box_args_ls)) {
                data_kb <- data_kb %>% kableExtra::kable_paper()
                data_kb <- rlang::exec(kableExtra::scroll_box, 
                  data_kb, !!!scroll_box_args_ls)
            }
            data_xx <- data_kb
        }
        if (output_type_1L_chr == "Word") {
            j2_1L_dbl <- ncol(data_tb)
            data_fx <- flextable::flextable(data_tb)
            if (!use_rdocx_1L_lgl) {
                data_fx <- data_fx %>% flextable::set_caption(caption_1L_chr, 
                  autonum = officer::run_autonum())
            }
            if (!all(is.na(merge_row_idx_int))) {
                data_fx <- data_fx %>% flextable::merge_h_range(i = merge_row_idx_int, 
                  j1 = 1, j2 = j2_1L_dbl) %>% flextable::bold(i = merge_row_idx_int, 
                  j = 1)
            }
            if (!is.null(digits_dbl)) {
                numeric_lgl <- unlist(lapply(data_tb, is.numeric)) %>% 
                  unname()
                data_fx <- unique(digits_dbl) %>% purrr::reduce(.init = data_fx, 
                  ~.x %>% flextable::colformat_num(j = names(data_tb)[which(digits_dbl == 
                    .y & numeric_lgl)], digits = .y))
            }
            data_fx <- data_fx %>% flextable::autofit(add_w = 0, 
                add_h = 0)
            if (!all(is.na(footnotes_chr))) {
                data_fx <- data_fx %>% flextable::add_footer_lines(values = footnotes_chr)
            }
            data_xx <- data_fx
        }
        return(data_xx)
    }
}
