add_footnote_in_fmt <- function(table_xx, footnotes_chr, output_type_1L_chr = c("PDF", "HTML", "Word"),
                                footnote_fns_ls = make_table_fns_ls("null")){
  output_type_1L_chr <- match.arg(output_type_1L_chr)
  footnote_fn <- footnote_fns_ls %>% purrr::pluck(output_type_1L_chr)
  if(is.null(footnote_fn)){
    if(inherits(table_xx, "flextable")){
      footnote_fn <- function(table_xx, footnotes_chr) table_xx %>% flextable::add_footer_lines(values = footnotes_chr)
    }
    if(inherits(table_xx, "gt_tbl")){
      footnote_fn <- function(table_xx, footnotes_chr) table_xx %>% gt::tab_footnote(footnote = footnotes_chr)
    }
    if(inherits(table_xx, "gtsummary")){
      footnote_fn <- function(table_xx, footnotes_chr) table_xx %>% gtsummary::modify_table_styling(footnote = footnotes_chr)
    }
    if(inherits(table_xx, "knitr_kable")){
      footnote_fn <- function(table_xx, footnotes_chr) table_xx %>% kableExtra::add_footnote(label = footnotes_chr)
    }
  }
  table_xx <- table_xx %>% footnote_fn(footnotes_chr = footnotes_chr)
  return(table_xx)
}
add_tfmn_for_fmt <- function(data_xx,
                             output_type_1L_chr = c("PDF", "HTML", "Word"),
                             tfmns_fn_ls = make_table_fns_ls("identity")){
  output_type_1L_chr <- match.arg(output_type_1L_chr)
  tfmn_fn <- tfmns_fn_ls %>% purrr::pluck(output_type_1L_chr)
  data_xx <- data_xx %>% tfmn_fn()
  return(data_xx)
}
