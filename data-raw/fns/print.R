print_from_chunk <- function(ds_tb,
                             caption_1L_chr = NULL,
                             mkdn_tbl_ref_1L_chr = NULL,
                             output_type_1L_chr = "HTML",
                             use_lbls_as_col_nms_1L_lgl = T,
                             var_desc_chr = NA_character_,
                             ...){
  if(is.null(caption_1L_chr)){
    caption_1L_chr <- knitr::opts_current$get("tab.cap")
  }
  if(is.null(mkdn_tbl_ref_1L_chr)){
    mkdn_tbl_ref_1L_chr <- paste0("tab:",knitr::opts_current$get("tab.id"))
  }
  if(use_lbls_as_col_nms_1L_lgl){
    if (!is.na(var_desc_chr[1]) & length(var_desc_chr) == ncol(ds_tb)) {
      ds_tb <- ds_tb %>% ready4::remove_lbls_from_df()
      ds_tb <- seq_len(length(var_desc_chr)) %>%
        purrr::reduce(.init = ds_tb, ~{
          Hmisc::label(.x[[names(ds_tb)[.y]]]) <- var_desc_chr[.y]
          .x
        })
    }
  }
  ds_tb %>%
    print_table(caption_1L_chr = caption_1L_chr,
                            mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr,
                            output_type_1L_chr = output_type_1L_chr,
                            use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl,
                            use_rdocx_1L_lgl = ifelse(output_type_1L_chr=="Word",T,F),
                            ...)
}
print_in_format <- function(table_xx,
                            caption_1L_chr = NULL,
                            mkdn_tbl_ref_1L_chr = NULL,
                            output_type_1L_chr = c("PDF", "HTML", "Word"),
                            table_fns_ls = make_table_fns_ls(),
                            use_lbls_as_col_nms_1L_lgl = T,
                            var_desc_chr = NA_character_,
                            ...){

  output_type_1L_chr <- match.arg(output_type_1L_chr)
  if (is.null(mkdn_tbl_ref_1L_chr)) {
    mkdn_tbl_ref_1L_chr <- paste0("tab:", knitr::opts_current$get("tab.id"))
  }
  table_fn <- NULL
  if(!is.null(table_fns_ls)){
    table_fn <- table_fns_ls %>% purrr::pluck(output_type_1L_chr)
  }
  if(!is.null(table_fn)){
    table_xx <- table_xx %>% table_fn()
  }else{
    table_xx <- table_xx %>% print_from_chunk(data_df,#ready4show::
                                              caption_1L_chr = caption_1L_chr, mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr,
                                              output_type_1L_chr = output_type_1L_chr, use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl,
                                              var_desc_chr = var_desc_chr, ...)
  }
  return(table_xx)


}
print_plot <- function(plot_plt,
                       name_1L_chr = "plot",
                       output_type_1L_chr = c("PDF", "HTML", "Word"),
                       scale_1L_dbl = 1,
                       write_to_1L_chr = tempdir()){
  output_type_1L_chr <- match.arg(output_type_1L_chr)
  if(output_type_1L_chr != "Word"){
    plot_plt
  }else{
    plot_plt %>%
      ggplot2::ggsave(filename = paste0(write_to_1L_chr,"/", name_1L_chr, ".png"), scale = scale_1L_dbl)

    knitr::include_graphics(paste0(write_to_1L_chr,"/", name_1L_chr, ".png"))
  }
}
print_table <- function(data_tb,
                        mkdn_tbl_ref_1L_chr,
                        add_to_row_ls = NULL,
                        big_mark_1L_chr = " ",
                        caption_1L_chr = NA_character_,
                        digits_dbl = NULL,
                        footnotes_chr = NA_character_,
                        heading_grps_chr = NULL,
                        hline_after_ls = NULL,
                        inc_col_nms_1L_lgl = FALSE,
                        inc_row_nms_1L_lgl = FALSE,
                        merge_row_idx_int = NA_integer_,
                        output_type_1L_chr = "PDF",
                        sanitize_fn = getOption("xtable.sanitize.text.function", NULL),
                        scroll_box_args_ls = NULL,
                        use_lbls_as_col_nms_1L_lgl = F,
                        use_rdocx_1L_lgl = F){
  if(use_lbls_as_col_nms_1L_lgl & !any(Hmisc::label(data_tb)=="")){ #Add assert all cols have (unique) labels
    data_tb <- data_tb %>%
      dplyr::rename_with(~Hmisc::label(data_tb)[names(Hmisc::label(data_tb))==.x])
  }
  if(output_type_1L_chr == "PDF"){
    data_x_tb <- data_tb %>%
      xtable::xtable(caption = caption_1L_chr, label = mkdn_tbl_ref_1L_chr, digits = digits_dbl)
    data_x_tb %>%
      print(comment = F,
            floating = TRUE,
            hline.after = hline_after_ls,
            caption.placement = "top",
            add.to.row = add_to_row_ls,
            sanitize.text.function = sanitize_fn,
            format.args = list(big.mark = big_mark_1L_chr),
            include.colnames = inc_col_nms_1L_lgl,
            include.rownames = inc_row_nms_1L_lgl)
  }else{
    if(output_type_1L_chr == "HTML"){
      data_kb <- data_tb %>%
        kableExtra::kbl(format = "html",escape = F,
                        caption = caption_1L_chr, align = c("l",rep("r",max(nrow(data_tb)-1,0)))) %>%
        kableExtra::kable_paper("hover", full_width = F)
      if(!all(is.na(merge_row_idx_int))){
        data_kb <- data_kb %>%
          kableExtra::row_spec(merge_row_idx_int, bold = T)
      }
      data_kb <- data_kb %>%
        kableExtra::footnote(general = ifelse(is.na(footnotes_chr[1]),"",footnotes_chr),
                             general_title = "",
                             footnote_as_chunk = T,
                             threeparttable = T)
      if(!is.null(scroll_box_args_ls)){
        data_kb <- data_kb %>%
          kableExtra::kable_paper()
        data_kb <- rlang::exec(kableExtra::scroll_box, data_kb, !!!scroll_box_args_ls)
      }
      data_xx <- data_kb %>%
        kableExtra::add_header_above(header = heading_grps_chr)
    }
    if(output_type_1L_chr == "Word"){
      j2_1L_dbl <- ncol(data_tb)
      data_fx <- flextable::flextable(data_tb)
      if(!use_rdocx_1L_lgl){
        data_fx <- data_fx %>% #
          flextable::set_caption(caption_1L_chr,
                                 autonum = officer::run_autonum())
      }
      if(!all(is.na(merge_row_idx_int))){
        data_fx <- data_fx %>%
          flextable::merge_h_range(i = merge_row_idx_int,
                                   j1 = 1,
                                   j2 = j2_1L_dbl) %>%
          flextable::bold(i = merge_row_idx_int, j = 1)
      }
      if(!is.null(digits_dbl)){
        numeric_lgl <- unlist(lapply(data_tb, is.numeric)) %>% unname()
        data_fx <- unique(digits_dbl) %>%
          purrr::reduce(.init = data_fx,
                        ~ .x %>%
                          flextable::colformat_num(j = names(data_tb)[which(digits_dbl==.y & numeric_lgl)],
                                                   digits = .y)

          )
      }
      data_fx <- data_fx %>%
        flextable::autofit(add_w = 0, add_h = 0)
      if(!all(is.na(footnotes_chr))){
        data_fx <- data_fx %>%
          flextable::add_footer_lines(values = footnotes_chr)
      }
      data_xx <- data_fx
    }
    return(data_xx)
  }
}
