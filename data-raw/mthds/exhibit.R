exhibit.ready4show_authors <- function(x,
                                       caption_1L_chr = NULL,
                                       mkdn_tbl_ref_1L_chr = NULL,
                                       output_type_1L_chr = "HTML",
                                       use_lbls_as_col_nms_1L_lgl = T,
                                       ...){
  var_desc_chr = c("First-name",
                   "Middle-name",
                   "Last-name",
                   "Title",
                   "Qualifications",
                   "Institutes",
                   "Sequence Position",
                   "Corresponding",
                   "Email",
                   "Joint-first")
  x %>%
    print_from_chunk(caption_1L_chr = caption_1L_chr,
                     mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr,
                     output_type_1L_chr = output_type_1L_chr,
                     use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl,
                     var_desc_chr = var_desc_chr,
                     ...)
}
exhibit.ready4show_correspondences <- function(x,
                                               caption_1L_chr = NULL,
                                               mkdn_tbl_ref_1L_chr = NULL,
                                               output_type_1L_chr = "HTML",
                                               use_lbls_as_col_nms_1L_lgl = T,
                                               ...){
  var_desc_chr = c("Old name",
                   "New name")
  x %>%
    print_from_chunk(caption_1L_chr = caption_1L_chr,
                     mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr,
                     output_type_1L_chr = output_type_1L_chr,
                     use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl,
                     var_desc_chr = var_desc_chr,
                     ...)
}
exhibit.ready4show_institutes <- function(x,
                                          caption_1L_chr = NULL,
                                          mkdn_tbl_ref_1L_chr = NULL,
                                          output_type_1L_chr = "HTML",
                                          use_lbls_as_col_nms_1L_lgl = T,
                                          ...){
  var_desc_chr = c("Reference",
                   "Name")
  x %>%
    print_from_chunk(caption_1L_chr = caption_1L_chr,
                     mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr,
                     output_type_1L_chr = output_type_1L_chr,
                     use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl,
                     var_desc_chr = var_desc_chr,
                     ...)
}
