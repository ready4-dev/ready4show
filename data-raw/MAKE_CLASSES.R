classes_to_make_tb <- ready4class::ready4_constructor_tbl() %>%
  dplyr::bind_rows(tibble::tribble(
    ~ make_s3_lgl, ~ name_stub_chr, ~ pt_ls, ~ pt_chkr_pfx_ls, ~ pt_ns_ls, ~ vals_ls, ~ allowed_vals_ls, ~ min_max_vals_ls, ~ start_end_vals_ls, ~ class_desc_chr, ~ parent_class_chr, ~ slots_ls, ~ meaningful_nms_ls, ~ inc_clss_ls, ~ asserts_ls,
    TRUE, "authors_lup", list("tibble"), list("is_"),list("tibble"),list(first_nm_chr = "character(0)",
                                                                         middle_nm_chr = "character(0)",
                                                                         last_nm_chr = "character(0)",
                                                                         title_chr = "character(0)",
                                                                         qualifications_chr = "character(0)",
                                                                         institute_chr = "character(0)",
                                                                         sequence_int = "integer(0)",
                                                                         is_corresponding_lgl = "logical(0)",
                                                                         email_chr = "character(0)",
                                                                         is_equal_first_lgl = "logical(0)"), NULL,NULL, NULL, "ready4show S3 class for authors lookup table", NA_character_, NULL, NULL, NULL, NULL,
    TRUE, "institutes_lup", list("tibble"), list("is_"),list("tibble"),list(short_name_chr = "character(0)",
                                                                            long_name_chr = "character(0)"), NULL,NULL, NULL, "ready4show S3 class for institutes lookup table", NA_character_, NULL, NULL, NULL, NULL)
  )
name_pfx_1L_chr <- "ready4_"
