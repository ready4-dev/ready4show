library(magrittr)
ready4fun::read_fns("data-raw/fns")
#source("data-raw/MAKE_CLASSES.R")
abbreviations_lup <- ready4fun::get_rds_from_dv("abbreviations_lup")
fn_type_lup_tb <- ready4fun::get_rds_from_dv("fn_type_lup_tb")
# Edits go here
# Push updates to dataverse
abbreviations_lup %>%
  ready4use::write_paired_ds_fls_to_dv(fl_nm_1L_chr = "abbreviations_lup",
                            desc_1L_chr = "Abbreviations lookup table")
fn_type_lup_tb %>%
  ready4use::write_paired_ds_fls_to_dv(fl_nm_1L_chr = "fn_type_lup_tb",
                            desc_1L_chr = "Function type lookup table")
# Previous edits
# abbreviations_lup <- abbreviations_lup %>%
#   dplyr::filter(!short_name_chr %in% c(paste0(name_pfx_1L_chr,classes_to_make_tb$name_stub_chr))) %>%
#   dplyr::filter(!short_name_chr %in% c(paste0(paste0(name_pfx_1L_chr,classes_to_make_tb$name_stub_chr),"s"))) %>%
#   ready4fun::update_abbr_lup(short_name_chr = c(paste0(name_pfx_1L_chr,classes_to_make_tb$name_stub_chr)),
#                              long_name_chr = c(classes_to_make_tb$class_desc_chr),
#                              no_plural_chr = c(classes_to_make_tb$class_desc_chr),
#                              custom_plural_ls = NULL, pfx_rgx = NA_character_)
# abbreviations_lup <- abbreviations_lup %>%
#   ready4fun::update_abbr_lup(short_name_chr = c("hline","lbl","mkdn","rprt"),
#                              long_name_chr = c("horizonal line","label","markdown","report"),
#                              no_plural_chr = NA_character_,
#                              custom_plural_ls = NULL)
# fn_type_lup_tb <- fn_type_lup_tb %>%
#   ready4fun::add_rows_to_fn_type_lup(fn_type_nm_chr = ready4fun::get_new_fn_types(abbreviations_lup = abbreviations_lup,
#                                                                                   fn_type_lup_tb = fn_type_lup_tb,
#                                                                                   object_type_lup = object_type_lup),
#                                      fn_type_desc_chr = c("Prints output to console"),
#                                      is_generic_lgl = F,
#                                      is_method_lgl = F)

