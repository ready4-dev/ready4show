z$x_ready4fun_manifest$problems_ls$missing_abbrs_chr <- "ms"
z$x_ready4fun_manifest <- ready4::renew(z$x_ready4fun_manifest,
                                           long_name_chr = c("manuscript"),
                                           # custom_plural_ls = list(analysis = "analyses"),
                                           # no_plural_chr = c(),
                                           type_1L_chr = "abbreviations")

# manifest_r3 <- ready4fun::update_msng_abbrs(manifest_r3,
#                                              are_words_chr = c("email", "rdocx", "yaml"),
#                                              tf_to_singular_chr = c(prj = "prjs",
#                                                                     qual = "quals"))
# manifest_r3 <- ready4fun::write_new_abbrs(manifest_r3,
#                                            long_name_chr = c("graphics","project","qualification","rendered"),
#                                            no_plural_chr = c("graphics", "rendered"))
# manifest_r3$manifest_r3$subsequent_ls$abbreviations_lup <- manifest_r3$manifest_r3$subsequent_ls$abbreviations_lup %>% dplyr::filter(!short_name_chr %in% c("ready4show_authors_lup","ready4show_institutes_lup"))
# prototype_lup <- ready4::get_rds_from_dv("prototype_lup")
# prototype_lup <- prototype_lup %>%
#   dplyr::filter(pt_ns_chr != pkg_setup_ls$initial_ls$pkg_desc_ls$Package)
# write_env_objs_to_dv(list(prototype_lup = prototype_lup),
#                      descriptions_chr = "Class prototype lookup table",
#                      ds_url_1L_chr = pkg_setup_ls$subsequent_ls$pkg_dmt_dv_dss_chr[2],
#                      #key_1L_chr = key_1L_chr,
#                      publish_dv_1L_lgl = T,
#                      server_1L_chr = pkg_setup_ls$subsequent_ls$server_1L_chr)
##
# usethis::use_package("knitrBootstrap")
# ##
# Next bit needs to be generalised to
# pkg_ds_ls_ls[[1]]$db_df
# pkg_dss_tb <- pkg_ds_ls_ls[[1]]$db_df %>%
#   ready4show_authors() %>%
#   ready4fun::write_and_doc_ds(db_1L_chr = pkg_ds_ls_ls[[1]]$db_1L_chr,
#                               title_1L_chr = pkg_ds_ls_ls[[1]]$title_1L_chr,
#                               desc_1L_chr = pkg_ds_ls_ls[[1]]$desc_1L_chr,
#                               url_1L_chr = pkg_ds_ls_ls[[1]]$url_1L_chr,
#                               abbreviations_lup = manifest_r3$subsequent_ls$abbreviations_lup,
#                               object_type_lup = manifest_r3$subsequent_ls$object_type_lup)
# pkg_dss_tb <- pkg_ds_ls_ls[[2]]$db_df %>%
#   ready4show_institutes() %>%
#   ready4fun::write_and_doc_ds(db_1L_chr = pkg_ds_ls_ls[[2]]$db_1L_chr,
#                               title_1L_chr = pkg_ds_ls_ls[[2]]$title_1L_chr,
#                               desc_1L_chr = pkg_ds_ls_ls[[2]]$desc_1L_chr,
#                               url_1L_chr = pkg_ds_ls_ls[[2]]$url_1L_chr,
#                               abbreviations_lup = manifest_r3$subsequent_ls$abbreviations_lup,
#                               object_type_lup = manifest_r3$subsequent_ls$object_type_lu,
#                               pkg_dss_tb = pkg_dss_tb)
#usethis::use_package("ggfortify")
