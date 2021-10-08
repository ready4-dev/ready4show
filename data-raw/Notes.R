# manifest_r3 <- ready4fun::update_msng_abbrs(manifest_r3,
#                                              are_words_chr = c("email", "rdocx", "yaml"),
#                                              tf_to_singular_chr = c(prj = "prjs",
#                                                                     qual = "quals"))
# manifest_r3 <- ready4fun::write_new_abbrs(manifest_r3,
#                                            long_name_chr = c("graphics","project","qualification","rendered"),
#                                            no_plural_chr = c("graphics", "rendered"))
# manifest_r3$manifest_r3$subsequent_ls$abbreviations_lup <- manifest_r3$manifest_r3$subsequent_ls$abbreviations_lup %>% dplyr::filter(!short_name_chr %in% c("ready4show_authors_lup","ready4show_institutes_lup"))
# prototype_lup <- ready4fun::get_rds_from_dv("prototype_lup")
# prototype_lup <- prototype_lup %>%
#   dplyr::filter(pt_ns_chr != pkg_setup_ls$initial_ls$pkg_desc_ls$Package)
# write_env_objs_to_dv(list(prototype_lup = prototype_lup),
#                      descriptions_chr = "Class prototype lookup table",
#                      ds_url_1L_chr = pkg_setup_ls$subsequent_ls$pkg_dmt_dv_dss_chr[2],
#                      #key_1L_chr = key_1L_chr,
#                      publish_dv_1L_lgl = T,
#                      server_1L_chr = pkg_setup_ls$subsequent_ls$server_1L_chr)
