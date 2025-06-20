library(ready4)
library(ready4fun)
x <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Author Health Economic Analysis Programs and Reporting Templates" %>% tools::toTitleCase(),
                                 pkg_desc_1L_chr = "ready4show provides tools for authoring repeatable analyses and to help automate the generation of scientific summaries for modelling studies implemented with the ready4 framework.
                                 For detailed documentation about the framework and how to use it visit <https://www.ready4-dev.com/>. For a background to the methodological issues that the framework is attempting to help solve, see Hamilton et al. (2024) <doi:10.1007/s40273-024-01378-8>.
                                 This development version of the ready4show package has been made available as part of the process of testing and documenting the package. If you have any questions, please contact the authors (matthew.hamilton1@monash.edu).",
                                 authors_prsn = c(utils::person(
                                   given = "Matthew",
                                   family = "Hamilton",
                                   email = "matthew.hamilton1@monash.edu",
                                   role = c("aut", "cre", "cph"),
                                   comment = c(ORCID = "0000-0001-7407-9194")
                                 ),
                                 utils::person("Glen", "Wiesner", #email = "Glen.Wiesner@vu.edu.au",
                                               role = c("aut"), comment = c(ORCID = "0000-0002-0071-130X")),
                                 utils::person("Orygen", role = c("cph", "fnd")),
                                 utils::person("Australian Government Research Training Program", role =c("fnd")),
                                 utils::person("VicHealth",role = c("fnd")),
                                 utils::person("Victoria University", role =c("fnd"))
                                 ),
                                 urls_chr = c("https://ready4-dev.github.io/ready4show/",
                                              "https://github.com/ready4-dev/ready4show",
                                              "https://www.ready4-dev.com/")) %>%
  ready4fun::make_manifest(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(#depends_chr = "ready4",
                                                                       imports_chr = "knitrBootstrap",
                                                                       suggests_chr = c("bookdown", "officedown","rmarkdown", "rticles")),
                           build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R")),
                           check_type_1L_chr = "ready4",
                           copyright_holders_chr = "Matthew Hamilton and Orygen",
                           custom_dmt_ls = ready4fun::make_custom_dmt_ls(user_manual_fns_chr = c("make_eq_ref", "make_rprt_type_ls", "make_table_fns_ls",
                                                                                                 "print_in_format","print_table",
                                                                                                 "write_mdl_plt_fl","write_mkdn_from_pkg","write_rprt")),
                           #dev_pkgs_chr = c("ready4"),
                           lifecycle_stage_1L_chr = "experimental",
                           path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4show-logo/default.png",
                           piggyback_to_1L_chr = "ready4-dev/ready4",
                           ready4_type_1L_chr = "authoring",
                           zenodo_badge_1L_chr = "[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5644568.svg)](https://doi.org/10.5281/zenodo.5644568)")
y <- ready4class::ready4class_constructor() %>%
  dplyr::bind_rows(ready4class::make_pt_ready4class_constructor(make_s3_lgl = TRUE,
                                                                name_stub_chr = "authors",
                                                                pt_ls = list(list("tibble")),
                                                                pt_chkr_pfx_ls = list(list("is_")),
                                                                pt_ns_ls = list(list("tibble")),
                                                                vals_ls = list(list(first_nm_chr = "character(0)",
                                                                                    middle_nm_chr = "character(0)",
                                                                                    last_nm_chr = "character(0)",
                                                                                    title_chr = "character(0)",
                                                                                    qualifications_chr = "character(0)",
                                                                                    institute_chr = "character(0)",
                                                                                    sequence_int = "integer(0)",
                                                                                    is_corresponding_lgl = "logical(0)",
                                                                                    email_chr = "character(0)",
                                                                                    is_equal_first_lgl = "logical(0)")),
                                                                class_desc_chr = "Authors lookup table"),
                   ready4class::make_pt_ready4class_constructor(make_s3_lgl = TRUE,
                                                                name_stub_chr = "institutes",
                                                                pt_ls = list(list("tibble")),
                                                                pt_chkr_pfx_ls = list(list("is_")),
                                                                pt_ns_ls = list(list("tibble")),
                                                                vals_ls = list(list(short_name_chr = "character(0)",
                                                                                    long_name_chr = "character(0)")),
                                                                class_desc_chr = "Institutes lookup table"),
                   ready4class::make_pt_ready4class_constructor(make_s3_lgl = TRUE,
                                                                name_stub_chr = "correspondences",
                                                                pt_ls = list(list("tibble")),
                                                                pt_chkr_pfx_ls = list(list("is_")),
                                                                pt_ns_ls = list(list("tibble")),
                                                                vals_ls = list(list(old_nms_chr = "character(0)",
                                                                                    new_nms_chr = "character(0)")),
                                                                class_desc_chr = "Name correspondences lookup table"),
                   ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                                name_stub_chr = "Paths",
                                                                slots_ls = list("mkdn_data_dir_1L_chr",
                                                                                "mkdn_source_dir_1L_chr",
                                                                                "ms_mkdn_dir_1L_chr",
                                                                                "ms_dir_1L_chr",
                                                                                "outp_data_dir_1L_chr",
                                                                                "reports_dir_1L_chr") %>% list(),
                                                                pt_ls = list("character",
                                                                             "character",
                                                                             "character",
                                                                             "character",
                                                                             "character",
                                                                             "character") %>% list(),
                                                                # vals_ls = list(list(mkdn_data_dir_1L_chr = "'Markdown'",
                                                                #                     ms_mkdn_dir_1L_chr = "'Manuscript'",
                                                                #                     ms_dir_1L_chr = "'Manuscript'",
                                                                #                     outp_data_dir_1L_chr = "'Output'",
                                                                #                     reports_dir_1L_chr = "'Reports'")),
                                                                class_desc_chr= "Metadata about paths to Markdown input and output",
                                                                parent_class_chr = "Ready4Module"),
                   ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                                name_stub_chr = "Synopsis",
                                                                slots_ls = list("a_Ready4showPaths",
                                                                                "abstract_args_ls",
                                                                                "authors_r3",
                                                                                "background_1L_chr",
                                                                                "coi_1L_chr",
                                                                                "conclusion_1L_chr",
                                                                                "correspondences_r3",
                                                                                "digits_int",
                                                                                "ethics_1L_chr",
                                                                                "fl_nm_1L_chr",
                                                                                "figures_in_body_lgl",
                                                                                "funding_1L_chr",
                                                                                "institutes_r3",
                                                                                "interval_chr",
                                                                                "keywords_chr",
                                                                                "outp_formats_chr",
                                                                                "rmd_fl_nms_ls",
                                                                                "sample_desc_1L_chr",
                                                                                "tables_in_body_lgl",
                                                                                "title_1L_chr") %>% list(),
                                                                pt_ls = list("Ready4showPaths",
                                                                             "list",
                                                                             "ready4show_authors",
                                                                             "character",
                                                                             "character",
                                                                             "character",
                                                                             "ready4show_correspondences",
                                                                             "integer",
                                                                             "character",
                                                                             "character",
                                                                             "logical",
                                                                             "character",
                                                                             "ready4show_institutes",
                                                                             "character",
                                                                             "character",
                                                                             "character",
                                                                             "list",
                                                                             "character",
                                                                             "logical",
                                                                             "character") %>% list(),
                                                                vals_ls = list(list(a_Ready4showPaths = "make_default_paths()",
                                                                                    abstract_args_ls = "make_abstract_args_ls()",
                                                                                    coi_1L_chr = "'None declared.'",
                                                                                    digits_int = "3L",
                                                                                    ethics_1L_chr = "'Details on ethics approvals go here.'",
                                                                                    figures_in_body_lgl = "T",
                                                                                    funding_1L_chr = "'Details on study funders go here.'",
                                                                                    fl_nm_1L_chr = "'Manuscript'",
                                                                                    outp_formats_chr = "'PDF'",
                                                                                    rmd_fl_nms_ls = "make_rmd_fl_nms_ls()",
                                                                                    tables_in_body_lgl = "T",
                                                                                    title_1L_chr = "'Manuscript title goes here.'"
                                                                )),
                                                                class_desc_chr= "Metadata about a scientific summary manuscript",
                                                                parent_class_chr = "Ready4Module")

                   )
datasets_ls <- list(tibble::tibble(first_nm_chr = c("Alejandra","Fionn"),
                                   middle_nm_chr = c("Rocio", "Seamus"),
                                   last_nm_chr = c("Scienceace", "Researchchamp"),
                                   title_chr = c("Dr", "Prof"),
                                   qualifications_chr = c("MD, PhD", "MSc, PhD"),
                                   institute_chr = c("Insitute_A, Institute_B", "Institute_C, Institute_B"),
                                   sequence_int = c(1,2) %>% as.integer(),
                                   is_corresponding_lgl = c(T, F),
                                   email_chr = c("fake_email@fake_institute.com", "fake_email@made_up_org.com"),
                                   is_equal_first_lgl = c(F,F)) %>%
                      ready4fun::make_pkg_ds_ls(db_1L_chr = "authors_tb",
                                                desc_1L_chr = "Example of an authors table with fake author entries",
                                                title_1L_chr = "Example authors table",
                                                url_1L_chr = "https://ready4-dev.com"),
                    tibble::tibble(short_name_chr = c("Insitute_A", "Institute_B", "Institute_C"),
                                   long_name_chr = c("Awesome University, Shanghai","August Institution, London","Highly Ranked Uni, Montreal")) %>%
                      ready4fun::make_pkg_ds_ls(db_1L_chr = "institutes_tb",
                                                desc_1L_chr = "Example of an institutes table with fake institute entries",
                                                title_1L_chr = "Example institutes table",
                                                url_1L_chr = "https://ready4-dev.com"))
z <- ready4pack::make_pt_ready4pack_manifest(x,
                                             constructor_r3 = y,
                                             pkg_ds_ls_ls = datasets_ls,
                                             clss_to_apply_ls = list(ready4show_authors = c("authors_tb"),
                                                                     ready4show_institutes = "institutes_tb")) %>%
  ready4pack::ready4pack_manifest()
z <- author(z)
#write_extra_pkgs_to_actions(path_to_dir_1L_chr = ".github/workflows")
write_to_edit_workflow("pkgdown.yaml", consent_1L_chr = "Y") # In other packages, run for "test-coverage.yaml" as well.
write_to_tidy_pkg(manifest_ls, build_vignettes_1L_lgl = TRUE,
                  clean_license_1L_lgl = TRUE, consent_1L_chr = "Y",
                  examples_chr = character(0), project_1L_chr = "Framework"#, suggest_chr = "pkgload"
                  )
readLines("README.md") %>% # update in ready4fun
  #gsub(pattern = "doi:10.48550/arXiv.([^&]+)", replacement = "https://arxiv.org/abs/\\1") %>%
  gsub(pattern = "doi:([^&]+)", replacement = "https://doi.org/\\1") %>%
  writeLines(con = "README.md")
#usethis::use_package("pkgload", type = "Suggests") # ??
# write_examples(consent_1L_chr = "Y", path_1L_chr = x$initial_ls$path_to_pkg_rt_1L_chr)
# write_examples(consent_1L_chr = "Y", path_1L_chr = x$initial_ls$path_to_pkg_rt_1L_chr, type_1L_chr = "r4")
# readLines("_pkgdown.yml") %>%
#   stringr::str_replace_all("  - text: Model", "  - text: Framework") %>%
#   writeLines(con = "_pkgdown.yml")
# unlink("LICENSE")
# readLines("DESCRIPTION") %>%
#   purrr::map_chr(~.x %>% stringr::str_replace("GPL-3 \\+ file LICENSE","GPL-3")) %>%
#   writeLines("DESCRIPTION")
# devtools::document()
# devtools::build_vignettes()
