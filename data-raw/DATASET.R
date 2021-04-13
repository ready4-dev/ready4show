## Script to make classes and save updated prototype table.
## This script creates the data files embedded with this package.
# 1. Load magrittr package to that the pipe operator ("%>%") can be used in this script.
library(magrittr)
#
# 2. Create "fns", "gnrcs" and "mthds" sub-directories.
ready4fun::write_fn_type_dirs()
#
# 3. MANUAL STEP. Write all your functions to R files in the new "fns" directory.
#
# 4. Set-up package structure
ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Standardised Developer Tools For Sharing Insights From Projects Developed With The Ready4 Suite" %>% tools::toTitleCase(),
                            pkg_desc_1L_chr = "ready4show provides a set of classes and methods for authoring reports and user-interfaces that showcase insights generated by projects developped with the ready4 suite of tools for mental health data synthesis and modelling.
  This development version of the ready4show package has been made available as part of the process of testing and documenting the package. The tools contained in this development release automate a number of tasks which MODIFY THE DIRECTORY STRUCTURE OF YOUR LOCAL MACHINE.
  Therefore you should only trial this software if you feel confident that you understand what it does and have created a sandpit area in which you can safely undertake testing. If you have any questions, please contact the authors (matthew.hamilton@orygen.org.au).",
                            authors_prsn = c(utils::person(
                              given = "Matthew",family = "Hamilton", email =
                                "matthew.hamilton@orygen.org.au",role = c("aut",
                                                                          "cre"),comment = c(ORCID = "0000-0001-7407-9194")
                            ),
                            utils::person("Glen", "Wiesner", email = "Glen.Wiesner@vu.edu.au",
                                          role = c("aut"), comment = c(ORCID = "0000-0002-0071-130X")),
                            #person("Alexandra", "Parker", email =  "Alex.Parker@vu.edu.au", role = c("rev"), comment = c(ORCID ="0000-0002-2398-6306")),
                            #person("Cathrine", "Mihalopoulos",email = "cathy.mihalopoulos@deakin.edu.au", role = c("rev"), comment = c(ORCID = "0000-0002-7127-9462")),
                            #person("Jonathan", "Karnon", email ="Jonathan.Karnon@flinders.edu.au", role = c("rev"), comment =c(ORCID = "0000-0003-3220-2099")),
                            #person("Petra","Plencnerova", email = "Petra.Plencnerova@vu.edu.au", role =c("rev"), comment = c(ORCID = "0000-0001-9698-9084")),
                            utils::person("Orygen", role = c("cph", "fnd")),
                            utils::person("VicHealth",role = c("fnd")),
                            utils::person("Victoria University", role =c("fnd"))
                            ),
                            urls_chr = c("https://ready4-dev.github.io/ready4show/",
                                         "https://github.com/ready4-dev/ready4show",
                                         "https://www.ready4-dev.com/")) %>%
  ready4fun::write_pkg_setup_fls(incr_ver_1L_lgl = F,
                                 delete_r_dir_cnts_1L_lgl = T,
                                 copyright_holders_chr = "Orygen",
                                 check_type_1L_chr = "gh",
                                 path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4show-logo/default.png",
                                 github_repo = "ready4-dev/ready4show",
                                 lifecycle_stage_1L_chr = "experimental",
                                 badges_lup = ready4fun::badges_lup,
                                 addl_badges_ls = list(ready4 = "authoring"))
# PAUSE FOR INTERACTIVE
##
## PART THREE
# 5. Create a lookup table of abbreviations used in this package and save it as a package dataset
object_type_lup <- ready4fun::get_rds_from_dv("object_type_lup")
pkg_dss_tb <- ready4fun::get_rds_from_dv("abbreviations_lup") %>%
  ready4fun::write_abbr_lup(object_type_lup = object_type_lup)
utils::data("abbreviations_lup")
#
# 6. Create function types look-up table and save it as a package dataset
pkg_dss_tb <- ready4fun::get_rds_from_dv("fn_type_lup_tb") %>%
  ready4fun::write_dmtd_fn_type_lup(abbreviations_lup = abbreviations_lup,
                                    object_type_lup = object_type_lup,
                                    pkg_dss_tb = pkg_dss_tb)
utils::data("fn_type_lup_tb")
#
# 7. Create a table of all functions to document
pkg_dss_tb <- ready4fun::make_dmt_for_all_fns(paths_ls = ready4fun::make_fn_nms()[1],
                                              undocumented_fns_dir_chr = ready4fun::make_undmtd_fns_dir_chr()[1],
                                              custom_dmt_ls = list(details_ls = NULL,
                                                                   inc_for_main_user_lgl_ls = list(force_true_chr = c("make_eqn_ref","make_rprt_type_ls",
                                                                                                                      "print_table",
                                                                                                                      "write_mkdn_from_pkg","write_rprt"),
                                                                                                   force_false_chr = NA_character_),
                                                                   args_ls_ls = NULL),
                                              fn_type_lup_tb = fn_type_lup_tb,
                                              abbreviations_lup = abbreviations_lup,
                                              object_type_lup = object_type_lup) %>%
  ready4fun::write_and_doc_ds(db_1L_chr = "fns_dmt_tb",
                              title_1L_chr = "ready4show function documentation table",
                              desc_1L_chr = "Meta-data on each ready4u function used to create package documentation",
                              url_1L_chr = "https://ready4-dev.github.io/ready4u/",
                              abbreviations_lup = abbreviations_lup,
                              object_type_lup = object_type_lup,
                              pkg_dss_tb = pkg_dss_tb)
utils::data("fns_dmt_tb")
## NEED TO MAKE EXAMPLE REPORTS RMDS AND LUP
## NEED TO ADD RPRTS LUP AND KNIT PARS LS CLASSES AND KNIT METHOD
# pkg_dss_tb <- tibble::tibble(rprt_nms_chr = "Main_Mdl_Smry",
#                              title_chr = "Sample report template.",
#                              paths_to_rmd_dir_1L_chr = NA_character_,
#                              pkg_dirs_chr = "Markdown",
#                              packages_chr = "TTU",
#                              nms_of_rmd_chr = "_Mdls_Report.RMD",
#                              rltv_paths_to_outpt_yaml_chr = "_output.yml") %>%
#   ready4fun::write_and_doc_ds(db_1L_chr = "rprt_lup",
#                               title_1L_chr = "Report types lookup table",
#                               desc_1L_chr = "A lookup table of the different report types supported by ready4show",
#                               abbreviations_lup = abbreviations_lup,
#                               object_type_lup = object_type_lup,
#                               pkg_dss_tb = pkg_dss_tb)
# NOTE: NEED TO UPDATE DIR PATH FOR MODELS
## Note files to be rewritten cannot be open in RStudio.
## 8. Document functions.
usethis::use_build_ignore("initial_setup.R")
readLines(".github/workflows/R-CMD-check.yaml")[-28] %>%
  writeLines(".github/workflows/R-CMD-check.yaml")
usethis::use_package("ggfortify")
usethis::use_package("knitrBootstrap")
#usethis::use_package("knitr", type = "suggests")
#usethis::use_package("rgl")
ready4fun::write_and_doc_fn_fls(fns_dmt_tb,
                                r_dir_1L_chr = "R",
                                dev_pkgs_chr = c("ready4fun","ready4class","ready4use"),
                                update_pkgdown_1L_lgl = T)
##
## PART FOUR
# Uncomment once classes are created
# data("prototype_lup")
# if(!identical(prototype_lup,ready4fun::get_rds_from_dv("prototype_lup"))){
#   prototype_lup %>%
#     write_paired_ds_fls_to_dv(fl_nm_1L_chr = "prototype_lup",
#                               desc_1L_chr = "Prototypes lookup table")
# }
ready4fun::write_links_for_website(user_manual_url_1L_chr = "https://github.com/ready4-dev/ready4show/releases/download/v0.0.0.9014/ready4show_user_0.0.0.9014.pdf",
                                   developer_manual_url_1L_chr = "https://github.com/ready4-dev/ready4show/releases/download/v0.0.0.9014/ready4show_developer_0.0.0.9014.pdf",
                                   project_website_url_1L_chr = "https://www.ready4-dev.com/")
##
## Add, Commit and Push