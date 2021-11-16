#' write header files
#' @description write_header_fls() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write header files. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param path_to_header_dir_1L_chr Path to header directory (a character vector of length one)
#' @param header_yaml_args_ls Header yaml arguments (a list)
#' @param abstract_args_ls Abstract arguments (a list), Default: NULL
#' @return NULL
#' @rdname write_header_fls
#' @export 
#' @importFrom rlang exec
#' @keywords internal
write_header_fls <- function (path_to_header_dir_1L_chr, header_yaml_args_ls, abstract_args_ls = NULL) 
{
    if (!dir.exists(path_to_header_dir_1L_chr)) 
        dir.create(path_to_header_dir_1L_chr)
    rlang::exec(write_header_yaml, path_to_header_dir_1L_chr, 
        !!!header_yaml_args_ls)
    if (!is.null(abstract_args_ls)) 
        abstract_args_ls$abstract_ls %>% make_abstract_lines() %>% 
            writeLines(paste0(path_to_header_dir_1L_chr, "/", 
                abstract_args_ls$fl_nm_1L_chr))
}
#' write header yaml
#' @description write_header_yaml() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write header yaml. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param path_to_header_dir_1L_chr Path to header directory (a character vector of length one)
#' @param fl_nm_1L_chr File name (a character vector of length one), Default: 'header.yaml'
#' @param authors_tb Authors (a tibble)
#' @param institutes_tb Institutes (a tibble)
#' @param title_1L_chr Title (a character vector of length one), Default: 'Example title'
#' @param keywords_chr Keywords (a character vector), Default: c("Example keyword one", "Example keyword two")
#' @param path_to_tmpl_header_1L_chr Path to template header (a character vector of length one), Default: NULL
#' @param inc_quals_1L_lgl Include qualifications (a logical vector of length one), Default: F
#' @return NULL
#' @rdname write_header_yaml
#' @export 
#' @importFrom dplyr arrange
#' @importFrom purrr map flatten_chr discard
#' @importFrom stringr str_detect str_replace str_replace_all
#' @keywords internal
write_header_yaml <- function (path_to_header_dir_1L_chr, fl_nm_1L_chr = "header.yaml", 
    authors_tb, institutes_tb, title_1L_chr = "Example title", 
    keywords_chr = c("Example keyword one", "Example keyword two"), 
    path_to_tmpl_header_1L_chr = NULL, inc_quals_1L_lgl = F) 
{
    if (is.null(path_to_tmpl_header_1L_chr)) {
        tmpl_header_chr <- c("title: TITLE_PLACEHOLDER", "author:", 
            "AUTHOR_PLACEHOLDER", "institute:", "INSTITUTE_PLACEHOLDER", 
            "keywords:  KEYWORDS_PLACEHOLDER")
    }
    else {
        tmpl_header_chr <- readLines(path_to_tmpl_header_1L_chr)
    }
    authors_tb <- authors_tb %>% dplyr::arrange(sequence_int)
    tmpl_header_chr %>% purrr::map(~{
        if (.x == "AUTHOR_PLACEHOLDER") {
            make_authorship_lines(authors_tb, inc_quals_1L_lgl = inc_quals_1L_lgl)
        }
        else {
            if (.x == "INSTITUTE_PLACEHOLDER") {
                make_institutes_lines(authors_tb, institutes_tb = institutes_tb)
            }
            else {
                if (stringr::str_detect(.x, "KEYWORDS_PLACEHOLDER")) {
                  if (is.na(keywords_chr[1])) {
                    NA_character_
                  }
                  else {
                    stringr::str_replace(.x, "KEYWORDS_PLACEHOLDER", 
                      paste0(keywords_chr, collapse = ", "))
                  }
                }
                else {
                  .x %>% stringr::str_replace_all("TITLE_PLACEHOLDER", 
                    title_1L_chr)
                }
            }
        }
    }) %>% purrr::flatten_chr() %>% purrr::discard(is.na) %>% 
        writeLines(paste0(path_to_header_dir_1L_chr, "/", fl_nm_1L_chr))
}
#' write model plot file
#' @description write_mdl_plt_fl() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write model plot file. The function returns Path to plot (a character vector of length one).
#' @param plt_fn Plot (a function), Default: NULL
#' @param fn_args_ls Function arguments (a list), Default: NULL
#' @param path_to_write_to_1L_chr Path to write to (a character vector of length one)
#' @param plt_nm_1L_chr Plot name (a character vector of length one)
#' @param grpx_fn Graphics (a function), Default: grDevices::png
#' @param units_1L_chr Units (a character vector of length one), Default: 'in'
#' @param width_1L_dbl Width (a double vector of length one), Default: 6
#' @param height_1L_dbl Height (a double vector of length one), Default: 6
#' @param rsl_1L_dbl Resolution (a double vector of length one), Default: 300
#' @return Path to plot (a character vector of length one)
#' @rdname write_mdl_plt_fl
#' @export 
#' @importFrom grDevices png dev.off
#' @importFrom rlang exec
write_mdl_plt_fl <- function (plt_fn = NULL, fn_args_ls = NULL, path_to_write_to_1L_chr, 
    plt_nm_1L_chr, grpx_fn = grDevices::png, units_1L_chr = "in", 
    width_1L_dbl = 6, height_1L_dbl = 6, rsl_1L_dbl = 300) 
{
    if (!is.null(plt_fn)) {
        path_to_plot_1L_chr <- paste0(path_to_write_to_1L_chr, 
            "/", plt_nm_1L_chr, ifelse(identical(grpx_fn, grDevices::png), 
                ".png", ".tiff"))
        rlang::exec(grpx_fn, !!!list(path_to_plot_1L_chr, units = units_1L_chr, 
            width = width_1L_dbl, height = height_1L_dbl, res = rsl_1L_dbl))
        plt <- rlang::exec(plt_fn, !!!fn_args_ls)
        print(plt)
        grDevices::dev.off()
    }
    else {
        path_to_plot_1L_chr <- NA_character_
    }
    return(path_to_plot_1L_chr)
}
#' write markdown from package
#' @description write_mkdn_from_pkg() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write markdown from package. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param pkg_nm_1L_chr Package name (a character vector of length one)
#' @param dest_dir_1L_chr Destination directory (a character vector of length one), Default: 'Markdown'
#' @param overwrite_1L_lgl Overwrite (a logical vector of length one), Default: F
#' @return NULL
#' @rdname write_mkdn_from_pkg
#' @export 
#' @importFrom purrr map_lgl walk
write_mkdn_from_pkg <- function (pkg_nm_1L_chr, dest_dir_1L_chr = "Markdown", overwrite_1L_lgl = F) 
{
    all_mkdn_chr <- system.file("Markdown", package = pkg_nm_1L_chr) %>% 
        list.files()
    is_dir_lgl <- all_mkdn_chr %>% purrr::map_lgl(~system.file(paste0("Markdown/", 
        .x), package = pkg_nm_1L_chr) %>% dir.exists())
    all_mkdn_chr[is_dir_lgl] %>% purrr::walk(~{
        if (!dir.exists(paste0(dest_dir_1L_chr, "/", .x))) 
            dir.create(paste0(dest_dir_1L_chr, "/", .x))
    })
    all_mkdn_files_chr <- system.file("Markdown", package = pkg_nm_1L_chr) %>% 
        list.files(recursive = T)
    all_mkdn_files_chr %>% purrr::walk(~{
        if (!file.exists(paste0(dest_dir_1L_chr, "/", .x)) | 
            (file.exists(paste0(dest_dir_1L_chr, "/", .x)) & 
                overwrite_1L_lgl)) 
            file.create(paste0(dest_dir_1L_chr, "/", .x))
    })
}
#' write rendered report
#' @description write_rndrd_rprt() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write rendered report. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param rprt_type_ls Report type (a list)
#' @param params_ls Parameters (a list), Default: list(output_type_1L_chr = "HTML")
#' @param path_to_write_dirs_to_1L_chr Path to write directories to (a character vector of length one), Default: 'NA'
#' @param nm_of_mkdn_dir_1L_chr Name of markdown directory (a character vector of length one), Default: 'Markdown'
#' @param path_to_rprt_dir_1L_chr Path to report directory (a character vector of length one), Default: './'
#' @param header_yaml_args_ls Header yaml arguments (a list), Default: NULL
#' @param abstract_args_ls Abstract arguments (a list), Default: NULL
#' @param overwrite_1L_lgl Overwrite (a logical vector of length one), Default: T
#' @return NULL
#' @rdname write_rndrd_rprt
#' @export 
#' @importFrom purrr pluck
#' @importFrom rmarkdown render
#' @keywords internal
write_rndrd_rprt <- function (rprt_type_ls, params_ls = list(output_type_1L_chr = "HTML"), 
    path_to_write_dirs_to_1L_chr = NA_character_, nm_of_mkdn_dir_1L_chr = "Markdown", 
    path_to_rprt_dir_1L_chr = "./", header_yaml_args_ls = NULL, 
    abstract_args_ls = NULL, overwrite_1L_lgl = T) 
{
    if (!is.na(path_to_write_dirs_to_1L_chr)) {
        file.copy(rprt_type_ls$path_to_RMD_dir_1L_chr, path_to_write_dirs_to_1L_chr, 
            recursive = T, overwrite = overwrite_1L_lgl)
        dir_nm_1L_chr <- rprt_type_ls$path_to_RMD_dir_1L_chr %>% 
            normalizePath() %>% strsplit("\\\\") %>% purrr::pluck(1) %>% 
            tail(1)
        path_to_mkdn_dir_1L_chr <- paste0(path_to_write_dirs_to_1L_chr, 
            "/", nm_of_mkdn_dir_1L_chr)
        if (dir_nm_1L_chr != nm_of_mkdn_dir_1L_chr) 
            file.rename(paste0(path_to_write_dirs_to_1L_chr, 
                "/", dir_nm_1L_chr), path_to_mkdn_dir_1L_chr)
        path_to_wd_1L_chr <- path_to_mkdn_dir_1L_chr
    }
    else {
        path_to_wd_1L_chr <- rprt_type_ls$path_to_RMD_dir_1L_chr
    }
    if (!dir.exists(path_to_rprt_dir_1L_chr)) 
        dir.create(path_to_rprt_dir_1L_chr)
    if (!is.null(header_yaml_args_ls)) {
        write_header_fls(path_to_header_dir_1L_chr = paste0(path_to_mkdn_dir_1L_chr, 
            "/Header"), header_yaml_args_ls, abstract_args_ls = abstract_args_ls)
    }
    path_to_RMD_1L_chr <- paste0(path_to_wd_1L_chr, "/", rprt_type_ls$nm_of_RMD_1L_chr)
    rmarkdown::render(path_to_RMD_1L_chr, output_format = switch(params_ls$output_type_1L_chr, 
        PDF = "bookdown::pdf_book", HTML = "bookdown::html_document2", 
        Word = "officedown::rdocx_document"), output_yaml = paste0(path_to_wd_1L_chr, 
        "/", rprt_type_ls$rltv_path_to_outp_yaml_1L_chr), params = params_ls, 
        envir = new.env(), output_file = paste0(rprt_type_ls$file_nm_1L_chr, 
            ".", ifelse(params_ls$output_type_1L_chr == "Word", 
                "docx", tolower(params_ls$output_type_1L_chr))), 
        output_dir = path_to_rprt_dir_1L_chr)
}
#' write report
#' @description write_rprt() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write report. The function returns Output summary (a list).
#' @param rprt_type_ls Report type (a list)
#' @param outp_smry_ls Output summary (a list)
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'PDF'
#' @param section_type_1L_chr Section type (a character vector of length one), Default: '#'
#' @param path_to_prjs_dir_1L_chr Path to projects directory (a character vector of length one), Default: '../../../../Data/Project'
#' @param prj_dir_dir_1L_chr Project directory directory (a character vector of length one), Default: 'My_Project'
#' @param reports_dir_1L_chr Reports directory (a character vector of length one), Default: 'Reports'
#' @param rltv_path_to_data_dir_1L_chr Relative path to data directory (a character vector of length one), Default: '../Output'
#' @param nm_of_mkdn_dir_1L_chr Name of markdown directory (a character vector of length one), Default: 'Markdown'
#' @param push_copy_to_dv_1L_lgl Push copy to dataverse (a logical vector of length one), Default: T
#' @param append_params_ls Append parameters (a list), Default: NULL
#' @return Output summary (a list)
#' @rdname write_rprt
#' @export 
#' @importFrom tibble tibble
#' @importFrom ready4 write_to_dv_with_wait
write_rprt <- function (rprt_type_ls, outp_smry_ls, output_type_1L_chr = "PDF", 
    section_type_1L_chr = "#", path_to_prjs_dir_1L_chr = "../../../../Data/Project", 
    prj_dir_dir_1L_chr = "My_Project", reports_dir_1L_chr = "Reports", 
    rltv_path_to_data_dir_1L_chr = "../Output", nm_of_mkdn_dir_1L_chr = "Markdown", 
    push_copy_to_dv_1L_lgl = T, append_params_ls = NULL) 
{
    path_to_outp_dir_1L_chr <- paste0(path_to_prjs_dir_1L_chr, 
        "/", prj_dir_dir_1L_chr)
    path_to_rprt_dir_1L_chr <- paste0(path_to_outp_dir_1L_chr, 
        "/", reports_dir_1L_chr)
    if (!dir.exists(path_to_rprt_dir_1L_chr)) 
        dir.create(path_to_rprt_dir_1L_chr)
    path_to_rprt_dir_1L_chr <- normalizePath(path_to_rprt_dir_1L_chr)
    params_ls <- list(outp_smry_ls = outp_smry_ls, output_type_1L_chr = output_type_1L_chr, 
        rltv_path_to_data_dir_1L_chr = rltv_path_to_data_dir_1L_chr, 
        section_type_1L_chr = section_type_1L_chr)
    if (!is.null(append_params_ls)) {
        params_ls <- append(params_ls, append_params_ls)
    }
    write_rndrd_rprt(rprt_type_ls = rprt_type_ls, params_ls = params_ls, 
        path_to_write_dirs_to_1L_chr = normalizePath(path_to_outp_dir_1L_chr), 
        nm_of_mkdn_dir_1L_chr = nm_of_mkdn_dir_1L_chr, path_to_rprt_dir_1L_chr = path_to_rprt_dir_1L_chr)
    if (!is.null(outp_smry_ls$dv_ls) & push_copy_to_dv_1L_lgl) {
        outp_smry_ls$rprt_dss_tb <- tibble::tibble(ds_obj_nm_chr = rprt_type_ls$file_nm_1L_chr, 
            title_chr = rprt_type_ls$title_1L_chr)
        ready4::write_to_dv_with_wait(outp_smry_ls$rprt_dss_tb, 
            dv_nm_1L_chr = outp_smry_ls$dv_ls$dv_nm_1L_chr, ds_url_1L_chr = outp_smry_ls$dv_ls$ds_url_1L_chr, 
            parent_dv_dir_1L_chr = outp_smry_ls$dv_ls$parent_dv_dir_1L_chr, 
            paths_to_dirs_chr = paste0(path_to_outp_dir_1L_chr, 
                "/", reports_dir_1L_chr), inc_fl_types_chr = paste0(".", 
                ifelse(output_type_1L_chr == "Word", "docx", 
                  tolower(output_type_1L_chr))))
    }
    return(outp_smry_ls)
}
#' write report from template
#' @description write_rprt_from_tmpl() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write report from template. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param rprt_type_ls Report type (a list)
#' @param params_ls Parameters (a list), Default: NULL
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'PDF'
#' @param path_to_prjs_dir_1L_chr Path to projects directory (a character vector of length one)
#' @param prj_dir_1L_chr Project directory (a character vector of length one), Default: 'Fake'
#' @param header_yaml_args_ls Header yaml arguments (a list), Default: NULL
#' @param abstract_args_ls Abstract arguments (a list), Default: NULL
#' @param reports_dir_1L_chr Reports directory (a character vector of length one), Default: 'Reports'
#' @param rltv_path_to_data_dir_1L_chr Relative path to data directory (a character vector of length one), Default: '../Output'
#' @param nm_of_mkdn_dir_1L_chr Name of markdown directory (a character vector of length one), Default: 'Markdown'
#' @return NULL
#' @rdname write_rprt_from_tmpl
#' @export 
#' @keywords internal
write_rprt_from_tmpl <- function (rprt_type_ls, params_ls = NULL, output_type_1L_chr = "PDF", 
    path_to_prjs_dir_1L_chr, prj_dir_1L_chr = "Fake", header_yaml_args_ls = NULL, 
    abstract_args_ls = NULL, reports_dir_1L_chr = "Reports", 
    rltv_path_to_data_dir_1L_chr = "../Output", nm_of_mkdn_dir_1L_chr = "Markdown") 
{
    path_to_outp_dir_1L_chr <- paste0(path_to_prjs_dir_1L_chr, 
        "/", prj_dir_1L_chr)
    path_to_rprt_dir_1L_chr <- paste0(path_to_outp_dir_1L_chr, 
        "/", reports_dir_1L_chr)
    if (!dir.exists(path_to_rprt_dir_1L_chr)) 
        dir.create(path_to_rprt_dir_1L_chr)
    path_to_rprt_dir_1L_chr <- normalizePath(path_to_rprt_dir_1L_chr)
    write_rndrd_rprt(rprt_type_ls = rprt_type_ls, params_ls = params_ls, 
        path_to_write_dirs_to_1L_chr = normalizePath(path_to_outp_dir_1L_chr), 
        nm_of_mkdn_dir_1L_chr = nm_of_mkdn_dir_1L_chr, path_to_rprt_dir_1L_chr = path_to_rprt_dir_1L_chr, 
        header_yaml_args_ls = header_yaml_args_ls, abstract_args_ls = abstract_args_ls)
}
