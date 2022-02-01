#' Make abstract arguments list
#' @description make_abstract_args_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make abstract arguments list. The function returns Abstract arguments (a list).
#' @param background_1L_chr Background (a character vector of length one), Default: 'NA'
#' @param objectives_1L_chr Objectives (a character vector of length one), Default: 'NA'
#' @param methods_1L_chr Methods (a character vector of length one), Default: 'NA'
#' @param results_1L_chr Results (a character vector of length one), Default: 'NA'
#' @param conclusions_1L_chr Conclusions (a character vector of length one), Default: 'NA'
#' @param data_1L_chr Data (a character vector of length one), Default: 'NA'
#' @param fl_nm_1L_chr File name (a character vector of length one), Default: 'abstract.txt'
#' @return Abstract arguments (a list)
#' @rdname make_abstract_args_ls
#' @export 
#' @keywords internal
make_abstract_args_ls <- function (background_1L_chr = NA_character_, objectives_1L_chr = NA_character_, 
    methods_1L_chr = NA_character_, results_1L_chr = NA_character_, 
    conclusions_1L_chr = NA_character_, data_1L_chr = NA_character_, 
    fl_nm_1L_chr = "abstract.txt") 
{
    if (is.na(background_1L_chr)) 
        background_1L_chr <- "Brief background to study goes here."
    if (is.na(objectives_1L_chr)) 
        objectives_1L_chr <- "Brief study objectives goes here."
    if (is.na(methods_1L_chr)) 
        methods_1L_chr <- "Brief description of methods goes here."
    if (is.na(results_1L_chr)) 
        results_1L_chr <- "Brief summary of results goes here."
    if (is.na(conclusions_1L_chr)) 
        conclusions_1L_chr <- "Headline conclusions go here."
    if (is.na(data_1L_chr)) 
        data_1L_chr <- "Statement about availability of study data and materials goes here."
    abstract_args_ls <- list(abstract_ls = list(Background = background_1L_chr, 
        Objectives = objectives_1L_chr, Methods = methods_1L_chr, 
        Results = results_1L_chr, Conclusions = conclusions_1L_chr, 
        Data = data_1L_chr), fl_nm_1L_chr = fl_nm_1L_chr)
    return(abstract_args_ls)
}
#' Make abstract lines
#' @description make_abstract_lines() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make abstract lines. The function returns Abstract lines (a character vector).
#' @param abstract_ls Abstract (a list)
#' @return Abstract lines (a character vector)
#' @rdname make_abstract_lines
#' @export 
#' @importFrom purrr map2 flatten_chr
#' @keywords internal
make_abstract_lines <- function (abstract_ls) 
{
    abstract_lines_chr <- purrr::map2(abstract_ls, names(abstract_ls), 
        ~{
            c(paste0("\\\\textbf{", .y, ": }"), .x, "\\\\newline", 
                "\\\\newline")
        }) %>% purrr::flatten_chr()
    return(abstract_lines_chr)
}
#' Make authorship lines
#' @description make_authorship_lines() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make authorship lines. The function returns Authorship lines (a character vector).
#' @param authors_tb Authors (a tibble)
#' @param inc_quals_1L_lgl Include qualifications (a logical vector of length one), Default: F
#' @return Authorship lines (a character vector)
#' @rdname make_authorship_lines
#' @export 
#' @importFrom purrr map flatten_chr
#' @keywords internal
make_authorship_lines <- function (authors_tb, inc_quals_1L_lgl = F) 
{
    authorship_lines_chr <- 1:nrow(authors_tb) %>% purrr::map(~make_sngl_author_lines(authors_tb, 
        slice_1L_int = .x, inc_quals_1L_lgl = inc_quals_1L_lgl)) %>% 
        purrr::flatten_chr()
    return(authorship_lines_chr)
}
#' Make default paths
#' @description make_default_paths() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make default paths. The function is called for its side effects and does not return a value.

#' @return X (Metadata about paths to Markdown input and output)
#' @rdname make_default_paths
#' @export 
#' @keywords internal
make_default_paths <- function () 
{
    x_Ready4showPaths <- Ready4showPaths(mkdn_data_dir_1L_chr = "Markdown", 
        mkdn_source_dir_1L_chr = NA_character_, ms_mkdn_dir_1L_chr = "Manuscript", 
        ms_dir_1L_chr = "Manuscript", outp_data_dir_1L_chr = "Output", 
        reports_dir_1L_chr = "Reports")
    return(x_Ready4showPaths)
}
#' Make equation reference
#' @description make_eq_ref() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make equation reference. The function returns Equation reference (a character vector of length one).
#' @param eq_nm_1L_chr Equation name (a character vector of length one)
#' @param output_type_1L_chr Output type (a character vector of length one)
#' @return Equation reference (a character vector of length one)
#' @rdname make_eq_ref
#' @export 
make_eq_ref <- function (eq_nm_1L_chr, output_type_1L_chr) 
{
    eq_ref_1L_chr <- ifelse(output_type_1L_chr == "Word", paste0("\\@ref(eq:", 
        eq_nm_1L_chr, ")"), paste0("\\ref{eq:", eq_nm_1L_chr, 
        "}"))
    return(eq_ref_1L_chr)
}
#' Make header yaml arguments list
#' @description make_header_yaml_args_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make header yaml arguments list. The function returns Header yaml arguments (a list).
#' @param authors_tb Authors (a tibble)
#' @param institutes_tb Institutes (a tibble)
#' @param title_1L_chr Title (a character vector of length one)
#' @param keywords_chr Keywords (a character vector)
#' @param fl_nm_1L_chr File name (a character vector of length one), Default: 'header_common.yaml'
#' @param use_fake_data_1L_lgl Use fake data (a logical vector of length one), Default: F
#' @return Header yaml arguments (a list)
#' @rdname make_header_yaml_args_ls
#' @export 
#' @keywords internal
make_header_yaml_args_ls <- function (authors_tb, institutes_tb, title_1L_chr, keywords_chr, 
    fl_nm_1L_chr = "header_common.yaml", use_fake_data_1L_lgl = F) 
{
    if (!use_fake_data_1L_lgl) {
        header_yaml_args_ls <- list(authors_tb = authors_tb, 
            institutes_tb = institutes_tb, fl_nm_1L_chr = "header_common.yaml", 
            title_1L_chr = title_1L_chr, keywords_chr = keywords_chr)
    }
    else {
        data("authors_tb", package = "ready4show", envir = environment())
        data("institutes_tb", package = "ready4show", envir = environment())
        header_yaml_args_ls <- make_header_yaml_args_ls(authors_tb = authors_tb, 
            institutes_tb = institutes_tb, title_1L_chr = "A hypothetical study using fake data for instructional purposes only", 
            keywords_chr = c("this", "is", "a", "replication", 
                "using", "fake", "data", "do", "not", "cite"))
    }
    return(header_yaml_args_ls)
}
#' Make institutes lines
#' @description make_institutes_lines() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make institutes lines. The function returns Institutes lines (a character vector).
#' @param authors_tb Authors (a tibble)
#' @param institutes_tb Institutes (a tibble)
#' @return Institutes lines (a character vector)
#' @rdname make_institutes_lines
#' @export 
#' @importFrom purrr map flatten flatten_chr map_chr
#' @importFrom stringr str_remove_all
#' @importFrom ready4 get_from_lup_obj
#' @keywords internal
make_institutes_lines <- function (authors_tb, institutes_tb) 
{
    institutes_chr <- authors_tb$institute_chr %>% purrr::map(~stringr::str_remove_all(.x, 
        "\"") %>% strsplit(", ")) %>% purrr::flatten() %>% purrr:::flatten_chr() %>% 
        unique()
    institutes_lines_chr <- institutes_chr %>% purrr::map_chr(~{
        paste0("  - ", .x, ": ", ready4::get_from_lup_obj(institutes_tb, 
            match_var_nm_1L_chr = "short_name_chr", match_value_xx = .x, 
            target_var_nm_1L_chr = "long_name_chr", evaluate_1L_lgl = F) %>% 
            stringr::str_remove_all("\""))
    })
    return(institutes_lines_chr)
}
#' Make output format list
#' @description make_output_format_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make output format list. The function returns Output format (a list).
#' @param manuscript_outp_1L_chr Manuscript output (a character vector of length one), Default: 'Word'
#' @param manuscript_digits_1L_int Manuscript digits (an integer vector of length one), Default: 2
#' @param supplementary_outp_1L_chr Supplementary output (a character vector of length one), Default: 'PDF'
#' @param supplementary_digits_1L_int Supplementary digits (an integer vector of length one), Default: 2
#' @return Output format (a list)
#' @rdname make_output_format_ls
#' @export 
#' @keywords internal
make_output_format_ls <- function (manuscript_outp_1L_chr = "Word", manuscript_digits_1L_int = 2L, 
    supplementary_outp_1L_chr = "PDF", supplementary_digits_1L_int = 2L) 
{
    output_format_ls <- list(manuscript_outp_1L_chr = manuscript_outp_1L_chr, 
        manuscript_digits_1L_int = manuscript_digits_1L_int, 
        supplementary_outp_1L_chr = supplementary_outp_1L_chr, 
        supplementary_digits_1L_int = supplementary_digits_1L_int)
    return(output_format_ls)
}
#' Make path parameters list
#' @description make_path_params_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make path parameters list. The function returns Path parameters (a list).
#' @param path_to_data_from_top_level_chr Path to data from top level (a character vector), Default: NULL
#' @param path_from_top_level_1L_chr Path from top level (a character vector of length one), Default: NULL
#' @param path_to_current_1L_chr Path to current (a character vector of length one), Default: NULL
#' @param dv_ds_nm_and_url_chr Dataverse dataset name and url (a character vector), Default: NULL
#' @param write_new_dir_1L_lgl Write new directory (a logical vector of length one), Default: F
#' @param use_fake_data_1L_lgl Use fake data (a logical vector of length one), Default: F
#' @param R_fl_nm_1L_chr R file name (a character vector of length one), Default: 'aaaaaaaaaa.txt'
#' @return Path parameters (a list)
#' @rdname make_path_params_ls
#' @export 
#' @importFrom purrr pluck
#' @keywords internal
make_path_params_ls <- function (path_to_data_from_top_level_chr = NULL, path_from_top_level_1L_chr = NULL, 
    path_to_current_1L_chr = NULL, dv_ds_nm_and_url_chr = NULL, 
    write_new_dir_1L_lgl = F, use_fake_data_1L_lgl = F, R_fl_nm_1L_chr = "aaaaaaaaaa.txt") 
{
    if (is.null(path_to_data_from_top_level_chr)) 
        path_to_data_from_top_level_chr <- ifelse(use_fake_data_1L_lgl, 
            "fake_data.rds", "data.rds")
    if (is.null(path_from_top_level_1L_chr)) {
        path_from_top_level_1L_chr <- normalizePath("../") %>% 
            strsplit("\\\\") %>% purrr::pluck(1) %>% tail(1)
    }
    if (is.null(path_to_current_1L_chr)) {
        path_to_current_1L_chr <- normalizePath(".") %>% strsplit("\\\\") %>% 
            purrr::pluck(1) %>% tail(1)
    }
    path_params_ls <- list(path_from_top_level_1L_chr = path_from_top_level_1L_chr, 
        path_to_data_from_top_level_chr = path_to_data_from_top_level_chr, 
        path_to_current_1L_chr = path_to_current_1L_chr, dv_ds_nm_and_url_chr = dv_ds_nm_and_url_chr)
    if (write_new_dir_1L_lgl) 
        path_params_ls$paths_ls <- write_main_outp_dir(path_params_ls, 
            use_fake_data_1L_lgl = use_fake_data_1L_lgl, R_fl_nm_1L_chr = R_fl_nm_1L_chr)
    return(path_params_ls)
}
#' Make paths list
#' @description make_paths_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make paths list. The function returns Paths (a list).
#' @param params_ls Parameters (a list)
#' @param depth_1L_int Depth (an integer vector of length one), Default: 1
#' @return Paths (a list)
#' @rdname make_paths_ls
#' @export 
#' @importFrom purrr pluck
#' @keywords internal
make_paths_ls <- function (params_ls, depth_1L_int = 1) 
{
    paths_ls <- list(path_from_top_level_1L_chr = params_ls$path_from_top_level_1L_chr, 
        path_to_data_from_top_level_chr = params_ls$path_to_data_from_top_level_chr)
    if (!params_ls$use_fake_data_1L_lgl) {
        paths_ls$write_to_dir_nm_1L_chr <- "Real"
    }
    else {
        if (is.null(paths_ls$path_from_top_level_1L_chr)) {
            path_elements_chr <- dirname(getwd()) %>% normalizePath() %>% 
                strsplit("\\\\") %>% purrr::pluck(1)
            paths_ls$path_from_top_level_1L_chr <- path_elements_chr[length(path_elements_chr) - 
                depth_1L_int]
            extended_path_elmts_chr <- getwd() %>% normalizePath() %>% 
                strsplit("\\\\") %>% purrr::pluck(1)
            paths_ls$path_to_current_1L_chr <- paste0(extended_path_elmts_chr[(length(extended_path_elmts_chr) - 
                depth_1L_int):length(extended_path_elmts_chr)], 
                collapse = "/")
        }
        paths_ls$write_to_dir_nm_1L_chr <- "Fake"
        paths_ls$path_to_fake_data_1L_chr <- paste0(paths_ls$path_from_top_level_1L_chr, 
            "/", paths_ls$write_to_dir_nm_1L_chr, "/fake_data.rds")
        paths_ls$path_to_data_from_top_level_chr <- c(paths_ls$path_from_top_level_1L_chr, 
            paths_ls$write_to_dir_nm_1L_chr, "fake_data.rds")
    }
    return(paths_ls)
}
#' Make Markdown file names list
#' @description make_rmd_fl_nms_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make markdown file names list. The function returns a R Markdown file names (a list).
#' @param html_fl_nm_1L_chr Html file name (a character vector of length one), Default: 'Main_HTML'
#' @param pdf_fl_nm_1L_chr Pdf file name (a character vector of length one), Default: 'Main_HTML'
#' @param word_fl_nm_1L_chr Word file name (a character vector of length one), Default: 'Main_HTML'
#' @return a R Markdown file names (a list)
#' @rdname make_rmd_fl_nms_ls
#' @export 
#' @keywords internal
make_rmd_fl_nms_ls <- function (html_fl_nm_1L_chr = "Main_HTML", pdf_fl_nm_1L_chr = "Main_HTML", 
    word_fl_nm_1L_chr = "Main_HTML") 
{
    rmd_fl_nms_ls <- list(HTML = html_fl_nm_1L_chr, PDF = pdf_fl_nm_1L_chr, 
        Word = word_fl_nm_1L_chr)
    return(rmd_fl_nms_ls)
}
#' Make report type list
#' @description make_rprt_type_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make report type list. The function returns Report type (a list).
#' @param rprt_nm_1L_chr Report name (a character vector of length one)
#' @param rprt_lup Report (a lookup table)
#' @return Report type (a list)
#' @rdname make_rprt_type_ls
#' @export 
#' @importFrom purrr map_chr
#' @importFrom ready4 get_from_lup_obj
make_rprt_type_ls <- function (rprt_nm_1L_chr, rprt_lup) 
{
    values_chr <- names(rprt_lup)[names(rprt_lup) != "rprt_nms_chr"] %>% 
        purrr::map_chr(~ready4::get_from_lup_obj(rprt_lup, match_value_xx = rprt_nm_1L_chr, 
            match_var_nm_1L_chr = "rprt_nms_chr", target_var_nm_1L_chr = .x, 
            evaluate_1L_lgl = F))
    rprt_type_ls <- list(path_to_RMD_dir_1L_chr = ifelse(!is.na(values_chr[2]), 
        values_chr[2], system.file(values_chr[3], package = values_chr[4])), 
        nm_of_RMD_1L_chr = values_chr[5], rltv_path_to_outp_yaml_1L_chr = values_chr[6], 
        file_nm_1L_chr = rprt_nm_1L_chr, title_1L_chr = values_chr[1])
    return(rprt_type_ls)
}
#' Make single author lines
#' @description make_sngl_author_lines() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make single author lines. The function returns Author lines (a character vector).
#' @param authors_tb Authors (a tibble)
#' @param slice_1L_int Slice (an integer vector of length one)
#' @param inc_quals_1L_lgl Include qualifications (a logical vector of length one), Default: F
#' @return Author lines (a character vector)
#' @rdname make_sngl_author_lines
#' @export 
#' @importFrom dplyr slice
#' @importFrom stringr str_sub str_replace_all
#' @importFrom purrr discard
#' @keywords internal
make_sngl_author_lines <- function (authors_tb, slice_1L_int, inc_quals_1L_lgl = F) 
{
    sngl_author_tb <- authors_tb %>% dplyr::slice(slice_1L_int)
    indent_1L_chr <- ifelse(slice_1L_int == 1, "      ", "    ")
    author_1L_chr <- paste0(ifelse(slice_1L_int == 1, "  - ", 
        "  - name: "), sngl_author_tb$first_nm_chr, " ", ifelse(is.na(sngl_author_tb$middle_nm_chr), 
        "", paste0(stringr::str_sub(sngl_author_tb$middle_nm_chr, 
            end = 1), " ")), sngl_author_tb$last_nm_chr, ifelse(inc_quals_1L_lgl, 
        ifelse(is.na(sngl_author_tb$qualifications_chr), "", 
            paste0(" (", sngl_author_tb$qualifications_chr %>% 
                stringr::str_replace_all("\"", ""), ")")), ""), 
        ifelse(slice_1L_int == 1, ":", ""))
    email_1L_chr <- ifelse(is.na(sngl_author_tb$email_chr), NA_character_, 
        ifelse(sngl_author_tb$email_chr == "", NA_character_, 
            paste0(indent_1L_chr, "email: ", sngl_author_tb$email_chr)))
    institute_1L_chr <- paste0(indent_1L_chr, "institute: [", 
        sngl_author_tb$institute_chr %>% stringr::str_replace_all("\"", 
            ""), "]")
    correspondence_1L_chr <- ifelse(is.na(sngl_author_tb$is_corresponding_lgl), 
        NA_character_, ifelse(sngl_author_tb$is_corresponding_lgl, 
            paste0(indent_1L_chr, "correspondence: true"), NA_character_))
    equal_1L_chr <- ifelse(is.na(sngl_author_tb$is_equal_first_lgl), 
        NA_character_, ifelse(sngl_author_tb$is_equal_first_lgl, 
            paste0(indent_1L_chr, "equal_contributor: \"yes\""), 
            NA_character_))
    author_lines_chr <- c(author_1L_chr, email_1L_chr, institute_1L_chr, 
        correspondence_1L_chr, equal_1L_chr) %>% purrr::discard(is.na)
    return(author_lines_chr)
}
