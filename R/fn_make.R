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
#' @param inc_quals_1L_lgl Include quals (a logical vector of length one), Default: F
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
#' Make eqn reference
#' @description make_eqn_ref() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make eqn reference. The function returns Eqn reference (a character vector of length one).
#' @param eqn_nm_1L_chr Eqn name (a character vector of length one)
#' @param output_type_1L_chr Output type (a character vector of length one)
#' @return Eqn reference (a character vector of length one)
#' @rdname make_eqn_ref
#' @export 

make_eqn_ref <- function (eqn_nm_1L_chr, output_type_1L_chr) 
{
    eqn_ref_1L_chr <- ifelse(output_type_1L_chr == "Word", paste0("\\@ref(eq:", 
        eqn_nm_1L_chr, ")"), paste0("\\ref{eq:", eqn_nm_1L_chr, 
        "}"))
    return(eqn_ref_1L_chr)
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
#' @importFrom ready4fun get_from_lup_obj
#' @keywords internal
make_institutes_lines <- function (authors_tb, institutes_tb) 
{
    institutes_chr <- authors_tb$institute_chr %>% purrr::map(~stringr::str_remove_all(.x, 
        "\"") %>% strsplit(", ")) %>% purrr::flatten() %>% purrr:::flatten_chr() %>% 
        unique()
    institutes_lines_chr <- institutes_chr %>% purrr::map_chr(~{
        paste0("  - ", .x, ": ", ready4fun::get_from_lup_obj(institutes_tb, 
            match_var_nm_1L_chr = "short_name_chr", match_value_xx = .x, 
            target_var_nm_1L_chr = "long_name_chr", evaluate_lgl = F) %>% 
            stringr::str_remove_all("\""))
    })
    return(institutes_lines_chr)
}
#' Make paths
#' @description make_paths_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make paths list. The function returns Paths (a list).
#' @param params_ls Params (a list)
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
#' Make report type
#' @description make_rprt_type_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make report type list. The function returns Report type (a list).
#' @param rprt_nm_1L_chr Report name (a character vector of length one)
#' @param rprt_lup Report (a lookup table)
#' @return Report type (a list)
#' @rdname make_rprt_type_ls
#' @export 
#' @importFrom purrr map_chr
#' @importFrom ready4fun get_from_lup_obj
make_rprt_type_ls <- function (rprt_nm_1L_chr, rprt_lup) 
{
    values_chr <- names(rprt_lup)[names(rprt_lup) != "rprt_nms_chr"] %>% 
        purrr::map_chr(~ready4fun::get_from_lup_obj(rprt_lup, 
            match_value_xx = rprt_nm_1L_chr, match_var_nm_1L_chr = "rprt_nms_chr", 
            target_var_nm_1L_chr = .x, evaluate_lgl = F))
    rprt_type_ls <- list(path_to_RMD_dir_1L_chr = ifelse(!is.na(values_chr[2]), 
        values_chr[2], system.file(values_chr[3], package = values_chr[4])), 
        nm_of_RMD_1L_chr = values_chr[5], rltv_path_to_outpt_yaml_1L_chr = values_chr[6], 
        file_nm_1L_chr = rprt_nm_1L_chr, title_1L_chr = values_chr[1])
    return(rprt_type_ls)
}
#' Make single author lines
#' @description make_sngl_author_lines() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make single author lines. The function returns Author lines (a character vector).
#' @param authors_tb Authors (a tibble)
#' @param slice_1L_int Slice (an integer vector of length one)
#' @param inc_quals_1L_lgl Include quals (a logical vector of length one), Default: F
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
