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
