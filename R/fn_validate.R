#' Validate arguments include consent
#' @description validate_args_inc_consent() is a Validate function that validates that an object conforms to required criteria. Specifically, this function implements an algorithm to validate arguments include consent. The function returns Arguments (a list).
#' @param args_ls Arguments (a list)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @return Arguments (a list)
#' @rdname validate_args_inc_consent
#' @export 
#' @keywords internal
validate_args_inc_consent <- function (args_ls, consent_1L_chr = "", consent_indcs_int = 1L, 
    options_chr = c("Y", "N")) 
{
    if (!"consent_1L_chr" %in% names(args_ls)) 
        args_ls$consent_1L_chr <- consent_1L_chr
    if (!"consent_indcs_int" %in% names(args_ls)) 
        args_ls$consent_indcs_int <- consent_indcs_int
    if (!"options_chr" %in% names(args_ls)) 
        args_ls$options_chr <- options_chr
    return(args_ls)
}
