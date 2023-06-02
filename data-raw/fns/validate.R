validate_args_inc_consent <- function(args_ls,
                                      consent_1L_chr = "",
                                      consent_indcs_int = 1L,
                                      options_chr = c("Y", "N")){
  if(!"consent_1L_chr" %in% names(args_ls))
    args_ls$consent_1L_chr <- consent_1L_chr
  if(!"consent_indcs_int" %in% names(args_ls))
    args_ls$consent_indcs_int <- consent_indcs_int
  if(!"options_chr" %in% names(args_ls))
    args_ls$options_chr <- options_chr
  return(args_ls)
}
